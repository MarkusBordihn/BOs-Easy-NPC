/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.data.saveddata;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.npc.DeletedNPCEntry;
import de.markusbordihn.easynpc.data.npc.NPCEntityMetadata;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.SavedNPCEntityEntry;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.core.RegistryAccess;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import org.junit.jupiter.api.Test;

class NPCEntityDataRetentionTest {

  private static final long ONE_DAY = 24L * 60L * 60L * 1000L;

  private static NPCEntityMetadata metadata(NPCRemovalReason removalReason, long removalTimestamp) {
    return new NPCEntityMetadata(
        null,
        "easy_npc:humanoid",
        "minecraft:overworld",
        null,
        null,
        removalReason,
        false,
        removalTimestamp,
        0L);
  }

  private static UUID putEntry(NPCEntityData entityData, NPCEntityMetadata entityMetadata) {
    UUID entityUUID = UUID.randomUUID();
    entityData.putEntry(entityUUID, new SavedNPCEntityEntry(entityUUID, null, entityMetadata));
    return entityUUID;
  }

  @Test
  void testRemoveExpiredEntriesOnlyRemovesOldEntriesOfReason() {
    long now = System.currentTimeMillis();
    NPCEntityData entityData = new NPCEntityData();
    UUID expiredKilledUUID =
        putEntry(entityData, metadata(NPCRemovalReason.KILLED, now - 2 * ONE_DAY));
    UUID recentKilledUUID = putEntry(entityData, metadata(NPCRemovalReason.KILLED, now));
    UUID oldDespawnedUUID =
        putEntry(entityData, metadata(NPCRemovalReason.DESPAWNED, now - 2 * ONE_DAY));
    UUID killedWithoutTimestampUUID = putEntry(entityData, metadata(NPCRemovalReason.KILLED, 0L));

    assertEquals(1, entityData.removeExpiredEntries(NPCRemovalReason.KILLED, ONE_DAY));
    assertFalse(entityData.hasEntry(expiredKilledUUID));
    assertTrue(entityData.hasEntry(recentKilledUUID));
    assertTrue(entityData.hasEntry(oldDespawnedUUID));
    assertTrue(entityData.hasEntry(killedWithoutTimestampUUID));
  }

  @Test
  void testLegacyMetadataWithoutTimestampStartsRetentionOnLoad() {
    long beforeLoad = System.currentTimeMillis();
    CompoundTag legacyTag = metadata(NPCRemovalReason.KILLED, 0L).toCompoundTag();

    NPCEntityMetadata loadedMetadata = NPCEntityMetadata.fromCompoundTag(legacyTag);

    assertTrue(loadedMetadata.removalTimestamp() >= beforeLoad);
    assertFalse(loadedMetadata.isRemovedLongerThan(ONE_DAY, System.currentTimeMillis()));
  }

  @Test
  void testRemovalTimestampFollowsRemovalReason() {
    NPCEntityMetadata removedMetadata = metadata(NPCRemovalReason.KILLED, 1000L);

    assertEquals(
        1000L, removedMetadata.withRemovalReason(NPCRemovalReason.KILLED).removalTimestamp());
    assertTrue(
        removedMetadata.withRemovalReason(NPCRemovalReason.DESPAWNED).removalTimestamp() > 1000L);
    assertEquals(0L, removedMetadata.withRemovalReason(NPCRemovalReason.NONE).removalTimestamp());

    NPCEntityMetadata reloadedMetadata =
        NPCEntityMetadata.fromCompoundTag(removedMetadata.toCompoundTag());
    assertEquals(1000L, reloadedMetadata.removalTimestamp());
  }

  @Test
  void testUpdateNPCDataHashAdoptsMissingHashWithoutChangingEntry() {
    NPCEntityData entityData = new NPCEntityData();
    NPCEntityMetadata storedMetadata = metadata(NPCRemovalReason.NONE, 0L);
    UUID entityUUID = putEntry(entityData, storedMetadata);
    entityData.setDirty(false);

    entityData.updateNPCDataHash(entityUUID, 42L);

    assertEquals(
        Optional.of(storedMetadata.withNPCDataHash(42L)), entityData.getMetadata(entityUUID));
    assertTrue(entityData.isDirty());

    entityData.setDirty(false);
    entityData.updateNPCDataHash(entityUUID, 42L);
    entityData.updateNPCDataHash(UUID.randomUUID(), 42L);

    assertFalse(entityData.isDirty());
  }

  @Test
  void testMarkDeletedReplacesEntryWithTombstone() {
    NPCEntityData entityData = new NPCEntityData();
    UUID entityUUID = putEntry(entityData, metadata(NPCRemovalReason.NONE, 0L));

    entityData.markDeleted(entityUUID);

    assertFalse(entityData.hasEntry(entityUUID));
    assertTrue(entityData.isDeleted(entityUUID));

    entityData.clearDeleted(entityUUID);
    assertFalse(entityData.isDeleted(entityUUID));
  }

  @Test
  void testDeletedEntriesSurviveSaveAndLoad() {
    NPCEntityData entityData = new NPCEntityData();
    UUID deletedUUID = UUID.randomUUID();
    entityData.markDeleted(deletedUUID);

    NPCEntityData reloadedData =
        NPCEntityData.load(
            entityData.save(new CompoundTag(), RegistryAccess.EMPTY), RegistryAccess.EMPTY);

    assertTrue(reloadedData.isDeleted(deletedUUID));
  }

  @Test
  void testRemoveExpiredDeletedEntries() {
    long now = System.currentTimeMillis();
    UUID expiredUUID = UUID.randomUUID();
    UUID recentUUID = UUID.randomUUID();
    CompoundTag storedTag = new CompoundTag();
    storedTag.put(
        "Deleted",
        DeletedNPCEntry.LIST_CODEC
            .encodeStart(
                NbtOps.INSTANCE,
                List.of(
                    new DeletedNPCEntry(expiredUUID, now - 2 * ONE_DAY),
                    new DeletedNPCEntry(recentUUID, now)))
            .getOrThrow());
    NPCEntityData entityData = NPCEntityData.load(storedTag, RegistryAccess.EMPTY);

    assertEquals(1, entityData.removeExpiredDeletedEntries(ONE_DAY));
    assertFalse(entityData.isDeleted(expiredUUID));
    assertTrue(entityData.isDeleted(recentUUID));
  }
}
