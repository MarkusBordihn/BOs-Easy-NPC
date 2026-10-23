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

package de.markusbordihn.easynpc.client.renderer.layer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Optional;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ShoulderNPCCacheTest {

  private static CompoundTag createShoulderNPCTag() {
    CompoundTag entityTag = new CompoundTag();
    entityTag.putString("id", "easy_npc:allay");
    entityTag.putUUID("UUID", UUID.randomUUID());
    return entityTag;
  }

  private static CompoundTag createChangedShoulderNPCTag(CompoundTag entityTag) {
    CompoundTag changedEntityTag = entityTag.copy();
    changedEntityTag.putString("CustomName", "changed");
    return changedEntityTag;
  }

  private static ShoulderNPCCache.CachedShoulderNPC createCachedShoulderNPC(
      CompoundTag entityTag) {
    return new ShoulderNPCCache.CachedShoulderNPC(
        entityTag, entityTag.copy(), Optional.empty(), 0L);
  }

  @Test
  @DisplayName("The same shoulder data instance matches the cached NPC")
  void sameTagInstanceMatches() {
    CompoundTag entityTag = createShoulderNPCTag();

    assertTrue(createCachedShoulderNPC(entityTag).matches(entityTag));
  }

  @Test
  @DisplayName("Equal shoulder data from a new sync matches the cached NPC")
  void equalTagCopyMatches() {
    CompoundTag entityTag = createShoulderNPCTag();

    assertTrue(createCachedShoulderNPC(entityTag).matches(entityTag.copy()));
  }

  @Test
  @DisplayName("Changed shoulder data from a new sync does not match the cached NPC")
  void changedTagDoesNotMatch() {
    CompoundTag entityTag = createShoulderNPCTag();

    assertFalse(
        createCachedShoulderNPC(entityTag).matches(createChangedShoulderNPCTag(entityTag)));
  }

  @Test
  @DisplayName("A reused cache entry keeps its snapshot and remembers the latest source")
  void withLastUseKeepsSnapshot() {
    CompoundTag entityTag = createShoulderNPCTag();
    ShoulderNPCCache.CachedShoulderNPC cachedShoulderNPC = createCachedShoulderNPC(entityTag);
    CompoundTag syncedEntityTag = entityTag.copy();

    ShoulderNPCCache.CachedShoulderNPC reusedShoulderNPC =
        cachedShoulderNPC.withLastUse(syncedEntityTag, 20L);

    assertSame(cachedShoulderNPC.entityTag(), reusedShoulderNPC.entityTag());
    assertSame(syncedEntityTag, reusedShoulderNPC.sourceEntityTag());
    assertEquals(20L, reusedShoulderNPC.lastUsedGameTime());
  }

  @Test
  @DisplayName("Changed data in the latest source instance is compared against the snapshot")
  void reusedEntryDetectsChangedSource() {
    CompoundTag entityTag = createShoulderNPCTag();
    CompoundTag changedEntityTag = createChangedShoulderNPCTag(entityTag);

    assertFalse(
        createCachedShoulderNPC(entityTag)
            .withLastUse(entityTag.copy(), 20L)
            .matches(changedEntityTag));
  }
}
