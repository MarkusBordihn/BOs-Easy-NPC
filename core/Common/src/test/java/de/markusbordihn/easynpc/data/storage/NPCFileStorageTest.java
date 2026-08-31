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

package de.markusbordihn.easynpc.data.storage;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import java.util.UUID;
import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class NPCFileStorageTest {

  @TempDir Path worldPath;

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static CompoundTag createNPCData(String name) {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putString("Name", name);
    return compoundTag;
  }

  @Test
  void testUnchangedDataIsNotMarkedDirtyAfterSave() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();

    storage.markDirty(uuid, createNPCData("Villager"));
    assertEquals(1, storage.saveAllDirty());
    assertEquals(0, storage.getDirtyCount());

    storage.markDirty(uuid, createNPCData("Villager"));
    assertEquals(0, storage.getDirtyCount());
    assertFalse(storage.isDirty(uuid));
  }

  @Test
  void testChangedDataIsMarkedDirtyAfterSave() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();

    storage.markDirty(uuid, createNPCData("Villager"));
    storage.saveAllDirty();

    storage.markDirty(uuid, createNPCData("Zombie"));
    assertTrue(storage.isDirty(uuid));
    assertEquals(1, storage.saveAllDirty());
  }

  @Test
  void testUnchangedDataIsNotMarkedDirtyAfterLoad() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();
    assertTrue(storage.save(uuid, createNPCData("Villager")));

    NPCFileStorage reloadedStorage = new NPCFileStorage(this.worldPath);
    assertTrue(reloadedStorage.load(uuid).isPresent());

    reloadedStorage.markDirty(uuid, createNPCData("Villager"));
    assertEquals(0, reloadedStorage.getDirtyCount());
  }

  @Test
  void testPendingWriteCountsAsExistingFile() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();
    assertFalse(storage.exists(uuid));

    storage.markDirty(uuid, createNPCData("Villager"));
    assertTrue(storage.exists(uuid));

    storage.saveAllDirty();
    assertTrue(storage.exists(uuid));
  }

  @Test
  void testDeletedNPCDoesNotExist() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();

    storage.markDirty(uuid, createNPCData("Villager"));
    storage.saveAllDirty();
    assertTrue(storage.delete(uuid));
    assertFalse(storage.exists(uuid));
  }

  @Test
  void testDeleteResetsChangeDetection() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();

    storage.markDirty(uuid, createNPCData("Villager"));
    storage.saveAllDirty();
    assertTrue(storage.delete(uuid));

    storage.markDirty(uuid, createNPCData("Villager"));
    assertTrue(storage.isDirty(uuid));
  }
}
