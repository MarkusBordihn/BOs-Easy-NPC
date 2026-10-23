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

import de.markusbordihn.easynpc.io.BackgroundFileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class NPCFileStorageTest {

  private static final Duration WRITER_TIMEOUT = Duration.ofSeconds(10);

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
    storage.delete(uuid);
    assertFalse(storage.exists(uuid));
  }

  @Test
  void testDeleteResetsChangeDetection() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();

    storage.markDirty(uuid, createNPCData("Villager"));
    storage.saveAllDirty();
    storage.delete(uuid);

    storage.markDirty(uuid, createNPCData("Villager"));
    assertTrue(storage.isDirty(uuid));
  }

  @Test
  void testMarkDirtyReportsQueuedWrite() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();

    assertTrue(storage.markDirty(uuid, createNPCData("Villager")));
    storage.saveAllDirty();
    assertFalse(storage.markDirty(uuid, createNPCData("Villager")));
    assertTrue(storage.markDirty(uuid, createNPCData("Zombie")));
  }

  @Test
  void testStatisticsCountWritesAndReads() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();

    storage.markDirty(uuid, createNPCData("Villager"));
    storage.markDirty(uuid, createNPCData("Villager"));
    storage.saveAllDirty();
    NPCStorageStatistics statistics = storage.getStatistics();
    assertEquals(2, statistics.serializations());
    assertEquals(1, statistics.filesWritten());
    assertEquals(0, statistics.fileWriteFailures());
    assertEquals(0, statistics.dirtyFiles());

    NPCFileStorage reloadedStorage = new NPCFileStorage(this.worldPath);
    assertTrue(reloadedStorage.load(uuid).isPresent());
    assertTrue(reloadedStorage.load(uuid).isPresent());
    assertEquals(1, reloadedStorage.getStatistics().filesRead());
    assertEquals(1, reloadedStorage.getStatistics().cachedFiles());
  }

  @Test
  void testFailedWriteStaysDirtyAndIsRetried() throws IOException {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();
    Path blockingDirectory = storage.getNPCFilePath(uuid);
    Path blockingFile = Files.createDirectories(blockingDirectory).resolve("blocking");
    Files.createFile(blockingFile);

    storage.markDirty(uuid, createNPCData("Villager"));
    assertEquals(0, storage.saveAllDirty());
    assertEquals(1, storage.getStatistics().fileWriteFailures());
    assertTrue(storage.isDirty(uuid));

    Files.delete(blockingFile);
    Files.delete(blockingDirectory);
    assertEquals(1, storage.saveAllDirty());
    assertFalse(storage.isDirty(uuid));
    assertEquals(1, storage.getStatistics().filesWritten());
  }

  @Test
  void testQueuedWriteKeepsNewestDataReadable() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();
    CountDownLatch writerBlock = blockWriter();
    try {
      storage.markDirty(uuid, createNPCData("Villager"));
      assertTrue(storage.queue(uuid));
      assertFalse(Files.exists(storage.getNPCFilePath(uuid)));
      assertTrue(storage.isDirty(uuid));
      assertEquals("Villager", storage.load(uuid).orElseThrow().getString("Name"));
    } finally {
      writerBlock.countDown();
    }

    assertTrue(BackgroundFileWriter.awaitPendingWrites(WRITER_TIMEOUT));
    assertTrue(Files.exists(storage.getNPCFilePath(uuid)));
    assertFalse(storage.isDirty(uuid));
  }

  @Test
  void testQueuedWriteIsNotQueuedTwice() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();
    CountDownLatch writerBlock = blockWriter();
    try {
      storage.markDirty(uuid, createNPCData("Villager"));
      assertTrue(storage.queue(uuid));
      assertFalse(storage.queue(uuid));
      assertEquals(0, storage.queueAllDirty());
    } finally {
      writerBlock.countDown();
    }

    assertTrue(BackgroundFileWriter.awaitPendingWrites(WRITER_TIMEOUT));
    assertEquals(1, storage.getStatistics().filesWritten());
  }

  @Test
  void testRevertDuringQueuedWriteIsSaved() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();
    storage.markDirty(uuid, createNPCData("Villager"));
    storage.saveAllDirty();

    CountDownLatch writerBlock = blockWriter();
    try {
      storage.markDirty(uuid, createNPCData("Zombie"));
      assertTrue(storage.queue(uuid));
      assertTrue(storage.markDirty(uuid, createNPCData("Villager")));
    } finally {
      writerBlock.countDown();
    }

    storage.saveAllDirty();
    NPCFileStorage reloadedStorage = new NPCFileStorage(this.worldPath);
    assertEquals("Villager", reloadedStorage.load(uuid).orElseThrow().getString("Name"));
  }

  @Test
  void testDeleteAfterQueuedWriteRemovesFile() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();
    storage.markDirty(uuid, createNPCData("Villager"));
    storage.saveAllDirty();

    CountDownLatch writerBlock = blockWriter();
    try {
      storage.markDirty(uuid, createNPCData("Zombie"));
      assertTrue(storage.queue(uuid));
      storage.delete(uuid);
      assertFalse(storage.exists(uuid));
      assertTrue(storage.load(uuid).isEmpty());
      assertFalse(storage.getAllStoredNPCUUIDs().anyMatch(uuid::equals));
    } finally {
      writerBlock.countDown();
    }

    assertTrue(BackgroundFileWriter.awaitPendingWrites(WRITER_TIMEOUT));
    assertFalse(Files.exists(storage.getNPCFilePath(uuid)));
    assertFalse(storage.exists(uuid));
  }

  @Test
  void testWriteAfterDeleteIsKept() {
    NPCFileStorage storage = new NPCFileStorage(this.worldPath);
    UUID uuid = UUID.randomUUID();
    storage.markDirty(uuid, createNPCData("Villager"));
    storage.saveAllDirty();

    CountDownLatch writerBlock = blockWriter();
    try {
      storage.delete(uuid);
      storage.markDirty(uuid, createNPCData("Zombie"));
      assertTrue(storage.queue(uuid));
      assertTrue(storage.exists(uuid));
    } finally {
      writerBlock.countDown();
    }

    assertTrue(BackgroundFileWriter.awaitPendingWrites(WRITER_TIMEOUT));
    NPCFileStorage reloadedStorage = new NPCFileStorage(this.worldPath);
    assertEquals("Zombie", reloadedStorage.load(uuid).orElseThrow().getString("Name"));
  }

  private static CountDownLatch blockWriter() {
    CountDownLatch writerBlock = new CountDownLatch(1);
    BackgroundFileWriter.submit(
        () -> {
          try {
            writerBlock.await(WRITER_TIMEOUT.toMillis(), TimeUnit.MILLISECONDS);
          } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
          }
        });
    return writerBlock;
  }
}
