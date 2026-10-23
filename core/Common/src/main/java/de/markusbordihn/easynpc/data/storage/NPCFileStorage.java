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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.io.BackgroundFileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Stream;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NPCFileStorage {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String NPCS_FOLDER = "npcs";
  private static final String NPC_FILE_EXTENSION = ".npc.nbt";
  private static final long SLOW_FILE_WRITE_MILLISECONDS = 250;
  private static final Duration SAVE_ALL_TIMEOUT = Duration.ofMinutes(2);

  private final Path storageFolder;
  private final Map<UUID, CompoundTag> cache = new ConcurrentHashMap<>();
  private final Map<UUID, CompoundTag> dirtyNPCs = new ConcurrentHashMap<>();
  private final Map<UUID, CompoundTag> lastSavedData = new ConcurrentHashMap<>();
  private final Map<UUID, CompoundTag> queuedNPCs = new ConcurrentHashMap<>();
  private final Set<UUID> pendingRelease = ConcurrentHashMap.newKeySet();
  private final Set<UUID> pendingDeletes = ConcurrentHashMap.newKeySet();
  private final AtomicLong filesWritten = new AtomicLong();
  private final AtomicLong fileWriteMilliseconds = new AtomicLong();
  private final AtomicLong fileWriteFailures = new AtomicLong();
  private final AtomicLong filesRead = new AtomicLong();
  private final AtomicLong serializations = new AtomicLong();
  private final AtomicLong skippedUnchangedUnloads = new AtomicLong();

  public NPCFileStorage(Path worldPath) {
    this.storageFolder = worldPath.resolve(Constants.MOD_ID).resolve(NPCS_FOLDER);
    this.ensureStorageFolderExists();
  }

  private void ensureStorageFolderExists() {
    try {
      if (!Files.exists(this.storageFolder)) {
        Files.createDirectories(this.storageFolder);
        log.info("Created NPC storage folder at {}", this.storageFolder);
      }
    } catch (IOException e) {
      log.error("Failed to create NPC storage folder at {}", this.storageFolder, e);
    }
  }

  public Path getNPCFilePath(UUID uuid) {
    return this.storageFolder.resolve(uuid.toString() + NPC_FILE_EXTENSION);
  }

  public boolean exists(UUID uuid) {
    if (uuid == null) {
      return false;
    }

    if (this.dirtyNPCs.containsKey(uuid)) {
      return true;
    }

    return !this.pendingDeletes.contains(uuid) && Files.exists(this.getNPCFilePath(uuid));
  }

  public Optional<CompoundTag> load(UUID uuid) {
    if (uuid == null) {
      return Optional.empty();
    }

    CompoundTag pending = this.dirtyNPCs.get(uuid);
    if (pending != null) {
      return Optional.of(pending);
    }

    CompoundTag cached = this.cache.get(uuid);
    if (cached != null) {
      return Optional.of(cached);
    }

    if (this.pendingDeletes.contains(uuid)) {
      return Optional.empty();
    }

    Path npcFile = this.getNPCFilePath(uuid);
    if (!Files.exists(npcFile)) {
      log.debug("NPC file not found for UUID {}", uuid);
      return Optional.empty();
    }

    try {
      CompoundTag data = NbtIo.readCompressed(npcFile.toFile());
      this.filesRead.incrementAndGet();
      this.cache.put(uuid, data);
      this.lastSavedData.put(uuid, data);
      log.debug("Loaded NPC data for UUID {} from file {}", uuid, npcFile);
      return Optional.of(data);
    } catch (IOException e) {
      log.error("Failed to load NPC file {} for UUID {}", npcFile, uuid, e);
      return Optional.empty();
    }
  }

  public boolean markDirty(UUID uuid, CompoundTag data) {
    if (uuid == null || data == null) {
      return false;
    }

    this.serializations.incrementAndGet();
    if (!this.queuedNPCs.containsKey(uuid) && data.equals(this.lastSavedData.get(uuid))) {
      this.dirtyNPCs.remove(uuid);
      return false;
    }

    this.dirtyNPCs.put(uuid, data);
    return true;
  }

  public void recordSkippedUnchangedUnload() {
    this.skippedUnchangedUnloads.incrementAndGet();
  }

  public boolean save(UUID uuid, CompoundTag data) {
    if (uuid == null || data == null) {
      log.warn("Attempted to save null UUID or data");
      return false;
    }

    return this.saveToFile(uuid, data);
  }

  private boolean saveToFile(UUID uuid, CompoundTag data) {
    Path npcFile = this.getNPCFilePath(uuid);
    long startTime = System.currentTimeMillis();
    try {
      Path tempFile = npcFile.getParent().resolve(uuid + ".tmp");
      NbtIo.writeCompressed(data, tempFile.toFile());
      Files.move(tempFile, npcFile, StandardCopyOption.REPLACE_EXISTING);
      this.lastSavedData.put(uuid, data);

      long duration = System.currentTimeMillis() - startTime;
      this.filesWritten.incrementAndGet();
      this.fileWriteMilliseconds.addAndGet(duration);
      if (duration >= SLOW_FILE_WRITE_MILLISECONDS) {
        log.warn(
            "Writing NPC file {} took {} ms, disk or scanner is slowing the save down!",
            npcFile,
            duration);
      } else {
        log.debug("Saved NPC data for UUID {} to file {}", uuid, npcFile);
      }
      return true;
    } catch (IOException e) {
      this.fileWriteFailures.incrementAndGet();
      log.error("Failed to save NPC file for UUID {}", uuid, e);
      return false;
    }
  }

  public boolean queue(UUID uuid) {
    if (uuid == null) {
      return false;
    }

    CompoundTag data = this.dirtyNPCs.get(uuid);
    return data != null && this.queue(uuid, data);
  }

  public int queueAllDirty() {
    int queuedCount = 0;
    for (Map.Entry<UUID, CompoundTag> entry : this.dirtyNPCs.entrySet()) {
      if (this.queue(entry.getKey(), entry.getValue())) {
        queuedCount++;
      }
    }
    return queuedCount;
  }

  private boolean queue(UUID uuid, CompoundTag data) {
    if (this.queuedNPCs.get(uuid) == data) {
      return false;
    }

    this.queuedNPCs.put(uuid, data);
    BackgroundFileWriter.submit(() -> this.writeQueued(uuid, data));
    return true;
  }

  private void writeQueued(UUID uuid, CompoundTag data) {
    try {
      if (this.dirtyNPCs.get(uuid) != data) {
        return;
      }

      if (this.saveToFile(uuid, data)
          && this.dirtyNPCs.remove(uuid, data)
          && this.pendingRelease.remove(uuid)) {
        this.releaseCachedData(uuid);
      }
    } finally {
      this.queuedNPCs.remove(uuid, data);
    }
  }

  public int saveAllDirty() {
    Map<UUID, CompoundTag> snapshot = new HashMap<>(this.dirtyNPCs);
    if (snapshot.isEmpty()) {
      return 0;
    }

    long startTime = System.currentTimeMillis();
    this.queueAllDirty();
    if (!BackgroundFileWriter.awaitPendingWrites(SAVE_ALL_TIMEOUT)) {
      log.warn(
          "Timed out after {} waiting for {} pending NPC file writes!",
          SAVE_ALL_TIMEOUT,
          BackgroundFileWriter.getPendingWriteCount());
    }

    int savedCount = 0;
    for (Map.Entry<UUID, CompoundTag> entry : snapshot.entrySet()) {
      if (this.dirtyNPCs.get(entry.getKey()) != entry.getValue()) {
        savedCount++;
      }
    }

    log.info(
        "Saved {} of {} NPC files to {} in {} ms",
        savedCount,
        snapshot.size(),
        this.storageFolder,
        System.currentTimeMillis() - startTime);
    return savedCount;
  }

  public int getDirtyCount() {
    return this.dirtyNPCs.size();
  }

  public boolean isDirty(UUID uuid) {
    return this.dirtyNPCs.containsKey(uuid);
  }

  public void delete(UUID uuid) {
    if (uuid == null) {
      return;
    }

    this.cache.remove(uuid);
    this.dirtyNPCs.remove(uuid);
    this.lastSavedData.remove(uuid);
    this.pendingRelease.remove(uuid);
    this.pendingDeletes.add(uuid);
    BackgroundFileWriter.submit(() -> this.deleteFile(uuid));
  }

  private void deleteFile(UUID uuid) {
    try {
      if (Files.deleteIfExists(this.getNPCFilePath(uuid))) {
        log.debug("Deleted NPC file for UUID {}", uuid);
      }
      this.lastSavedData.remove(uuid);
    } catch (IOException e) {
      log.error("Failed to delete NPC file for UUID {}", uuid, e);
    } finally {
      this.pendingDeletes.remove(uuid);
    }
  }

  public void clearCache() {
    this.cache.clear();
    log.debug("Cleared NPC file storage cache");
  }

  public void evictFromCache(UUID uuid) {
    this.cache.remove(uuid);
  }

  public void release(UUID uuid) {
    if (uuid == null) {
      return;
    }

    if (this.dirtyNPCs.containsKey(uuid)) {
      this.pendingRelease.add(uuid);
      if (this.dirtyNPCs.containsKey(uuid) || !this.pendingRelease.remove(uuid)) {
        return;
      }
    }

    this.releaseCachedData(uuid);
  }

  private void releaseCachedData(UUID uuid) {
    this.cache.remove(uuid);
    this.lastSavedData.remove(uuid);
  }

  public int getCacheSize() {
    return this.cache.size();
  }

  public NPCStorageStatistics getStatistics() {
    return new NPCStorageStatistics(
        this.filesWritten.get(),
        this.fileWriteMilliseconds.get(),
        this.fileWriteFailures.get(),
        this.filesRead.get(),
        this.serializations.get(),
        this.skippedUnchangedUnloads.get(),
        this.dirtyNPCs.size(),
        this.cache.size());
  }

  public Stream<UUID> getAllStoredNPCUUIDs() {
    try (Stream<Path> pathStream = Files.list(this.storageFolder)) {
      return pathStream
          .filter(Files::isRegularFile)
          .filter(path -> path.toString().endsWith(NPC_FILE_EXTENSION))
          .map(
              path -> {
                String fileName = path.getFileName().toString();
                String uuidString =
                    fileName.substring(0, fileName.length() - NPC_FILE_EXTENSION.length());
                try {
                  return UUID.fromString(uuidString);
                } catch (IllegalArgumentException e) {
                  log.warn("Invalid UUID in filename: {}", fileName);
                  return null;
                }
              })
          .filter(Objects::nonNull)
          .filter(uuid -> !this.pendingDeletes.contains(uuid))
          .toList()
          .stream();
    } catch (IOException e) {
      log.error("Failed to list NPC files in storage folder", e);
      return Stream.empty();
    }
  }
}
