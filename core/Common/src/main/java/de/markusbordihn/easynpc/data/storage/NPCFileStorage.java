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
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtAccounter;
import net.minecraft.nbt.NbtIo;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NPCFileStorage {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String NPCS_FOLDER = "npcs";
  private static final String NPC_FILE_EXTENSION = ".npc.nbt";

  private final Path storageFolder;
  private final Map<UUID, CompoundTag> cache = new ConcurrentHashMap<>();
  private final Map<UUID, CompoundTag> dirtyNPCs = new ConcurrentHashMap<>();

  public NPCFileStorage(Path worldPath) {
    this.storageFolder = worldPath.resolve(Constants.MOD_ID).resolve(NPCS_FOLDER);
    ensureStorageFolderExists();
  }

  private void ensureStorageFolderExists() {
    try {
      if (!Files.exists(storageFolder)) {
        Files.createDirectories(storageFolder);
        log.info("Created NPC storage folder at {}", storageFolder);
      }
    } catch (IOException e) {
      log.error("Failed to create NPC storage folder at {}", storageFolder, e);
    }
  }

  public Path getNPCFilePath(UUID uuid) {
    return storageFolder.resolve(uuid.toString() + NPC_FILE_EXTENSION);
  }

  public boolean exists(UUID uuid) {
    return Files.exists(getNPCFilePath(uuid));
  }

  public Optional<CompoundTag> load(UUID uuid) {
    if (uuid == null) {
      return Optional.empty();
    }

    CompoundTag cached = cache.get(uuid);
    if (cached != null) {
      return Optional.of(cached);
    }

    Path npcFile = getNPCFilePath(uuid);
    if (!Files.exists(npcFile)) {
      log.debug("NPC file not found for UUID {}", uuid);
      return Optional.empty();
    }

    try {
      CompoundTag data = NbtIo.readCompressed(npcFile, NbtAccounter.unlimitedHeap());
      cache.put(uuid, data);
      log.debug("Loaded NPC data for UUID {} from file {}", uuid, npcFile);
      return Optional.of(data);
    } catch (IOException e) {
      log.error("Failed to load NPC file {} for UUID {}", npcFile, uuid, e);
      return Optional.empty();
    }
  }

  public void markDirty(UUID uuid, CompoundTag data) {
    if (uuid != null && data != null) {
      dirtyNPCs.put(uuid, data);
      cache.put(uuid, data);
    }
  }

  public boolean save(UUID uuid, CompoundTag data) {
    if (uuid == null || data == null) {
      log.warn("Attempted to save null UUID or data");
      return false;
    }

    return saveToFile(uuid, data);
  }

  private boolean saveToFile(UUID uuid, CompoundTag data) {
    Path npcFile = getNPCFilePath(uuid);
    try {
      Path tempFile = npcFile.getParent().resolve(uuid.toString() + ".tmp");
      NbtIo.writeCompressed(data, tempFile);
      Files.move(tempFile, npcFile, StandardCopyOption.REPLACE_EXISTING);

      cache.put(uuid, data);
      log.debug("Saved NPC data for UUID {} to file {}", uuid, npcFile);
      return true;
    } catch (IOException e) {
      log.error("Failed to save NPC file for UUID {}", uuid, e);
      return false;
    }
  }

  public int saveAllDirty() {
    // Snapshot to avoid ConcurrentModificationException - new entries during save are kept
    Map<UUID, CompoundTag> snapshot = new HashMap<>(dirtyNPCs);
    int savedCount = 0;
    for (Map.Entry<UUID, CompoundTag> entry : snapshot.entrySet()) {
      if (saveToFile(entry.getKey(), entry.getValue())) {
        dirtyNPCs.remove(entry.getKey(), entry.getValue());
        savedCount++;
      }
    }

    if (savedCount > 0) {
      log.debug("Saved {} dirty NPC files out of {}", savedCount, snapshot.size());
    }
    return savedCount;
  }

  public int getDirtyCount() {
    return dirtyNPCs.size();
  }

  public boolean isDirty(UUID uuid) {
    return dirtyNPCs.containsKey(uuid);
  }

  public boolean delete(UUID uuid) {
    if (uuid == null) {
      return false;
    }

    cache.remove(uuid);
    dirtyNPCs.remove(uuid);

    Path npcFile = getNPCFilePath(uuid);
    if (!Files.exists(npcFile)) {
      return true;
    }

    try {
      Files.delete(npcFile);
      log.debug("Deleted NPC file for UUID {}", uuid);
      return true;
    } catch (IOException e) {
      log.error("Failed to delete NPC file for UUID {}", uuid, e);
      return false;
    }
  }

  public void clearCache() {
    cache.clear();
    log.debug("Cleared NPC file storage cache");
  }

  public void evictFromCache(UUID uuid) {
    cache.remove(uuid);
  }

  public int getCacheSize() {
    return cache.size();
  }

  public Stream<UUID> getAllStoredNPCUUIDs() {
    try (Stream<Path> pathStream = Files.list(storageFolder)) {
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
          .toList()
          .stream();
    } catch (IOException e) {
      log.error("Failed to list NPC files in storage folder", e);
      return Stream.empty();
    }
  }
}
