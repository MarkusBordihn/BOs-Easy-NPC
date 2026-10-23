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

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.config.StorageConfig;
import de.markusbordihn.easynpc.data.npc.NPCBackupSnapshot;
import de.markusbordihn.easynpc.data.npc.NPCEntityMetadata;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.SavedNPCEntityEntry;
import de.markusbordihn.easynpc.data.saveddata.FactionData;
import de.markusbordihn.easynpc.data.saveddata.NPCEntityData;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.data.storage.NPCStorageStatistics;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.entity.Mob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NPCEntityManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String LOG_PREFIX = "[NPC Entity Manager]";

  private static final Map<UUID, NPCRemovalReason> intentionalRemovals = new ConcurrentHashMap<>();
  private static final int MAX_LOGGED_ORPHAN_FILES = 10;

  private static MinecraftServer currentServer;

  private NPCEntityManager() {}

  public static void initialize(MinecraftServer server) {
    if (server == null) {
      log.error("{} Cannot initialize with null server", LOG_PREFIX);
      return;
    }

    currentServer = server;
    intentionalRemovals.clear();
    NPCEntityData.init(server);
    FactionData.init(server);
    removeExpiredEntries();
    log.info(
        "{} Initialized with {} tracked NPC entities", LOG_PREFIX, NPCEntityData.get().getCount());
  }

  public static void ensureInitialized(MinecraftServer server) {
    if (server == currentServer || server.overworld() == null) {
      return;
    }

    initialize(server);
  }

  public static void reset() {
    currentServer = null;
    intentionalRemovals.clear();
    NPCEntityData.reset();
  }

  private static void removeExpiredEntries() {
    NPCEntityData npcEntityData = getNPCEntityData();
    if (StorageConfig.KILLED_NPC_RETENTION_DAYS > 0) {
      int removedKilled =
          npcEntityData.removeExpiredEntries(
              NPCRemovalReason.KILLED,
              StorageConfig.toRetentionMilliseconds(StorageConfig.KILLED_NPC_RETENTION_DAYS));
      if (removedKilled > 0) {
        log.info("{} Removed saved data of {} long dead NPC(s)", LOG_PREFIX, removedKilled);
      }
    }

    if (StorageConfig.DELETED_NPC_RETENTION_DAYS > 0) {
      npcEntityData.removeExpiredDeletedEntries(
          StorageConfig.toRetentionMilliseconds(StorageConfig.DELETED_NPC_RETENTION_DAYS));
    }

    if (StorageConfig.REPORT_ORPHAN_NPC_FILES) {
      List<UUID> orphanFiles = npcEntityData.findOrphanFiles();
      if (!orphanFiles.isEmpty()) {
        log.warn(
            "{} Found {} NPC file(s) without index entry, e.g. {}",
            LOG_PREFIX,
            orphanFiles.size(),
            orphanFiles.subList(0, Math.min(MAX_LOGGED_ORPHAN_FILES, orphanFiles.size())));
      }
    }
  }

  public static <T extends Mob> void saveNPC(EasyNPC<T> easyNPC) {
    saveNPC(easyNPC, NPCRemovalReason.NONE);
  }

  public static <T extends Mob> void saveNPC(EasyNPC<T> easyNPC, NPCRemovalReason removalReason) {
    if (!validateServer() || easyNPC == null) {
      return;
    }

    SavedNPCEntityEntry entry = createSavedEntry(easyNPC, removalReason);
    if (entry == null) {
      return;
    }

    getNPCEntityData().putEntry(entry.entityUUID(), entry);
    log.debug("{} Saved NPC entity: {}", LOG_PREFIX, entry.entityUUID());
  }

  public static <T extends Mob> void saveChangedNPCOnChunkUnload(EasyNPC<T> easyNPC) {
    if (!validateServer() || easyNPC == null) {
      return;
    }

    if (easyNPC.getEasyNPCStatusData().hasUnsavedNPCData() || hasStoredDimensionChanged(easyNPC)) {
      saveNPC(easyNPC, NPCRemovalReason.UNLOADED_TO_CHUNK);
      return;
    }

    NPCEntityData npcEntityData = getNPCEntityData();
    npcEntityData.updateRemovalReason(easyNPC.getEntityUUID(), NPCRemovalReason.UNLOADED_TO_CHUNK);
    npcEntityData.recordSkippedUnchangedUnload();
  }

  private static <T extends Mob> boolean hasStoredDimensionChanged(EasyNPC<T> easyNPC) {
    String currentDimension = easyNPC.getEntity().level().dimension().location().toString();
    return getMetadata(easyNPC.getEntityUUID())
        .map(metadata -> !currentDimension.equals(metadata.dimension()))
        .orElse(true);
  }

  public static <T extends Mob> boolean saveChangedNPCAndQueueFile(EasyNPC<T> easyNPC) {
    if (!isSavableNPC(easyNPC) || !saveChangedNPC(easyNPC)) {
      return false;
    }

    getNPCEntityData().queueNPCFile(easyNPC.getEntityUUID());
    return true;
  }

  public static <T extends Mob> NPCBackupSnapshot saveChangedNPCForBackup(EasyNPC<T> easyNPC) {
    if (!isSavableNPC(easyNPC) || easyNPC.getEasyNPCPresetData() == null) {
      return null;
    }

    StatusDataCapable<T> statusData = easyNPC.getEasyNPCStatusData();
    boolean isMarkedAsChanged = statusData.hasUnsavedNPCData();
    statusData.markNPCDataSaved();
    CompoundTag serializedPresetData = easyNPC.getEasyNPCPresetData().serializePresetData();
    SavedNPCEntityEntry entry =
        SavedNPCEntityEntry.fromPresetData(
            easyNPC, serializedPresetData.copy(), NPCRemovalReason.NONE);
    if (storeChangedEntry(statusData, entry, isMarkedAsChanged)) {
      getNPCEntityData().queueNPCFile(entry.entityUUID());
    }
    return new NPCBackupSnapshot(serializedPresetData, entry.metadata().npcDataHash());
  }

  private static <T extends Mob> boolean isSavableNPC(EasyNPC<T> easyNPC) {
    return validateServer()
        && easyNPC != null
        && easyNPC.isPersistentNPC()
        && !isNPCDeleted(easyNPC.getEntityUUID());
  }

  private static <T extends Mob> boolean saveChangedNPC(EasyNPC<T> easyNPC) {
    StatusDataCapable<T> statusData = easyNPC.getEasyNPCStatusData();
    boolean isMarkedAsChanged = statusData.hasUnsavedNPCData();
    SavedNPCEntityEntry entry = createSavedEntry(easyNPC, NPCRemovalReason.NONE);
    return entry != null && storeChangedEntry(statusData, entry, isMarkedAsChanged);
  }

  private static <T extends Mob> boolean storeChangedEntry(
      StatusDataCapable<T> statusData, SavedNPCEntityEntry entry, boolean isMarkedAsChanged) {
    Optional<NPCEntityMetadata> storedMetadata = getMetadata(entry.entityUUID());
    if (isStoredEntryUpToDate(entry, storedMetadata)) {
      return false;
    }

    if (!isMarkedAsChanged && isOnlyNPCDataHashMissing(entry, storedMetadata)) {
      getNPCEntityData().updateNPCDataHash(entry.entityUUID(), entry.metadata().npcDataHash());
      return false;
    }

    // Lets the backup run, which compares NPC_DATA_LAST_UPDATE, see changes no setter marked.
    if (!isMarkedAsChanged) {
      statusData.markNPCDataUpdated("unmarked change");
      statusData.markNPCDataSaved();
    }
    getNPCEntityData().putEntry(entry.entityUUID(), entry);
    return true;
  }

  private static <T extends Mob> SavedNPCEntityEntry createSavedEntry(
      EasyNPC<T> easyNPC, NPCRemovalReason removalReason) {
    StatusDataCapable<T> statusData = easyNPC.getEasyNPCStatusData();
    long previousSavedTimestamp =
        statusData.getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_SAVED);
    statusData.markNPCDataSaved();
    SavedNPCEntityEntry entry = SavedNPCEntityEntry.fromEasyNPC(easyNPC, removalReason);
    if (entry == null) {
      statusData.setStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_SAVED, previousSavedTimestamp);
    }
    return entry;
  }

  private static boolean isStoredEntryUpToDate(
      SavedNPCEntityEntry entry, Optional<NPCEntityMetadata> storedMetadata) {
    NPCEntityMetadata currentMetadata = entry.metadata().withRemovalReason(NPCRemovalReason.NONE);
    return storedMetadata
        .map(metadata -> metadata.withRemovalReason(NPCRemovalReason.NONE).equals(currentMetadata))
        .orElse(false);
  }

  // Index entries written before 7.17.0 have no hash, adopting it avoids rewriting every NPC file.
  private static boolean isOnlyNPCDataHashMissing(
      SavedNPCEntityEntry entry, Optional<NPCEntityMetadata> storedMetadata) {
    NPCEntityMetadata currentMetadata =
        entry.metadata().withRemovalReason(NPCRemovalReason.NONE).withNPCDataHash(0L);
    return storedMetadata
        .filter(metadata -> metadata.npcDataHash() == 0L)
        .map(metadata -> metadata.withRemovalReason(NPCRemovalReason.NONE).equals(currentMetadata))
        .orElse(false);
  }

  public static boolean hasStoredNPC(UUID entityUUID) {
    return validateServer()
        && entityUUID != null
        && getNPCEntityData().hasCompleteEntry(entityUUID);
  }

  public static void evictFromCache(UUID entityUUID) {
    if (validateServer() && entityUUID != null) {
      getNPCEntityData().evictFromCache(entityUUID);
    }
  }

  public static void releaseNPC(UUID entityUUID) {
    if (validateServer() && entityUUID != null) {
      getNPCEntityData().releaseEntry(entityUUID);
    }
  }

  public static void markNPCDeleted(UUID entityUUID) {
    if (!validateServer() || entityUUID == null) {
      return;
    }

    getNPCEntityData().markDeleted(entityUUID);
    log.debug("{} Deleted NPC entity: {}", LOG_PREFIX, entityUUID);
  }

  public static boolean isNPCDeleted(UUID entityUUID) {
    return validateServer() && getNPCEntityData().isDeleted(entityUUID);
  }

  public static void clearNPCDeleted(UUID entityUUID) {
    if (validateServer()) {
      getNPCEntityData().clearDeleted(entityUUID);
    }
  }

  public static void removeNPC(UUID entityUUID) {
    if (!validateServer() || entityUUID == null) {
      return;
    }

    getNPCEntityData().removeEntry(entityUUID);
    log.debug("{} Removed NPC entity: {}", LOG_PREFIX, entityUUID);
  }

  public static void markIntentionalRemoval(UUID entityUUID, NPCRemovalReason reason) {
    if (entityUUID != null && reason != null && reason != NPCRemovalReason.NONE) {
      intentionalRemovals.put(entityUUID, reason);
    }
  }

  public static NPCRemovalReason takeIntentionalRemovalReason(UUID entityUUID) {
    if (entityUUID == null) {
      return null;
    }

    return intentionalRemovals.remove(entityUUID);
  }

  public static void updateRemovalReason(UUID entityUUID, NPCRemovalReason reason) {
    if (!validateServer() || entityUUID == null) {
      return;
    }

    getNPCEntityData().updateRemovalReason(entityUUID, reason);
  }

  public static <T extends Mob> boolean updateOwner(EasyNPC<T> easyNPC, UUID ownerUUID) {
    if (!validateServer() || easyNPC == null) {
      return false;
    }

    return getNPCEntityData().updateOwner(easyNPC, ownerUUID);
  }

  public static void saveAllDirtyNPCs() {
    if (!validateServer()) {
      return;
    }

    long startTime = System.currentTimeMillis();
    int savedCount =
        LivingEntityManager.getServerEasyNPCEntities()
            .mapToInt(
                easyNPC -> isSavableNPC(easyNPC) && saveChangedNPC(easyNPC) ? 1 : 0)
            .sum();

    if (savedCount > 0) {
      log.info(
          "{} Collected {} changed NPC(s) for persistent storage in {} ms",
          LOG_PREFIX,
          savedCount,
          System.currentTimeMillis() - startTime);
    }
  }

  public static boolean isNPCFileDirty(UUID entityUUID) {
    return validateServer() && getNPCEntityData().isFileDirty(entityUUID);
  }

  public static NPCStorageStatistics getStorageStatistics() {
    if (currentServer == null) {
      return NPCStorageStatistics.EMPTY;
    }

    return getNPCEntityData().getStorageStatistics();
  }

  public static Optional<SavedNPCEntityEntry> getNPC(UUID entityUUID) {
    if (!validateServer() || entityUUID == null) {
      return Optional.empty();
    }

    return getNPCEntityData().getEntry(entityUUID);
  }

  public static Optional<NPCEntityMetadata> getMetadata(UUID entityUUID) {
    if (!validateServer() || entityUUID == null) {
      return Optional.empty();
    }

    return getNPCEntityData().getMetadata(entityUUID);
  }

  public static Collection<SavedNPCEntityEntry> getAllNPCs() {
    if (!validateServer()) {
      return Collections.emptyList();
    }

    return getNPCEntityData().getAllEntries();
  }

  public static int getCount() {
    if (!validateServer()) {
      return 0;
    }

    return getNPCEntityData().getCount();
  }

  public static Collection<SavedNPCEntityEntry> getNPCsByOwner(UUID ownerUUID) {
    if (!validateServer()) {
      return Collections.emptyList();
    }

    return getNPCEntityData().getEntriesByOwner(ownerUUID);
  }

  public static Collection<SavedNPCEntityEntry> getNPCsByType(String type) {
    if (!validateServer()) {
      return Collections.emptyList();
    }

    return getNPCEntityData().getEntriesByType(type);
  }

  public static Collection<SavedNPCEntityEntry> getNPCsByDimension(String dimension) {
    if (!validateServer()) {
      return Collections.emptyList();
    }

    return getNPCEntityData().getEntriesByDimension(dimension);
  }

  public static Collection<SavedNPCEntityEntry> getNPCsByPreset(UUID presetUUID) {
    if (!validateServer()) {
      return Collections.emptyList();
    }

    return getNPCEntityData().getEntriesByPreset(presetUUID);
  }

  public static Collection<SavedNPCEntityEntry> getNPCsByCustomIdentifier(
      ResourceLocation customIdentifier) {
    if (!validateServer()) {
      return Collections.emptyList();
    }

    return getNPCEntityData().getEntriesByCustomIdentifier(customIdentifier);
  }

  public static Collection<SavedNPCEntityEntry> getNPCsByCustomIdentifierNamespace(
      String namespace) {
    if (!validateServer()) {
      return Collections.emptyList();
    }

    return getNPCEntityData().getEntriesByCustomIdentifierNamespace(namespace);
  }

  private static boolean validateServer() {
    if (currentServer == null) {
      log.warn("{} Server not initialized", LOG_PREFIX);
      return false;
    }

    return true;
  }

  private static NPCEntityData getNPCEntityData() {
    return NPCEntityData.get();
  }
}
