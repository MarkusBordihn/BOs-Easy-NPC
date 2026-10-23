/*
 * Copyright 2024 Markus Bordihn
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

package de.markusbordihn.easynpc.backup;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.config.StorageConfig;
import de.markusbordihn.easynpc.data.npc.NPCBackupSnapshot;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.io.BackgroundFileWriter;
import de.markusbordihn.easynpc.io.BackupDataFiles;
import de.markusbordihn.easynpc.io.PresetFileHandler;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.LocalDate;
import java.util.ArrayDeque;
import java.util.Date;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.nbt.CompoundTag;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class BackupManager {

  public static final int BACKUP_BATCH_SIZE_PER_TICK = 10;
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String LOG_PREFIX = "[Backup Manager]";
  private static final ConcurrentHashMap<UUID, BackupFileEntry> lastNPCBackups =
      new ConcurrentHashMap<>();
  private static final Queue<UUID> pendingBackups = new ArrayDeque<>();
  private static final long BACKUP_INTERVAL_MILLISECONDS = 60 * 60 * 1000;
  private static final long BACKUP_CHECK_INTERVAL_TICKS = 20 * 60;

  private static long lastBackupTime = 0;
  private static long backupTicks = 0;
  private static Date backupRunDate;
  private static LocalDate lastBackupCleanupDate;
  private static long backupRunStartTime;
  private static int exportedBackups;
  private static int linkedBackups;
  private static int skippedUnchangedBackups;

  public static void performBackup() {
    if (!pendingBackups.isEmpty()) {
      processPendingBackups();
      return;
    }

    if (backupTicks++ < BACKUP_CHECK_INTERVAL_TICKS) {
      return;
    }

    backupTicks = 0;

    if (!shouldPerformBackup()) {
      return;
    }

    startBackupRun();
  }

  public static void startBackupRun() {
    startBackupRun(new Date());
  }

  public static void startBackupRun(Date runDate) {
    lastBackupTime = System.currentTimeMillis();
    backupRunDate = runDate;
    backupRunStartTime = lastBackupTime;
    exportedBackups = 0;
    linkedBackups = 0;
    skippedUnchangedBackups = 0;
    removeExpiredBackupFolders();
    queueNPCsForBackup();
  }

  public static int getPendingBackupCount() {
    return pendingBackups.size();
  }

  public static Date getBackupRunDate() {
    return backupRunDate;
  }

  public static int getExportedBackupCount() {
    return exportedBackups;
  }

  public static int getLinkedBackupCount() {
    return linkedBackups;
  }

  public static void reset() {
    lastNPCBackups.clear();
    pendingBackups.clear();
    backupRunDate = null;
    lastBackupCleanupDate = null;
    lastBackupTime = 0;
    backupTicks = 0;
  }

  private static boolean shouldPerformBackup() {
    return lastBackupTime == 0
        || System.currentTimeMillis() - lastBackupTime > BACKUP_INTERVAL_MILLISECONDS;
  }

  private static void queueNPCsForBackup() {
    LivingEntityManager.getServerEasyNPCEntities()
        .filter(
            easyNPC ->
                easyNPC != null && easyNPC.getEntityUUID() != null && easyNPC.isPersistentNPC())
        .map(EasyNPC::getEntityUUID)
        .forEach(pendingBackups::add);
    log.debug("{} Queued {} NPCs for backup.", LOG_PREFIX, pendingBackups.size());
  }

  private static void removeExpiredBackupFolders() {
    LocalDate today = LocalDate.now();
    if (StorageConfig.BACKUP_RETENTION_DAYS <= 0 || today.equals(lastBackupCleanupDate)) {
      return;
    }

    lastBackupCleanupDate = today;
    int removedFolders =
        BackupDataFiles.removeBackupFoldersBefore(
            today.minusDays(StorageConfig.BACKUP_RETENTION_DAYS));
    if (removedFolders > 0) {
      log.info(
          "{} Removed {} backup folder(s) older than {} days.",
          LOG_PREFIX,
          removedFolders,
          StorageConfig.BACKUP_RETENTION_DAYS);
    }
  }

  private static void processPendingBackups() {
    for (int i = 0; i < BACKUP_BATCH_SIZE_PER_TICK && !pendingBackups.isEmpty(); i++) {
      backupNPCData(pendingBackups.poll());
    }

    if (pendingBackups.isEmpty()) {
      log.debug(
          "{} Exported {}, linked {}, skipped {} unchanged NPCs in {} ms.",
          LOG_PREFIX,
          exportedBackups,
          linkedBackups,
          skippedUnchangedBackups,
          System.currentTimeMillis() - backupRunStartTime);
    }
  }

  static boolean isBackupUpToDate(File backupFile, long lastUpdate) {
    return backupFile.lastModified() > lastUpdate;
  }

  private static void backupNPCData(UUID uuid) {
    EasyNPC<?> easyNPC = LivingEntityManager.getServerEasyNPCEntityByUUID(uuid);
    if (easyNPC == null) {
      return;
    }

    Path backupFile = BackupDataFiles.getBackupFile(uuid, backupRunDate);
    if (backupFile == null) {
      log.error("{} Backup file path for {} is null.", LOG_PREFIX, easyNPC);
      return;
    }

    NPCBackupSnapshot snapshot = NPCEntityManager.saveChangedNPCForBackup(easyNPC);
    if (snapshot == null) {
      return;
    }

    BackupFileEntry backupFileEntry = new BackupFileEntry(snapshot.npcDataHash(), backupFile);
    BackupFileEntry lastBackup = lastNPCBackups.get(uuid);
    if (lastBackup != null && lastBackup.npcDataHash() == snapshot.npcDataHash()) {
      if (lastBackup.backupFile().equals(backupFile)) {
        skippedUnchangedBackups++;
        return;
      }

      if (Files.isRegularFile(lastBackup.backupFile())) {
        lastNPCBackups.put(uuid, backupFileEntry);
        BackgroundFileWriter.submit(
            () -> {
              if (!linkOrCopyBackupFile(lastBackup.backupFile(), backupFile)) {
                lastNPCBackups.remove(uuid, backupFileEntry);
              }
            });
        linkedBackups++;
        return;
      }
    }

    if (lastBackup == null
        && isBackupUpToDate(
            backupFile.toFile(),
            easyNPC
                .getEasyNPCStatusData()
                .getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_UPDATE))) {
      lastNPCBackups.put(uuid, backupFileEntry);
      skippedUnchangedBackups++;
      return;
    }

    CompoundTag referenceTag = PresetHandler.getExportReferenceTag(easyNPC);
    lastNPCBackups.put(uuid, backupFileEntry);
    BackgroundFileWriter.submit(
        () -> {
          CompoundTag backupData =
              PresetHandler.prepareBackupData(snapshot.presetData(), referenceTag);
          if (!PresetFileHandler.save(backupFile.toFile(), backupData)) {
            log.error("{} Backup failed for {}", LOG_PREFIX, uuid);
            lastNPCBackups.remove(uuid, backupFileEntry);
          }
        });
    exportedBackups++;
  }

  static boolean linkOrCopyBackupFile(Path sourceFile, Path targetFile) {
    try {
      Files.createLink(targetFile, sourceFile);
      return true;
    } catch (IOException | UnsupportedOperationException linkException) {
      log.debug(
          "{} Hard link to {} failed, copying instead: {}",
          LOG_PREFIX,
          targetFile,
          linkException.toString());
    }

    Path temporaryFile = targetFile.resolveSibling(targetFile.getFileName() + ".tmp");
    try {
      Files.copy(sourceFile, temporaryFile, StandardCopyOption.REPLACE_EXISTING);
      Files.move(temporaryFile, targetFile, StandardCopyOption.REPLACE_EXISTING);
      return true;
    } catch (IOException e) {
      log.error("{} Failed to copy backup {} to {}:", LOG_PREFIX, sourceFile, targetFile, e);
      return false;
    }
  }

  private record BackupFileEntry(long npcDataHash, Path backupFile) {}
}
