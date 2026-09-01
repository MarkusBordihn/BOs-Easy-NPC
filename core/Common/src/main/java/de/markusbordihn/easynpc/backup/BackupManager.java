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
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.io.BackupDataFiles;
import java.io.File;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.Date;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class BackupManager {

  public static final int BACKUP_BATCH_SIZE_PER_TICK = 10;
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String LOG_PREFIX = "[Backup Manager]";
  private static final ConcurrentHashMap<UUID, Long> lastNPCBackupTime = new ConcurrentHashMap<>();
  private static final Queue<UUID> pendingBackups = new ArrayDeque<>();
  private static final long BACKUP_INTERVAL_MILLISECONDS = 60 * 60 * 1000;
  private static final long BACKUP_CHECK_INTERVAL_TICKS = 20 * 60;

  private static long lastBackupTime = 0;
  private static long backupTicks = 0;
  private static Date backupRunDate;

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
    lastBackupTime = System.currentTimeMillis();
    backupRunDate = new Date();
    forgetOutdatedBackupTimes();
    queueNPCsForBackup();
  }

  public static int getPendingBackupCount() {
    return pendingBackups.size();
  }

  public static Date getBackupRunDate() {
    return backupRunDate;
  }

  public static void reset() {
    lastNPCBackupTime.clear();
    pendingBackups.clear();
    backupRunDate = null;
    lastBackupTime = 0;
    backupTicks = 0;
  }

  private static boolean shouldPerformBackup() {
    return lastBackupTime == 0
        || System.currentTimeMillis() - lastBackupTime > BACKUP_INTERVAL_MILLISECONDS;
  }

  private static void queueNPCsForBackup() {
    LivingEntityManager.getServerEasyNPCEntities()
        .filter(easyNPC -> easyNPC != null && easyNPC.getEntityUUID() != null)
        .map(EasyNPC::getEntityUUID)
        .forEach(pendingBackups::add);
    log.debug("{} Queued {} NPCs for backup.", LOG_PREFIX, pendingBackups.size());
  }

  private static void processPendingBackups() {
    for (int i = 0; i < BACKUP_BATCH_SIZE_PER_TICK && !pendingBackups.isEmpty(); i++) {
      backupNPCData(pendingBackups.poll());
    }

    if (pendingBackups.isEmpty()) {
      NPCEntityManager.saveAllDirtyNPCs();
    }
  }

  private static void forgetOutdatedBackupTimes() {
    long currentTime = System.currentTimeMillis();
    lastNPCBackupTime
        .values()
        .removeIf(backupTime -> currentTime - backupTime > BACKUP_INTERVAL_MILLISECONDS);
  }

  private static void backupNPCData(UUID uuid) {
    EasyNPC<?> easyNPC = LivingEntityManager.getServerEasyNPCEntityByUUID(uuid);
    if (easyNPC == null) {
      return;
    }

    Long lastBackup = lastNPCBackupTime.get(uuid);
    if (lastBackup != null
        && System.currentTimeMillis() - lastBackup < BACKUP_INTERVAL_MILLISECONDS) {
      log.debug("{} [Skipping] Backup for {} already done in the last hour.", LOG_PREFIX, easyNPC);
      return;
    }

    Path backupFilePath = BackupDataFiles.getBackupFile(uuid, backupRunDate);
    if (backupFilePath == null) {
      log.error("{} Backup file path for {} is null.", LOG_PREFIX, easyNPC);
      return;
    }

    File backupFile = backupFilePath.toFile();
    if (PresetHandler.exportBackup(easyNPC, backupFile)) {
      lastNPCBackupTime.put(uuid, System.currentTimeMillis());
    } else {
      log.error("{} Backup failed for {}", LOG_PREFIX, easyNPC);
    }
  }
}
