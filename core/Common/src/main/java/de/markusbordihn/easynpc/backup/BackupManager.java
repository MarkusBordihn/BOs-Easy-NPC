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
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.io.BackupDataFiles;
import java.io.File;
import java.nio.file.Path;
import java.util.Date;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class BackupManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String LOG_PREFIX = "[Backup Manager]";

  private static final ConcurrentHashMap<UUID, Long> lastNPCBackupTime = new ConcurrentHashMap<>();
  private static final long BACKUP_INTERVAL = 60 * 60 * 1000; // each 1 hour
  private static final long BACKUP_TICK = 20 * 60; // each 1 minute
  private static long lastBackupTime = 0;
  private static long backupTicks = 0;

  public static void performBackup() {
    if (backupTicks++ < BACKUP_TICK) {
      return;
    }
    if (shouldPerformBackup()) {
      long backupTime = System.currentTimeMillis();
      backupNPCData();
      lastBackupTime = backupTime;
    }
    backupTicks = 0;
  }

  private static boolean shouldPerformBackup() {
    return lastBackupTime == 0 || System.currentTimeMillis() - lastBackupTime > BACKUP_INTERVAL;
  }

  private static void backupNPCData() {
    Date currentDate = new Date();
    LivingEntityManager.getNpcEntityMap()
        .forEach(
            (uuid, easyNPC) -> {
              if (uuid == null || easyNPC == null) {
                return;
              }

              // Check if NPC has already a backup.
              if (lastNPCBackupTime.containsKey(uuid)) {
                long lastBackup = lastNPCBackupTime.get(uuid);
                if (System.currentTimeMillis() - lastBackup < BACKUP_INTERVAL) {
                  log.debug(
                      "{} [Skipping] Backup for {} already done in the last hour.",
                      LOG_PREFIX,
                      easyNPC);
                  return;
                }
              }

              // Get backup file path.
              Path backupFilePath = BackupDataFiles.getBackupFile(uuid, currentDate);
              if (backupFilePath == null) {
                log.warn("{} [Error] Backup file path for {} is null.", LOG_PREFIX, easyNPC);
                return;
              }

              // Create preset files for the NPC.
              File backupFile = backupFilePath.toFile();
              if (backupFile.exists()) {
                log.debug(
                    "{} [Overwrite] Backup file {} for {} ...", LOG_PREFIX, backupFile, easyNPC);
              } else {
                log.debug("{} [Create] Backup file {} for {} ...", LOG_PREFIX, backupFile, easyNPC);
              }
              PresetHandler.exportPreset(easyNPC, backupFile);
            });
  }
}
