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

package de.markusbordihn.easynpc.io;

import de.markusbordihn.easynpc.Constants;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.UUID;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class BackupDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

  public static void registerBackupData() {
    // Prepare backup data folder
    Path backupFolder = DataFileHandler.getBackupFolder();
    if (backupFolder == null) {
      return;
    }

    // Prepare backup data folder for today
    Path backupDataFolder = getBackupDataFolder();
    if (backupDataFolder == null) {
      log.error("Backup data folder is null, unable to register backup data!");
    }
  }

  public static Path getBackupDataFolder() {
    return DataFileHandler.getOrCreateBackupFolder(DATE_FORMATTER.format(LocalDate.now()));
  }

  public static Path getBackupFile(UUID uuid, Date date) {
    LocalDate localDate = date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    String dateString = DATE_FORMATTER.format(localDate);
    String backupFileName = String.format("%s_%s.backup.npc.nbt", dateString, uuid);
    Path backupDataFolder = DataFileHandler.getOrCreateBackupFolder(dateString);
    return backupDataFolder != null ? backupDataFolder.resolve(backupFileName) : null;
  }
}
