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
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class BackupDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

  public static void registerBackupData() {
    Path backupFolder = DataFileHandler.getBackupFolder();
    if (backupFolder == null) {
      return;
    }

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

  public static int removeBackupFoldersBefore(LocalDate oldestKeptDate) {
    return removeBackupFoldersBefore(DataFileHandler.getBackupFolder(), oldestKeptDate);
  }

  public static int removeBackupFoldersBefore(Path backupFolder, LocalDate oldestKeptDate) {
    if (backupFolder == null || oldestKeptDate == null || !Files.isDirectory(backupFolder)) {
      return 0;
    }

    List<Path> backupDateFolders;
    try (Stream<Path> backupFolderEntries = Files.list(backupFolder)) {
      backupDateFolders = backupFolderEntries.filter(Files::isDirectory).toList();
    } catch (IOException e) {
      log.error("Unable to list backup folder {}:", backupFolder, e);
      return 0;
    }

    int removedFolders = 0;
    for (Path backupDateFolder : backupDateFolders) {
      Optional<LocalDate> backupDate = parseBackupDate(backupDateFolder.getFileName().toString());
      if (backupDate.isPresent()
          && backupDate.get().isBefore(oldestKeptDate)
          && deleteBackupFolder(backupDateFolder)) {
        removedFolders++;
      }
    }
    return removedFolders;
  }

  private static Optional<LocalDate> parseBackupDate(String folderName) {
    try {
      return Optional.of(LocalDate.parse(folderName, DATE_FORMATTER));
    } catch (DateTimeParseException e) {
      return Optional.empty();
    }
  }

  private static boolean deleteBackupFolder(Path backupDateFolder) {
    try (Stream<Path> backupPaths = Files.walk(backupDateFolder)) {
      for (Path backupPath : backupPaths.sorted(Comparator.reverseOrder()).toList()) {
        Files.delete(backupPath);
      }
      return true;
    } catch (IOException e) {
      log.error("Unable to delete backup folder {}:", backupDateFolder, e);
      return false;
    }
  }
}
