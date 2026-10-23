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

package de.markusbordihn.easynpc.backup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BackupManagerTest {

  private static final long LAST_UPDATE = 1_000_000L;

  @TempDir Path backupFolder;

  private File createBackupFile(long lastModified) throws IOException {
    File backupFile = Files.createFile(this.backupFolder.resolve("backup.npc.nbt")).toFile();
    assertTrue(backupFile.setLastModified(lastModified));
    return backupFile;
  }

  @Test
  @DisplayName("Backup file written after the last update is up to date")
  void testBackupFileAfterLastUpdate() throws IOException {
    File backupFile = this.createBackupFile(LAST_UPDATE + 1000);
    assertTrue(BackupManager.isBackupUpToDate(backupFile, LAST_UPDATE));
  }

  @Test
  @DisplayName("Backup file written before the last update is outdated")
  void testBackupFileBeforeLastUpdate() throws IOException {
    File backupFile = this.createBackupFile(LAST_UPDATE - 1000);
    assertFalse(BackupManager.isBackupUpToDate(backupFile, LAST_UPDATE));
  }

  @Test
  @DisplayName("Missing backup file is outdated")
  void testMissingBackupFile() {
    File missingFile = this.backupFolder.resolve("missing.npc.nbt").toFile();
    assertFalse(BackupManager.isBackupUpToDate(missingFile, LAST_UPDATE));
  }

  @Test
  @DisplayName("Unchanged backup is linked into the new day folder")
  void testLinkBackupFile() throws IOException {
    Path sourceFile = Files.writeString(this.backupFolder.resolve("source.npc.nbt"), "backup");
    Path targetFile = Files.createDirectories(this.backupFolder.resolve("today")).resolve("target");

    assertTrue(BackupManager.linkOrCopyBackupFile(sourceFile, targetFile));
    assertEquals("backup", Files.readString(targetFile));
  }

  @Test
  @DisplayName("Failed hard link falls back to a copy that replaces the target")
  void testCopyBackupFileWhenLinkFails() throws IOException {
    Path sourceFile = Files.writeString(this.backupFolder.resolve("source.npc.nbt"), "backup");
    Path targetFile = Files.writeString(this.backupFolder.resolve("target.npc.nbt"), "outdated");

    assertTrue(BackupManager.linkOrCopyBackupFile(sourceFile, targetFile));
    assertEquals("backup", Files.readString(targetFile));
    assertFalse(Files.exists(this.backupFolder.resolve("target.npc.nbt.tmp")));
  }

  @Test
  @DisplayName("Missing source backup fails without creating the target")
  void testLinkMissingBackupFile() {
    Path sourceFile = this.backupFolder.resolve("missing.npc.nbt");
    Path targetFile = this.backupFolder.resolve("target.npc.nbt");

    assertFalse(BackupManager.linkOrCopyBackupFile(sourceFile, targetFile));
    assertFalse(Files.exists(targetFile));
  }
}
