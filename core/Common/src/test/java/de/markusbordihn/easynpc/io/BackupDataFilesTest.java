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

package de.markusbordihn.easynpc.io;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BackupDataFilesTest {

  @Test
  void testRemovesOnlyDatedFoldersBeforeOldestKeptDate(@TempDir Path backupFolder)
      throws IOException {
    Path expiredFolder = Files.createDirectories(backupFolder.resolve("2026-01-01"));
    Files.writeString(expiredFolder.resolve("2026-01-01_npc.backup.npc.nbt"), "backup");
    Path keptFolder = Files.createDirectories(backupFolder.resolve("2026-02-01"));
    Path unrelatedFolder = Files.createDirectories(backupFolder.resolve("manual"));

    int removedFolders =
        BackupDataFiles.removeBackupFoldersBefore(backupFolder, LocalDate.of(2026, 2, 1));

    assertEquals(1, removedFolders);
    assertFalse(Files.exists(expiredFolder));
    assertTrue(Files.exists(keptFolder));
    assertTrue(Files.exists(unrelatedFolder));
  }

  @Test
  void testMissingBackupFolderRemovesNothing(@TempDir Path temporaryFolder) {
    assertEquals(
        0,
        BackupDataFiles.removeBackupFoldersBefore(
            temporaryFolder.resolve("missing"), LocalDate.of(2026, 2, 1)));
  }
}
