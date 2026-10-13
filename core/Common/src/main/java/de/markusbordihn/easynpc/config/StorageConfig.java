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

package de.markusbordihn.easynpc.config;

import java.io.File;
import java.util.Properties;
import java.util.concurrent.TimeUnit;

@SuppressWarnings({"java:S1104", "java:S1444", "java:S3008"})
public class StorageConfig extends Config {

  public static final String CONFIG_FILE_NAME = "storage.cfg";
  public static final String CONFIG_FILE_HEADER =
"""
Storage Configuration

All retention values are in days, 0 keeps the data forever.

backupRetentionDays: Deletes NPC backup folders older than this, shared by all worlds (default: 30)
killedNPCRetentionDays: Deletes the saved data of killed NPCs after this (default: 30)
deletedNPCRetentionDays: Forgets deleted NPCs after this, so their copies in old chunks are no longer removed on load (default: 365)
reportOrphanNPCFiles: Logs NPC files without index entry on world load, never deletes them (default: true)

""";

  private static final int MAX_RETENTION_DAYS = 3650;

  public static int BACKUP_RETENTION_DAYS = 30;
  public static int KILLED_NPC_RETENTION_DAYS = 30;
  public static int DELETED_NPC_RETENTION_DAYS = 365;
  public static boolean REPORT_ORPHAN_NPC_FILES = true;

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    parseConfigFile();
  }

  public static void parseConfigFile() {
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodifiedProperties = (Properties) properties.clone();

    BACKUP_RETENTION_DAYS =
        parseConfigValue(
            properties, "backupRetentionDays", BACKUP_RETENTION_DAYS, 0, MAX_RETENTION_DAYS);
    KILLED_NPC_RETENTION_DAYS =
        parseConfigValue(
            properties, "killedNPCRetentionDays", KILLED_NPC_RETENTION_DAYS, 0, MAX_RETENTION_DAYS);
    DELETED_NPC_RETENTION_DAYS =
        parseConfigValue(
            properties,
            "deletedNPCRetentionDays",
            DELETED_NPC_RETENTION_DAYS,
            0,
            MAX_RETENTION_DAYS);
    REPORT_ORPHAN_NPC_FILES =
        parseConfigValue(properties, "reportOrphanNPCFiles", REPORT_ORPHAN_NPC_FILES);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodifiedProperties);
  }

  public static long toRetentionMilliseconds(int retentionDays) {
    return TimeUnit.DAYS.toMillis(retentionDays);
  }
}
