/*
 * Copyright 2023 Markus Bordihn
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
import de.markusbordihn.easynpc.debug.Logger;
import net.minecraft.client.Minecraft;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.packs.resources.Resource;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;

public class DataFileHandler {

  protected static final String BACKUP_FOLDER_NAME = "backup";
  protected static final String CACHE_FOLDER_NAME = "cache";

  private DataFileHandler() {}

  public static void registerCommonDataFiles() {
    Logger.INSTANCE.info("{} Common data folders ...", Constants.LOG_REGISTER_PREFIX);
    getCacheFolder();
    getCustomDataFolder();
  }

  public static void registerServerDataFiles(MinecraftServer minecraftServer) {
    Logger.INSTANCE.info("{} Server data folders ...", Constants.LOG_REGISTER_PREFIX);

    Logger.INSTANCE.info("{} Pose data folders ...", Constants.LOG_REGISTER_PREFIX);
    CustomPoseDataFiles.registerCustomPoseData(minecraftServer);

    Logger.INSTANCE.info("{} Backup data folders ...", Constants.LOG_REGISTER_PREFIX);
    BackupDataFiles.registerBackupData();
  }

  public static void registerClientDataFiles() {
    Logger.INSTANCE.info("{} Client data folders ...", Constants.LOG_REGISTER_PREFIX);

    Logger.INSTANCE.info("{} Skin data folders ...", Constants.LOG_REGISTER_PREFIX);
    CustomSkinDataFiles.registerCustomSkinData();
    PlayerSkinDataFiles.registerPlayerSkinData();
    RemoteSkinDataFiles.registerRemoteSkinData();

    Logger.INSTANCE.info("{} Preset data folders ...", Constants.LOG_REGISTER_PREFIX);
    CustomPresetDataFiles.registerCustomPresetData();
  }

  public static Path getBackupFolder() {
    Path backupFolder = Constants.GAME_DIR.resolve(Constants.MOD_ID).resolve(BACKUP_FOLDER_NAME);
    try {
      if (Files.exists(backupFolder) && Files.isDirectory(backupFolder)) {
        return backupFolder;
      }
      Logger.INSTANCE.info("Creating backup folder at {} ...", backupFolder);
      return Files.createDirectories(backupFolder);
    } catch (Exception exception) {
      Logger.INSTANCE.error("There was an error, creating the backup folder:", exception);
    }
    return null;
  }

  public static Path getCacheFolder() {
    Path cacheFolder = Constants.GAME_DIR.resolve(Constants.MOD_ID).resolve(CACHE_FOLDER_NAME);
    try {
      if (Files.exists(cacheFolder) && Files.isDirectory(cacheFolder)) {
        return cacheFolder;
      }
      Logger.INSTANCE.info("Creating cache folder at {} ...", cacheFolder);
      return Files.createDirectories(cacheFolder);
    } catch (Exception exception) {
      Logger.INSTANCE.error("There was an error, creating the cache folder:", exception);
    }
    return null;
  }

  public static Path getCustomDataFolder() {
    Path customDataFolder = Constants.CONFIG_DIR.resolve(Constants.MOD_ID);
    try {
      if (Files.exists(customDataFolder) && Files.isDirectory(customDataFolder)) {
        return customDataFolder;
      }
      Logger.INSTANCE.info("Creating custom data folder at {} ...", customDataFolder);
      return Files.createDirectories(customDataFolder);
    } catch (Exception exception) {
      Logger.INSTANCE.error("There was an error, creating the custom data folder:", exception);
    }
    return null;
  }

  public static Path getOrCreateBackupFolder(String dataLabel) {
    Path backupFolder = getBackupFolder();
    if (backupFolder == null) {
      return null;
    }
    Path backupFolderPath = backupFolder.resolve(dataLabel);
    try {
      if (Files.exists(backupFolderPath) && Files.isDirectory(backupFolderPath)) {
        return backupFolderPath;
      }
      Logger.INSTANCE.info("Creating backup folder {} at {} ...", dataLabel, backupFolder);
      return Files.createDirectories(backupFolderPath);
    } catch (Exception exception) {
      Logger.INSTANCE.error("There was an error, creating the backup folder {}:", dataLabel, exception);
    }
    return null;
  }

  public static Path getOrCreateCacheFolder(String dataLabel) {
    Path cacheFolder = getCacheFolder();
    if (cacheFolder == null) {
      return null;
    }
    Path cacheFolderPath = cacheFolder.resolve(dataLabel);
    try {
      if (Files.exists(cacheFolderPath) && Files.isDirectory(cacheFolderPath)) {
        return cacheFolderPath;
      }
      Logger.INSTANCE.info("Creating cache folder {} at {} ...", dataLabel, cacheFolder);
      return Files.createDirectories(cacheFolderPath);
    } catch (Exception exception) {
      Logger.INSTANCE.error("There was an error, creating the cache folder {}:", dataLabel, exception);
    }
    return null;
  }

  public static Path getOrCreateCustomDataFolder(String dataLabel) {
    Path customDataFolder = getCustomDataFolder();
    if (customDataFolder == null) {
      return null;
    }
    Path customDataFolderPath = customDataFolder.resolve(dataLabel);
    try {
      if (Files.exists(customDataFolderPath) && Files.isDirectory(customDataFolderPath)) {
        return customDataFolderPath;
      }
      Logger.INSTANCE.info("Creating custom data folder {} at {} ...", dataLabel, customDataFolder);
      return Files.createDirectories(customDataFolderPath);
    } catch (Exception exception) {
      Logger.INSTANCE.error("There was an error, creating the custom data folder {}:", dataLabel, exception);
    }
    return null;
  }

  public static void copyResourceFile(
      MinecraftServer minecraftServer, ResourceLocation resourceLocation, File targetFile) {
    if (resourceLocation == null || targetFile == null) {
      return;
    }
    try {
      Optional<Resource> resources =
          minecraftServer.getResourceManager().getResource(resourceLocation);
      if (resources.isPresent()) {
        try (InputStream inputStream = resources.get().open();
            OutputStream outputStream = new FileOutputStream(targetFile)) {
          byte[] buffer = new byte[1024];
          int length;
          while ((length = inputStream.read(buffer)) > 0) {
            outputStream.write(buffer, 0, length);
          }
        }
      }
    } catch (Exception e) {
      Logger.INSTANCE.error("Failed to load resource {}:", resourceLocation, e);
    }
  }

  public static void copyResourceFile(ResourceLocation resourceLocation, File targetFile) {
    if (resourceLocation == null || targetFile == null) {
      return;
    }
    try {
      Optional<Resource> resources =
          Minecraft.getInstance().getResourceManager().getResource(resourceLocation);
      if (resources.isPresent()) {
        try (InputStream inputStream = resources.get().open();
            OutputStream outputStream = new FileOutputStream(targetFile)) {
          byte[] buffer = new byte[1024];
          int length;
          while ((length = inputStream.read(buffer)) > 0) {
            outputStream.write(buffer, 0, length);
          }
        }
      }
    } catch (Exception e) {
      Logger.INSTANCE.error("Failed to load resource {}:", resourceLocation, e);
    }
  }

  public static String getFileNameFromResourceLocation(ResourceLocation resourceLocation) {
    if (resourceLocation == null) {
      return null;
    }
    return Paths.get(resourceLocation.getPath()).getFileName().toString();
  }
}
