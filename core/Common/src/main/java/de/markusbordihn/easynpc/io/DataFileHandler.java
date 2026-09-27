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
import de.markusbordihn.easynpc.data.preset.PresetExportFormat;
import de.markusbordihn.easynpc.utils.ResourceNameNormalizer;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import net.minecraft.client.Minecraft;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.packs.resources.Resource;
import net.minecraft.server.packs.resources.ResourceManager;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DataFileHandler {

  public static final String RESOURCE_NAMESPACED_PRESET_PATH = Constants.MOD_ID + "/preset";
  public static final String RESOURCE_PRESET_PATH = "preset";
  public static final String RESOURCE_DEFAULT_PRESET_PATH = "default_preset";
  public static final String RESOURCE_NAMESPACED_API_PRESET_PATH = Constants.MOD_ID + "/api/preset";
  public static final String RESOURCE_API_PRESET_PATH = "api/preset";
  public static final String RESOURCE_BASE_PRESET_PATH = RESOURCE_API_PRESET_PATH + "/base";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String BACKUP_FOLDER_NAME = "backup";
  protected static final String CACHE_FOLDER_NAME = "cache";
  protected static final String RESOURCE_POSES_PATH = "poses";
  protected static final String RESOURCE_TEXTURES_ENTITY_PATH = "textures/entity";
  private static final Pattern VALID_PRESET_FILENAME_PATTERN = Pattern.compile("[a-zA-Z0-9/._-]+");
  private static final String PRESET_FILE_NAME_FALLBACK_PREFIX = "preset";

  private DataFileHandler() {}

  public static boolean isValidPresetFilename(String filename) {
    return VALID_PRESET_FILENAME_PATTERN.matcher(filename).matches();
  }

  public static boolean isValidPresetFilename(Path path) {
    return isValidPresetFilename(path.getFileName().toString());
  }

  public static boolean isPresetFile(Path path) {
    if (path == null) {
      return false;
    }

    PresetExportFormat format = PresetExportFormat.getPresetExportFormat(path.toString());
    if (format == PresetExportFormat.UNKNOWN || !isValidPresetFilename(path)) {
      return false;
    }

    return Files.isRegularFile(path);
  }

  public static boolean isPresetFile(ResourceLocation resourceLocation) {
    if (resourceLocation == null) {
      return false;
    }

    PresetExportFormat format =
        PresetExportFormat.getPresetExportFormat(resourceLocation.toString());
    return format != PresetExportFormat.UNKNOWN;
  }

  public static String getPresetFileName(String fileName) {
    if (fileName == null || fileName.isEmpty()) {
      return null;
    }

    String result = ResourceNameNormalizer.toFileName(fileName, PRESET_FILE_NAME_FALLBACK_PREFIX);
    if (result.isEmpty() || !VALID_PRESET_FILENAME_PATTERN.matcher(result).matches()) {
      return null;
    }

    if (PresetExportFormat.hasPresetExtension(result)) {
      return result;
    }

    return result + PresetExportFormat.getDefault().getFileExtension();
  }

  public static void registerCommonDataFiles() {
    log.info("{} Common data folders ...", Constants.LOG_REGISTER_PREFIX);
    getCacheFolder();
    getCustomDataFolder();
  }

  public static void registerServerDataFiles(MinecraftServer minecraftServer) {
    log.info("{} Server data folders ...", Constants.LOG_REGISTER_PREFIX);

    log.debug("{} Pose data from data packs ...", Constants.LOG_REGISTER_PREFIX);
    CustomPoseDataFiles.registerCustomPoseData(minecraftServer);

    log.debug("{} Backup data folders ...", Constants.LOG_REGISTER_PREFIX);
    BackupDataFiles.registerBackupData();

    log.debug("{} Preset data folders ...", Constants.LOG_REGISTER_PREFIX);
    CustomPresetDataFiles.registerCustomPresetData();
    WorldPresetDataFiles.registerWorldPresetData();
  }

  public static void registerClientDataFiles() {
    log.info("{} Client data folders ...", Constants.LOG_REGISTER_PREFIX);

    log.debug("{} Skin data folders ...", Constants.LOG_REGISTER_PREFIX);
    CustomSkinDataFiles.registerCustomSkinData();
    PlayerSkinDataFiles.registerPlayerSkinData();
    RemoteSkinDataFiles.registerRemoteSkinData();

    log.debug("{} Preset data folders ...", Constants.LOG_REGISTER_PREFIX);
    LocalPresetDataFiles.registerLocalPresetData();
  }

  public static Path getBackupFolder() {
    return getOrCreateDirectory(
        Constants.GAME_DIR.resolve(Constants.MOD_ID).resolve(BACKUP_FOLDER_NAME), "backup");
  }

  public static Path getCacheFolder() {
    return getOrCreateDirectory(
        Constants.GAME_DIR.resolve(Constants.MOD_ID).resolve(CACHE_FOLDER_NAME), "cache");
  }

  public static Path getCustomDataFolder() {
    return getOrCreateDirectory(Constants.CONFIG_DIR.resolve(Constants.MOD_ID), "custom data");
  }

  public static Path getOrCreateBackupFolder(String dataLabel) {
    return getOrCreateSubdirectory(getBackupFolder(), dataLabel, "backup");
  }

  public static Path getOrCreateCacheFolder(String dataLabel) {
    return getOrCreateSubdirectory(getCacheFolder(), dataLabel, "cache");
  }

  public static Path getOrCreateCustomDataFolder(String dataLabel) {
    return getOrCreateSubdirectory(getCustomDataFolder(), dataLabel, "custom data");
  }

  public static Path getOrCreateSubdirectory(
      Path parentDirectory, String directoryName, String folderLabel) {
    if (parentDirectory == null) {
      return null;
    }

    return getOrCreateDirectory(parentDirectory.resolve(directoryName), folderLabel);
  }

  public static Path getOrCreateDirectory(Path directory, String folderLabel) {
    try {
      if (Files.isDirectory(directory)) {
        return directory;
      }

      log.debug("Creating {} folder at {} ...", folderLabel, directory);
      return Files.createDirectories(directory);
    } catch (Exception exception) {
      log.error(
          "There was an error, creating the {} folder {}:", folderLabel, directory, exception);
    }
    return null;
  }

  public static void forEachPresetFile(
      Path presetDataFolder, BiConsumer<ResourceLocation, Path> presetFileConsumer)
      throws IOException {
    try (Stream<Path> filesStream = Files.walk(presetDataFolder)) {
      filesStream
          .filter(DataFileHandler::isPresetFile)
          .forEach(
              path ->
                  presetFileConsumer.accept(
                      new ResourceLocation(
                          Constants.MOD_ID,
                          RESOURCE_PRESET_PATH
                              + '/'
                              + presetDataFolder
                                  .relativize(path)
                                  .toString()
                                  .replace("\\", "/")
                                  .toLowerCase(Locale.ROOT)),
                      path));
    }
  }

  public static void forEachPngFile(Path directory, Consumer<File> pngFileConsumer) {
    if (directory == null || !Files.isDirectory(directory)) {
      return;
    }

    for (String fileName : directory.toFile().list()) {
      File file = directory.resolve(fileName).toFile();
      if (file.exists() && fileName.endsWith(".png")) {
        pngFileConsumer.accept(file);
      }
    }
  }

  public static boolean copyResourceFile(
      MinecraftServer minecraftServer, ResourceLocation resourceLocation, File targetFile) {
    return copyResourceFile(minecraftServer, resourceLocation, targetFile, false);
  }

  public static boolean copyResourceFile(
      MinecraftServer minecraftServer,
      ResourceLocation resourceLocation,
      File targetFile,
      boolean overwriteExisting) {
    return copyResourceFile(
        minecraftServer.getResourceManager(), resourceLocation, targetFile, overwriteExisting);
  }

  public static boolean copyResourceFile(ResourceLocation resourceLocation, File targetFile) {
    return copyResourceFile(resourceLocation, targetFile, false);
  }

  public static boolean copyResourceFile(
      ResourceLocation resourceLocation, File targetFile, boolean overwriteExisting) {
    return copyResourceFile(
        Minecraft.getInstance().getResourceManager(),
        resourceLocation,
        targetFile,
        overwriteExisting);
  }

  private static boolean copyResourceFile(
      ResourceManager resourceManager,
      ResourceLocation resourceLocation,
      File targetFile,
      boolean overwriteExisting) {
    if (resourceLocation == null || targetFile == null) {
      log.warn("Cannot copy resource file: resourceLocation or targetFile is null");
      return false;
    }

    if (targetFile.exists() && !overwriteExisting) {
      log.debug("Skipping copy of {} to {} - file already exists", resourceLocation, targetFile);
      return true;
    }

    try {
      Optional<Resource> resources = resourceManager.getResource(resourceLocation);
      if (resources.isPresent()) {
        return copyResourceToFile(resources.get(), targetFile);
      } else {
        log.error("Resource {} not found in resource manager", resourceLocation);
        return false;
      }
    } catch (Exception e) {
      log.error("Failed to load resource {}:", resourceLocation, e);
      return false;
    }
  }

  private static boolean copyResourceToFile(Resource resource, File targetFile) {
    try (InputStream inputStream = resource.open();
        OutputStream outputStream = new FileOutputStream(targetFile)) {
      byte[] buffer = new byte[8192];
      int bytesRead;
      long totalBytes = 0;
      while ((bytesRead = inputStream.read(buffer)) > 0) {
        outputStream.write(buffer, 0, bytesRead);
        totalBytes += bytesRead;
      }
      log.debug("Successfully copied {} bytes to {}", totalBytes, targetFile);
      return true;
    } catch (Exception e) {
      log.error("Failed to copy resource to file {}:", targetFile, e);
      return false;
    }
  }

  public static String getFileNameFromResourceLocation(ResourceLocation resourceLocation) {
    if (resourceLocation == null) {
      return null;
    }

    return Paths.get(resourceLocation.getPath()).getFileName().toString();
  }
}
