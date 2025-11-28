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
import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CustomPresetDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String DATA_FOLDER_NAME = "preset";
  private static final ConcurrentHashMap<ResourceLocation, Path> presetResourceLocationMap =
      new ConcurrentHashMap<>();

  private CustomPresetDataFiles() {}

  public static void registerCustomPresetData() {
    log.info("{} custom preset data ...", Constants.LOG_REGISTER_PREFIX);

    Path presetDataFolder = getPresetDataFolder();
    if (presetDataFolder == null) {
      return;
    }

    for (SkinModel skinModel : SkinModel.values()) {
      getPresetDataFolder(skinModel);
    }

    refreshPresetResourceLocations();
  }

  public static Path getPresetDataFolder() {
    return DataFileHandler.getOrCreateCustomDataFolder(DATA_FOLDER_NAME);
  }

  public static Path getPresetDataFolder(SkinModel skinModel) {
    Path skinDataFolder = getPresetDataFolder();
    if (skinDataFolder == null) {
      return null;
    }
    String skinModelName = skinModel.getName();
    Path presetDataFolderPath = skinDataFolder.resolve(skinModelName);
    try {
      if (Files.exists(presetDataFolderPath) && Files.isDirectory(presetDataFolderPath)) {
        return presetDataFolderPath;
      }
      log.info("Creating preset data folder {} at {} ...", skinModelName, presetDataFolderPath);
      return Files.createDirectories(presetDataFolderPath);
    } catch (IOException exception) {
      log.error("Could not create preset data folder {}:", skinDataFolder, exception);
    }
    return null;
  }

  public static File getPresetFile(SkinModel skinModel, String fileName) {
    Path presetModelFolder = getPresetDataFolder(skinModel);
    if (presetModelFolder == null || fileName == null || fileName.isEmpty()) {
      return null;
    }
    String sanitizedFileName = DataFileHandler.getPresetFileName(fileName);
    if (sanitizedFileName == null) {
      return null;
    }
    return presetModelFolder.resolve(sanitizedFileName).toFile();
  }

  public static File getPresetFile(SkinModel skinModel, UUID uuid) {
    return getPresetFile(skinModel, uuid.toString());
  }

  public static Stream<ResourceLocation> getPresetResourceLocations(SkinModel skinModel) {
    String searchName = "/" + skinModel.getName() + "/";
    return getPresetResourceLocations()
        .filter(
            resourceLocation ->
                resourceLocation.toString().contains(searchName)
                    && DataFileHandler.isPresetFile(resourceLocation));
  }

  public static Stream<ResourceLocation> getPresetResourceLocations() {
    return presetResourceLocationMap.keySet().stream();
  }

  public static Set<ResourceLocation> getPresetResourceLocationSet() {
    return presetResourceLocationMap.keySet();
  }

  public static void refreshPresetResourceLocations() {
    Path presetDataFolder = getPresetDataFolder();
    if (presetDataFolder == null) {
      return;
    }
    presetResourceLocationMap.clear();
    try (Stream<Path> filesStream = Files.walk(presetDataFolder)) {
      filesStream
          .filter(DataFileHandler::isPresetFile)
          .forEach(
              path -> {
                ResourceLocation resourceLocation =
                    ResourceLocation.fromNamespaceAndPath(
                        Constants.MOD_ID,
                        DATA_FOLDER_NAME
                            + '/'
                            + presetDataFolder
                                .relativize(path)
                                .toString()
                                .replace("\\", "/")
                                .toLowerCase(Locale.ROOT));
                presetResourceLocationMap.put(resourceLocation, path);
              });
    } catch (IOException exception) {
      log.error("Could not read custom preset data folder {}:", presetDataFolder, exception);
    }
  }

  public static Path getPresetsResourceLocationPath(ResourceLocation resourceLocation) {
    Path path = presetResourceLocationMap.get(resourceLocation);
    if (path == null) {
      refreshPresetResourceLocations();
      path = presetResourceLocationMap.get(resourceLocation);
    }
    return path;
  }
}
