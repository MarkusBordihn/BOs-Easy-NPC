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
import java.util.List;
import java.util.UUID;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String DATA_FOLDER_NAME = "preset";

  protected PresetDataFiles() {}

  public static void registerCustomPresetData() {
    log.info("{} custom preset data ...", Constants.LOG_REGISTER_PREFIX);

    // Prepare preset data folder
    Path presetDataFolder = getPresetDataFolder();
    if (presetDataFolder == null) {
      return;
    }
    log.info(
        "{} custom preset data folder at {} ...", Constants.LOG_CREATE_PREFIX, presetDataFolder);

    for (SkinModel skinModel : SkinModel.values()) {
      Path presetModelFolder = getPresetDataFolder(skinModel);
      if (presetModelFolder != null) {
        log.info(
            "{} preset model folder {} at {} ...",
            Constants.LOG_CREATE_PREFIX,
            skinModel,
            presetModelFolder);
      }
    }
  }

  public static Path getPresetDataFolder() {
    return DataFileHandler.getOrCreateCustomDataFolder(DATA_FOLDER_NAME);
  }

  public static Path getPresetDataFolder(SkinModel skinModel) {
    Path skinDataFolder = getPresetDataFolder();
    String skinModelName = skinModel.name();
    if (skinDataFolder != null && skinModelName != null) {
      try {
        return Files.createDirectories(skinDataFolder.resolve(skinModelName.toLowerCase()));
      } catch (IOException exception) {
        log.error("Could not create preset data folder {}!", skinDataFolder, exception);
      }
    }
    return null;
  }

  public static File getPresetFile(SkinModel skinModel, String fileName) {
    Path presetModelFolder = getPresetDataFolder(skinModel);
    if (presetModelFolder != null && fileName != null && !fileName.isEmpty()) {
      // Remove all invalid characters from file name.
      fileName = fileName.replaceAll("[^a-zA-Z0-9/._-]", "").replace("..", "");
      return presetModelFolder
          .resolve(
              fileName.endsWith(Constants.NPC_NBT_SUFFIX)
                  ? fileName
                  : fileName + Constants.NPC_NBT_SUFFIX)
          .toFile();
    }
    return null;
  }

  public static File getPresetFile(SkinModel skinModel, UUID uuid) {
    return getPresetFile(skinModel, uuid.toString());
  }

  public static Stream<Path> getPresetFilePathLocations() {
    Path presetDataFolder = getPresetDataFolder();
    try {
      try (Stream<Path> filesStream = Files.walk(presetDataFolder)) {
        // Get all files with the suffix .npc.nbt and return the relative path.
        List<Path> filePaths =
            filesStream
                .filter(path -> path.toString().endsWith(Constants.NPC_NBT_SUFFIX))
                .filter(path -> Pattern.matches("[a-zA-Z0-9/._-]+", path.getFileName().toString()))
                .toList();
        return filePaths.stream();
      }
    } catch (IOException exception) {
      log.error("Could not read preset data folder {}!", presetDataFolder, exception);
    }

    // Return a default or alternative stream in case of an exception
    return Stream.empty();
  }
}
