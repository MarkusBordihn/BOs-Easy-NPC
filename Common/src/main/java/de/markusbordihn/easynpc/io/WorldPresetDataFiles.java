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
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class WorldPresetDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String DATA_FOLDER_NAME = "preset";
  private static final ConcurrentHashMap<ResourceLocation, Path> presetResourceLocationMap =
      new ConcurrentHashMap<>();

  private WorldPresetDataFiles() {
  }

  public static Path getPresetDataFolder() {
    File worldDataFolder = new File(Constants.WORLD_DIR.toFile(), Constants.MOD_ID);
    Path path = worldDataFolder.toPath().resolve(DATA_FOLDER_NAME);
    if (!path.toFile().exists() && !path.toFile().mkdirs()) {
      log.error("Could not create preset data folder {}!", path);
    }
    return path;
  }

  public static Path getPresetDataFolder(SkinModel skinModel) {
    Path presetDataFolder = getPresetDataFolder();
    String skinModelName = skinModel.name();
    if (!skinModelName.isEmpty()) {
      Path path = presetDataFolder.resolve(skinModelName.toLowerCase());
      if (!path.toFile().exists() && !path.toFile().mkdirs()) {
        log.error("Could not create preset model folder {}!", path);
      }
      return path;
    }
    return null;
  }

  public static File getPresetFile(SkinModel skinModel, String fileName) {
    Path presetModelFolder = getPresetDataFolder(skinModel);
    if (presetModelFolder != null && fileName != null && !fileName.isEmpty()) {
      return presetModelFolder.resolve(getPresetFileName(fileName)).toFile();
    }
    return null;
  }

  public static String getPresetFileName(String fileName) {
    String result = fileName.replaceAll("[^a-zA-Z0-9/._-]", "").replace("..", "").replace("/", "_");
    return result.endsWith(Constants.NPC_NBT_SUFFIX) ? result : result + Constants.NPC_NBT_SUFFIX;
  }

  public static Stream<ResourceLocation> getPresetResourceLocations() {
    Path presetDataFolder = getPresetDataFolder();
    try {
      try (Stream<Path> filesStream = Files.walk(presetDataFolder)) {
        // Get all files with the suffix .npc.nbt and return the relative path.
        List<ResourceLocation> filePaths =
            filesStream
                .filter(path -> path.toString().endsWith(Constants.NPC_NBT_SUFFIX))
                .filter(path -> Pattern.matches("[a-z0-9/._-]+", path.getFileName().toString()))
                .map(
                    path -> {
                      ResourceLocation resourceLocation =
                          new ResourceLocation(
                              Constants.MOD_ID,
                              DATA_FOLDER_NAME
                                  + '/'
                                  + presetDataFolder
                                  .relativize(path)
                                  .toString()
                                  .replace("\\", "/"));
                      presetResourceLocationMap.put(resourceLocation, path);
                      return resourceLocation;
                    })
                .toList();
        return filePaths.stream();
      }
    } catch (IOException exception) {
      log.error("Could not read world preset data folder {}!", presetDataFolder, exception);
    }

    // Return a default or alternative stream in case of an exception
    return Stream.empty();
  }

  public static Path getPresetsResourceLocationPath(ResourceLocation resourceLocation) {
    return presetResourceLocationMap.get(resourceLocation);
  }
}
