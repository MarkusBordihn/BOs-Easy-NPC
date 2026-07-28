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
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import java.io.InputStream;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;
import net.minecraft.resources.Identifier;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.packs.resources.Resource;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DataPresetDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final long REFRESH_COOLDOWN_MS = 5000;
  private static List<Identifier> cachedUsablePresets;
  private static long lastAccessRefreshTime = 0;

  private DataPresetDataFiles() {}

  public static Stream<Identifier> getPresetIdentifiers(MinecraftServer minecraftServer) {
    try {
      return Stream.concat(
          listPresets(
              minecraftServer,
              DataFileHandler.RESOURCE_NAMESPACED_PRESET_PATH,
              DataFileHandler::isPresetFile),
          listPresets(
              minecraftServer,
              DataFileHandler.RESOURCE_PRESET_PATH,
              resourceLocation ->
                  resourceLocation.getNamespace().equals(Constants.MOD_ID)
                      && DataFileHandler.isPresetFile(resourceLocation)));
    } catch (Exception e) {
      log.error("Could not get data preset resource locations:", e);
    }
    return Stream.empty();
  }

  private static Stream<Identifier> listPresets(
      MinecraftServer minecraftServer, String resourcePath, Predicate<Identifier> filter) {
    return minecraftServer
        .getResourceManager()
        .listResources(resourcePath, filter)
        .keySet()
        .stream();
  }

  public static Stream<Identifier> getUsablePresetIdentifiers(MinecraftServer minecraftServer) {
    long currentTime = System.currentTimeMillis();
    if (cachedUsablePresets == null || currentTime - lastAccessRefreshTime >= REFRESH_COOLDOWN_MS) {
      cachedUsablePresets =
          getPresetIdentifiers(minecraftServer)
              .filter(
                  resourceLocation ->
                      getPresetMetadata(minecraftServer, resourceLocation)
                          .access()
                          .isUsableByCommand())
              .toList();
      lastAccessRefreshTime = currentTime;
    }

    return cachedUsablePresets.stream();
  }

  public static PresetMetadata getPresetMetadata(
      MinecraftServer minecraftServer, Identifier resourceLocation) {
    try {
      Resource resource =
          minecraftServer.getResourceManager().getResource(resourceLocation).orElse(null);
      if (resource == null) {
        log.warn("DATA preset resource not found: {}", resourceLocation);
        return PresetMetadata.createDefault();
      }

      try (InputStream inputStream = resource.open()) {
        return PresetFileHandler.extractMetadata(inputStream, resourceLocation);
      }
    } catch (Exception e) {
      log.warn("Failed to load metadata for DATA preset {}: {}", resourceLocation, e.getMessage());
      return PresetMetadata.createDefault();
    }
  }

  public static String getPresetDisplayName(Identifier resourceLocation, PresetMetadata metadata) {
    return PresetFileHandler.getDisplayName(resourceLocation, metadata);
  }
}
