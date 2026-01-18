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
import java.util.stream.Stream;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.packs.resources.ResourceManager;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DefaultPresetDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private DefaultPresetDataFiles() {}

  public static Stream<ResourceLocation> getPresetResourceLocations(
      MinecraftServer minecraftServer) {
    if (minecraftServer == null) {
      log.warn("Cannot get default preset resource locations: server is null");
      return Stream.empty();
    }
    return getPresetResourceLocations(minecraftServer.getResourceManager());
  }

  private static Stream<ResourceLocation> getPresetResourceLocations(
      ResourceManager resourceManager) {
    try {
      return resourceManager
          .listResources(
              DataFileHandler.RESOURCE_DEFAULT_PRESET_PATH,
              resourceLocation -> {
                boolean isOurNamespace = resourceLocation.getNamespace().equals(Constants.MOD_ID);
                boolean isPresetFile = DataFileHandler.isPresetFile(resourceLocation);
                if (isOurNamespace && isPresetFile) {
                  log.debug("Found DEFAULT preset from DataPack: {}", resourceLocation);
                }
                return isOurNamespace && isPresetFile;
              })
          .keySet()
          .stream();
    } catch (Exception e) {
      log.error("Could not get default preset resource locations from DataPack:", e);
    }
    return Stream.empty();
  }
}
