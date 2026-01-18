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
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.preset.PresetType;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ClientDefaultPresetDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String INDEX_PATH = "/data/easy_npc/default_preset/default_preset.index";
  private static List<ResourceLocation> cachedPresetLocations = null;

  private ClientDefaultPresetDataFiles() {}

  public static Stream<ResourceLocation> getDefaultPresetResourceLocations() {
    if (cachedPresetLocations != null) {
      return cachedPresetLocations.stream();
    }

    // Load index file
    InputStream indexStream = ClientDefaultPresetDataFiles.class.getResourceAsStream(INDEX_PATH);
    if (indexStream == null) {
      log.error("DEFAULT preset index file not found: {}", INDEX_PATH);
      return Stream.empty();
    }

    // Read index file line by line
    List<ResourceLocation> presetLocations = new ArrayList<>();
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(indexStream))) {
      String line;
      while ((line = reader.readLine()) != null) {
        line = line.trim();
        if (line.isEmpty() || line.startsWith("#")) {
          continue;
        }

        String fullPath = DataFileHandler.RESOURCE_DEFAULT_PRESET_PATH + "/" + line;
        ResourceLocation resourceLocation =
            ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, fullPath);
        if (ClientDefaultPresetDataFiles.class.getResourceAsStream(
                "/data/" + Constants.MOD_ID + "/" + fullPath)
            != null) {
          presetLocations.add(resourceLocation);
        } else {
          log.warn("DEFAULT preset listed in index but not found: {}", resourceLocation);
        }
      }

      // Cache loaded preset locations
      cachedPresetLocations = presetLocations;
      log.info("Loaded {} DEFAULT presets", presetLocations.size());
    } catch (Exception e) {
      log.error("Failed to load DEFAULT presets: {}", e.getMessage(), e);
    }

    return presetLocations.stream();
  }

  public static PresetData loadDefaultPresetData(ResourceLocation resourceLocation) {
    try (InputStream inputStream =
        ClientDefaultPresetDataFiles.class.getResourceAsStream(
            "/data/" + resourceLocation.getNamespace() + "/" + resourceLocation.getPath())) {
      if (inputStream == null) {
        log.error("DEFAULT preset not found: {}", resourceLocation);
        return null;
      }

      CompoundTag compoundTag =
          PresetFileHandler.loadFromInputStream(inputStream, resourceLocation);
      if (compoundTag == null || compoundTag.isEmpty()) {
        log.error("Failed to load data for: {}", resourceLocation);
        return null;
      }

      // Create PresetData from loaded CompoundTag
      PresetData presetData =
          PresetData.fromCompoundTag(resourceLocation, PresetType.DEFAULT, compoundTag);
      if (presetData == null || !presetData.hasValidData()) {
        log.error("Invalid PresetData for: {}", resourceLocation);
        return null;
      }
      return presetData;
    } catch (Exception e) {
      log.error("Failed to load DEFAULT preset {}: {}", resourceLocation, e.getMessage());
      return null;
    }
  }

  public static void clearCache() {
    cachedPresetLocations = null;
  }

  public static PresetMetadata getPresetMetadata(ResourceLocation resourceLocation) {
    try (InputStream inputStream =
        ClientDefaultPresetDataFiles.class.getResourceAsStream(
            "/data/" + resourceLocation.getNamespace() + "/" + resourceLocation.getPath())) {
      if (inputStream == null) {
        log.warn("DEFAULT preset file not found for resource location: {}", resourceLocation);
        return PresetMetadata.createDefault();
      }

      CompoundTag compoundTag =
          PresetFileHandler.loadFromInputStream(inputStream, resourceLocation);
      return PresetFileHandler.extractMetadata(compoundTag);
    } catch (Exception e) {
      log.warn(
          "Failed to load metadata for DEFAULT preset {}: {}", resourceLocation, e.getMessage());
      return PresetMetadata.createDefault();
    }
  }

  public static String getPresetDisplayName(
      ResourceLocation resourceLocation, PresetMetadata metadata) {
    return PresetFileHandler.getDisplayName(resourceLocation, metadata);
  }
}
