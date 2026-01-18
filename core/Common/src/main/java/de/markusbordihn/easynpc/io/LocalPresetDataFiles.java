/*
 * Copyright 2025 Markus Bordihn
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
import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.nio.file.Path;
import java.util.Set;
import java.util.stream.Stream;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LocalPresetDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private LocalPresetDataFiles() {}

  public static void registerLocalPresetData() {
    log.info("{} local preset data ...", Constants.LOG_REGISTER_PREFIX);
    CustomPresetDataFiles.registerCustomPresetData();
  }

  @SuppressWarnings("unused")
  public static Stream<ResourceLocation> getPresetResourceLocations(SkinModel skinModel) {
    return CustomPresetDataFiles.getPresetResourceLocations(skinModel);
  }

  @SuppressWarnings("unused")
  public static Stream<ResourceLocation> getPresetResourceLocations() {
    return CustomPresetDataFiles.getPresetResourceLocations();
  }

  @SuppressWarnings("unused")
  public static Set<ResourceLocation> getPresetResourceLocationSet() {
    return CustomPresetDataFiles.getPresetResourceLocationSet();
  }

  public static Path getPresetsResourceLocationPath(ResourceLocation resourceLocation) {
    return CustomPresetDataFiles.getPresetsResourceLocationPath(resourceLocation);
  }

  @SuppressWarnings("unused")
  public static PresetMetadata getPresetMetadata(ResourceLocation resourceLocation) {
    return CustomPresetDataFiles.getPresetMetadata(resourceLocation);
  }

  @SuppressWarnings("unused")
  public static String getPresetDisplayName(
      ResourceLocation resourceLocation, PresetMetadata metadata) {
    return CustomPresetDataFiles.getPresetDisplayName(resourceLocation, metadata);
  }

  @SuppressWarnings("unused")
  public static PresetData loadPresetData(ResourceLocation resourceLocation) {
    try {
      Path presetPath = getPresetsResourceLocationPath(resourceLocation);
      if (presetPath == null) {
        log.warn("Local preset file not found for: {}", resourceLocation);
        return null;
      }

      CompoundTag compoundTag = PresetFileHandler.load(presetPath.toFile());
      if (compoundTag == null || compoundTag.isEmpty()) {
        log.warn("Failed to load preset data from file: {}", presetPath);
        return null;
      }

      return PresetData.fromCompoundTag(resourceLocation, PresetType.LOCAL, compoundTag);
    } catch (Exception e) {
      log.error("Failed to load local preset data for {}: {}", resourceLocation, e.getMessage());
      return null;
    }
  }
}
