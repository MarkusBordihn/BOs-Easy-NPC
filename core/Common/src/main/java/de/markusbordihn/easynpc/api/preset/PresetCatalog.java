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

package de.markusbordihn.easynpc.api.preset;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.io.DataPresetDataFiles;
import de.markusbordihn.easynpc.io.DefaultPresetDataFiles;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetCatalog {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private PresetCatalog() {}

  public static Set<ResourceLocation> listPresets(MinecraftServer minecraftServer) {
    Set<ResourceLocation> presetLocations = new LinkedHashSet<>();
    for (PresetType presetType : PresetType.values()) {
      presetLocations.addAll(listPresets(minecraftServer, presetType));
    }

    return presetLocations;
  }

  public static Set<ResourceLocation> listPresets(
      MinecraftServer minecraftServer, PresetType presetType) {
    if (presetType == null) {
      log.error("Unable to list presets without a preset type");
      return Set.of();
    }

    return switch (presetType) {
      case CUSTOM, LOCAL ->
          listedForPlayers(
              CustomPresetDataFiles.getPresetResourceLocations(),
              CustomPresetDataFiles::getPresetMetadata);
      case WORLD ->
          listedForPlayers(
              WorldPresetDataFiles.getPresetResourceLocations(),
              WorldPresetDataFiles::getPresetMetadata);
      case DATA ->
          listedServerPresets(minecraftServer, DataPresetDataFiles::getPresetResourceLocations);
      case DEFAULT ->
          listedServerPresets(minecraftServer, DefaultPresetDataFiles::getPresetResourceLocations);
    };
  }

  private static Set<ResourceLocation> listedServerPresets(
      MinecraftServer minecraftServer,
      Function<MinecraftServer, Stream<ResourceLocation>> presetLocationProvider) {
    if (minecraftServer == null) {
      log.error("Unable to list presets without a server");
      return Set.of();
    }

    return listedForPlayers(
        presetLocationProvider.apply(minecraftServer),
        presetLocation -> DataPresetDataFiles.getPresetMetadata(minecraftServer, presetLocation));
  }

  private static Set<ResourceLocation> listedForPlayers(
      Stream<ResourceLocation> presetLocations,
      Function<ResourceLocation, PresetMetadata> metadataProvider) {
    return presetLocations
        .filter(
            presetLocation -> metadataProvider.apply(presetLocation).access().isListedForPlayers())
        .collect(Collectors.toCollection(LinkedHashSet::new));
  }
}
