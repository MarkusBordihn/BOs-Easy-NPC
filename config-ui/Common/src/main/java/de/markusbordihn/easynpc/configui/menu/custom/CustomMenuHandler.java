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

package de.markusbordihn.easynpc.configui.menu.custom;

import de.markusbordihn.easynpc.configui.data.custom.CustomMenuType;
import de.markusbordihn.easynpc.configui.menu.preset.PresetBrowserMenu;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.io.DataPresetDataFiles;
import de.markusbordihn.easynpc.io.PresetFileHandler;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.security.ActorSecurityContext;
import de.markusbordihn.easynpc.security.CommandSecurity;
import de.markusbordihn.easynpc.security.FeatureSecurity;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.MenuType;

public class CustomMenuHandler {

  private CustomMenuHandler() {}

  private static List<Identifier> sortedPresets(Set<Identifier> presets) {
    return presets.stream().sorted(Comparator.comparing(Identifier::toString)).toList();
  }

  public static ScreenData getScreenData(
      final CustomMenuType customMenuType, final ServerPlayer serverPlayer) {
    CompoundTag additionalData = new CompoundTag();

    if (customMenuType == CustomMenuType.PRESET_BROWSER) {
      ActorSecurityContext actorSecurityContext = CommandSecurity.getActorContext(serverPlayer);
      additionalData.putString(
          "SecurityRole", FeatureSecurity.getRole(actorSecurityContext).name());
      additionalData.putString(
          "SecurityCommandLevel",
          actorSecurityContext != null ? actorSecurityContext.permissionLevel().name() : "ALL");

      // WORLD Presets (server-side world folder)
      WorldPresetDataFiles.refreshPresetIdentifiers();
      Set<Identifier> worldPresets = new LinkedHashSet<>();
      CompoundTag worldMetadata = new CompoundTag();
      CompoundTag worldData = new CompoundTag();
      for (Identifier preset : sortedPresets(WorldPresetDataFiles.getPresetIdentifierSet())) {
        PresetMetadata metadata = WorldPresetDataFiles.getPresetMetadata(preset);
        if (!metadata.access().isListedForPlayers()) {
          continue;
        }
        worldPresets.add(preset);
        worldMetadata.put(preset.toString(), metadata.toCompoundTag());
        Path presetPath = WorldPresetDataFiles.getPresetsIdentifierPath(preset);
        if (presetPath != null) {
          CompoundTag presetTag = PresetFileHandler.loadNbt(presetPath.toFile());
          if (presetTag != null) {
            worldData.put(preset.toString(), presetTag);
          }
        }
      }
      additionalData.put("WorldPresets", CompoundTagUtils.writeIdentifiers(worldPresets));
      additionalData.put("WorldPresetsMetadata", worldMetadata);
      additionalData.put("WorldPresetsData", worldData);

      // CUSTOM Presets (server-side config folder)
      CustomPresetDataFiles.refreshPresetIdentifiers();
      Set<Identifier> customPresets = new LinkedHashSet<>();
      CompoundTag customMetadata = new CompoundTag();
      CompoundTag customData = new CompoundTag();
      for (Identifier preset : sortedPresets(CustomPresetDataFiles.getPresetIdentifierSet())) {
        PresetMetadata metadata = CustomPresetDataFiles.getPresetMetadata(preset);
        if (!metadata.access().isListedForPlayers()) {
          continue;
        }
        customPresets.add(preset);
        customMetadata.put(preset.toString(), metadata.toCompoundTag());
        Path presetPath = CustomPresetDataFiles.getPresetsIdentifierPath(preset);
        if (presetPath != null) {
          CompoundTag presetTag = PresetFileHandler.loadNbt(presetPath.toFile());
          if (presetTag != null) {
            customData.put(preset.toString(), presetTag);
          }
        }
      }
      additionalData.put("CustomPresets", CompoundTagUtils.writeIdentifiers(customPresets));
      additionalData.put("CustomPresetsMetadata", customMetadata);
      additionalData.put("CustomPresetsData", customData);

      // DATA Presets (datapacks)
      Set<Identifier> dataPresets = new LinkedHashSet<>();
      CompoundTag dataMetadata = new CompoundTag();
      for (Identifier preset :
          sortedPresets(
              DataPresetDataFiles.getPresetIdentifiers(serverPlayer.level().getServer())
                  .collect(Collectors.toSet()))) {
        PresetMetadata metadata =
            DataPresetDataFiles.getPresetMetadata(serverPlayer.level().getServer(), preset);
        if (!metadata.access().isListedForPlayers()) {
          continue;
        }
        dataPresets.add(preset);
        dataMetadata.put(preset.toString(), metadata.toCompoundTag());
      }
      additionalData.put("DataPresets", CompoundTagUtils.writeIdentifiers(dataPresets));
      additionalData.put("DataPresetsMetadata", dataMetadata);
    }

    return new ScreenData(UUID.randomUUID(), null, null, null, null, 0, additionalData);
  }

  public static MenuProvider getMenuProvider(
      final CustomMenuType customMenuType,
      final MenuType<?> menuType,
      final ScreenData screenData) {
    final Component displayName = getCustomMenuTitle(customMenuType);

    if (customMenuType == CustomMenuType.PRESET_BROWSER) {
      return new MenuProvider() {
        @Override
        public AbstractContainerMenu createMenu(
            int containerId, Inventory playerInventory, Player player) {
          return new PresetBrowserMenu(menuType, containerId, playerInventory, screenData.encode());
        }

        @Override
        public Component getDisplayName() {
          return displayName;
        }
      };
    }

    return null;
  }

  private static Component getCustomMenuTitle(final CustomMenuType customMenuType) {
    return TextComponent.getTranslatedConfigText(customMenuType.getName() + ".title");
  }
}
