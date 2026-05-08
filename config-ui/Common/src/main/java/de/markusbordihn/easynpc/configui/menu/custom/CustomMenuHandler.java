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
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.MenuType;

public class CustomMenuHandler {

  private CustomMenuHandler() {}

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
      WorldPresetDataFiles.refreshPresetResourceLocations();
      Set<ResourceLocation> worldPresets = WorldPresetDataFiles.getPresetResourceLocationSet();
      additionalData.put("WorldPresets", CompoundTagUtils.writeResourceLocations(worldPresets));
      CompoundTag worldMetadata = new CompoundTag();
      CompoundTag worldData = new CompoundTag();
      for (ResourceLocation preset : worldPresets) {
        PresetMetadata metadata = WorldPresetDataFiles.getPresetMetadata(preset);
        worldMetadata.put(preset.toString(), metadata.toCompoundTag());
        Path presetPath = WorldPresetDataFiles.getPresetsResourceLocationPath(preset);
        if (presetPath != null) {
          CompoundTag presetTag = PresetFileHandler.loadNbt(presetPath.toFile());
          if (presetTag != null) {
            worldData.put(preset.toString(), presetTag);
          }
        }
      }
      additionalData.put("WorldPresetsMetadata", worldMetadata);
      additionalData.put("WorldPresetsData", worldData);

      // CUSTOM Presets (server-side config folder)
      CustomPresetDataFiles.refreshPresetResourceLocations();
      Set<ResourceLocation> customPresets = CustomPresetDataFiles.getPresetResourceLocationSet();
      additionalData.put("CustomPresets", CompoundTagUtils.writeResourceLocations(customPresets));
      CompoundTag customMetadata = new CompoundTag();
      CompoundTag customData = new CompoundTag();
      for (ResourceLocation preset : customPresets) {
        PresetMetadata metadata = CustomPresetDataFiles.getPresetMetadata(preset);
        customMetadata.put(preset.toString(), metadata.toCompoundTag());
        Path presetPath = CustomPresetDataFiles.getPresetsResourceLocationPath(preset);
        if (presetPath != null) {
          CompoundTag presetTag = PresetFileHandler.loadNbt(presetPath.toFile());
          if (presetTag != null) {
            customData.put(preset.toString(), presetTag);
          }
        }
      }
      additionalData.put("CustomPresetsMetadata", customMetadata);
      additionalData.put("CustomPresetsData", customData);

      // DATA Presets (datapacks)
      Set<ResourceLocation> dataPresets =
          DataPresetDataFiles.getPresetResourceLocations(serverPlayer.getServer())
              .collect(Collectors.toSet());
      additionalData.put("DataPresets", CompoundTagUtils.writeResourceLocations(dataPresets));
      CompoundTag dataMetadata = new CompoundTag();
      for (ResourceLocation preset : dataPresets) {
        PresetMetadata metadata =
            DataPresetDataFiles.getPresetMetadata(serverPlayer.getServer(), preset);
        dataMetadata.put(preset.toString(), metadata.toCompoundTag());
      }
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
