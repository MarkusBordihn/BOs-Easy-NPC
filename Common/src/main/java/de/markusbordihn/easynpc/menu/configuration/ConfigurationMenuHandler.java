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

package de.markusbordihn.easynpc.menu.configuration;

import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.io.DefaultPresetDataFiles;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import de.markusbordihn.easynpc.menu.configuration.equipment.EquipmentConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.trading.AdvancedTradingConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.trading.BasicTradingConfigurationMenu;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
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

public class ConfigurationMenuHandler {

  public static MenuProvider getMenuProvider(
      final ConfigurationType configurationType,
      final EasyNPC<?> easyNPC,
      final MenuType<? extends ConfigurationMenu> menuType,
      ScreenData screenData) {
    final Component displayName = configurationType.getConfigurationTitle(easyNPC);

    // Special configuration menu for equipment
    if (configurationType == ConfigurationType.EQUIPMENT) {
      return new MenuProvider() {
        @Override
        public AbstractContainerMenu createMenu(
            int containerId, Inventory playerInventory, Player player) {
          return new EquipmentConfigurationMenu(
              menuType, containerId, playerInventory, screenData.encode());
        }

        @Override
        public Component getDisplayName() {
          return displayName;
        }
      };
    } else if (configurationType == ConfigurationType.ADVANCED_TRADING) {
      return new MenuProvider() {
        @Override
        public AbstractContainerMenu createMenu(
            int containerId, Inventory playerInventory, Player player) {
          return new AdvancedTradingConfigurationMenu(
              menuType, containerId, playerInventory, screenData.encode());
        }

        @Override
        public Component getDisplayName() {
          return displayName;
        }
      };
    } else if (configurationType == ConfigurationType.BASIC_TRADING) {
      return new MenuProvider() {
        @Override
        public AbstractContainerMenu createMenu(
            int containerId, Inventory playerInventory, Player player) {
          return new BasicTradingConfigurationMenu(
              menuType, containerId, playerInventory, screenData.encode());
        }

        @Override
        public Component getDisplayName() {
          return displayName;
        }
      };
    }

    // Default configuration menu
    return new MenuProvider() {
      @Override
      public AbstractContainerMenu createMenu(
          int containerId, Inventory playerInventory, Player player) {
        return new ConfigurationMenu(menuType, containerId, playerInventory, screenData.encode());
      }

      @Override
      public Component getDisplayName() {
        return displayName;
      }
    };
  }

  public static ScreenData getScreenData(
      final ConfigurationType configurationType,
      final EasyNPC<?> easyNPC,
      final ServerPlayer serverPlayer,
      final int pageIndex) {

    // Get basic data for configuration menu.
    final UUID npcUUID = easyNPC.getUUID();

    // Additional data for specific configuration menu.
    final CompoundTag additionalSyncData = new CompoundTag();
    switch (configurationType) {
      case DEFAULT_PRESET_IMPORT -> {
        Set<ResourceLocation> defaultPresets =
            DefaultPresetDataFiles.getPresetResourceLocations(serverPlayer.getServer())
                .collect(Collectors.toSet());
        additionalSyncData.put(
            "DefaultPresets", CompoundTagUtils.writeResourceLocations(defaultPresets));
      }
      case CUSTOM_PRESET_IMPORT -> {
        Set<ResourceLocation> customPresets =
            CustomPresetDataFiles.getPresetResourceLocations().collect(Collectors.toSet());
        additionalSyncData.put(
            "CustomPresets", CompoundTagUtils.writeResourceLocations(customPresets));
      }
      case WORLD_PRESET_IMPORT -> {
        Set<ResourceLocation> worldPresets =
            WorldPresetDataFiles.getPresetResourceLocations().collect(Collectors.toSet());
        additionalSyncData.put(
            "WorldPresets", CompoundTagUtils.writeResourceLocations(worldPresets));
      }
      case BASIC_ACTION, DIALOG_ACTION, DISTANCE_ACTION ->
          AdditionalScreenData.addActionEventSet(additionalSyncData, easyNPC);
      case BASE_ATTRIBUTE -> AdditionalScreenData.addBaseAttributes(additionalSyncData, easyNPC);
      case NONE_DIALOG, BASIC_DIALOG, YES_NO_DIALOG, ADVANCED_DIALOG ->
          AdditionalScreenData.addDialogDataSet(additionalSyncData, easyNPC);
      case ATTACK_OBJECTIVE, BASIC_OBJECTIVE, FOLLOW_OBJECTIVE, LOOK_OBJECTIVE ->
          AdditionalScreenData.addObjectiveDataSet(additionalSyncData, easyNPC);
      default -> {
        // Do nothing
      }
    }
    return new ScreenData(npcUUID, null, null, pageIndex, additionalSyncData);
  }
}
