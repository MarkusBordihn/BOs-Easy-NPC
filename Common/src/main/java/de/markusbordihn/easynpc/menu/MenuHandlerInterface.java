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

package de.markusbordihn.easynpc.menu;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationTypeHelper;
import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenuHandler;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.MenuType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface MenuHandlerInterface {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static void openEasyNpcMenu(
      final ServerPlayer serverPlayer,
      final MenuType<?> menuType,
      final EasyNPC<?> easyNPC,
      final UUID dialogId,
      final UUID dialogButtonId,
      final int pageIndex,
      final Component menuName,
      final CompoundTag data) {
    final UUID npcUUID = easyNPC.getUUID();
    final Component displayName = menuName != null ? menuName : easyNPC.getEntity().getName();
    final ScreenData screenData =
        new ScreenData(npcUUID, dialogId, dialogButtonId, pageIndex, data);
    final MenuProvider menuProvider =
        new MenuProvider() {
          @Override
          public AbstractContainerMenu createMenu(
              int containerId, Inventory playerInventory, Player player) {
            return new EasyNPCMenu(menuType, containerId, playerInventory, screenData.encode());
          }

          @Override
          public Component getDisplayName() {
            return displayName;
          }
        };
    MenuManager.openMenu(npcUUID, menuProvider, serverPlayer, screenData.encode());
  }

  default void openConfigurationMenu(
      final ConfigurationType configurationType,
      final ServerPlayer serverPlayer,
      final EasyNPC<?> easyNPC,
      final int pageIndex) {

    // Handle configuration type alias.
    final ConfigurationType configurationTypeAlias =
        ConfigurationTypeHelper.resolveConfigurationTypeAlias(configurationType, easyNPC);

    // Additional data for specific configuration menu.
    final ScreenData screenData =
        ConfigurationMenuHandler.getScreenData(
            configurationTypeAlias, easyNPC, serverPlayer, pageIndex);

    // Get menu type for configuration type.
    final MenuType<? extends ConfigurationMenu> menuType =
        getMenuTypeByConfigurationType(configurationTypeAlias);
    if (menuType == null) {
      log.error(
          "Unknown configuration {} for {} from {}", configurationTypeAlias, easyNPC, serverPlayer);
      return;
    }

    // Get menu provider for configuration type and open configuration menu.
    final MenuProvider menuProvider =
        ConfigurationMenuHandler.getMenuProvider(
            configurationTypeAlias, easyNPC, menuType, screenData);
    final UUID npcUUID = easyNPC.getUUID();
    MenuManager.openMenu(npcUUID, menuProvider, serverPlayer, screenData.encode());
  }

  default void openEditorMenu(
      ServerPlayer serverPlayer,
      final MenuType<?> menuType,
      EasyNPC<?> easyNPC,
      UUID dialogId,
      UUID dialogButtonId,
      int pageIndex,
      final CompoundTag additionalSyncData) {
    openEasyNpcMenu(
        serverPlayer,
        menuType,
        easyNPC,
        dialogId,
        dialogButtonId,
        pageIndex,
        null,
        additionalSyncData);
  }

  default void openEditorMenu(
      final EditorType editorType,
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      UUID dialogId,
      int pageIndex) {
    openEditorMenu(editorType, serverPlayer, easyNPC, dialogId, null, pageIndex);
  }

  default void openEditorMenu(
      final EditorType editorType,
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      UUID dialogId,
      UUID dialogButtonId,
      int pageIndex) {
    // Add additional sync data for specific editor menu.
    CompoundTag additionalSyncData = new CompoundTag();
    switch (editorType) {
      case DIALOG, DIALOG_BUTTON, DIALOG_TEXT ->
          AdditionalScreenData.addDialogDataSet(additionalSyncData, easyNPC);
      default -> {
        // Do nothing
      }
    }

    // Open editor menu based on editor type.
    MenuType<?> menuType = getMenuTypeByEditorType(editorType);
    if (menuType != null) {
      this.openEditorMenu(
          serverPlayer, menuType, easyNPC, dialogId, dialogButtonId, pageIndex, additionalSyncData);
    } else {
      log.error("Unknown editor {} for {} from {}", editorType, easyNPC, serverPlayer);
    }
  }

  default void openDialogMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC, UUID dialogId, int pageIndex) {
    openDialogMenu(serverPlayer, getDialogMenuType(), easyNPC, dialogId, pageIndex);
  }

  default void openDialogMenu(
      ServerPlayer serverPlayer,
      MenuType<?> menuType,
      EasyNPC<?> easyNPC,
      UUID dialogId,
      int pageIndex) {
    // Add additional sync data for the dialog.
    CompoundTag additionalSyncData = new CompoundTag();
    AdditionalScreenData.addActionEventSet(additionalSyncData, easyNPC);
    AdditionalScreenData.addDialogDataSet(additionalSyncData, easyNPC);
    openEasyNpcMenu(
        serverPlayer,
        menuType,
        easyNPC,
        dialogId,
        null,
        pageIndex,
        easyNPC.getEntity().getName(),
        additionalSyncData);
  }

  MenuType<? extends ConfigurationMenu> getMenuTypeByConfigurationType(
      ConfigurationType configurationType);

  MenuType<?> getMenuTypeByEditorType(EditorType editorType);

  MenuType<?> getDialogMenuType();
}
