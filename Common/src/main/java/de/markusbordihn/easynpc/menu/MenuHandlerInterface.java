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
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationTypeHelper;
import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenuHandler;
import de.markusbordihn.easynpc.menu.dialog.DialogMenu;
import de.markusbordihn.easynpc.menu.dialog.DialogMenuHandler;
import de.markusbordihn.easynpc.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.menu.editor.EditorMenuHandler;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.inventory.MenuType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface MenuHandlerInterface {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  default void openConfigurationMenu(
      final ConfigurationType configurationType,
      final ServerPlayer serverPlayer,
      final EasyNPC<?> easyNPC,
      final int pageIndex) {

    // Handle configuration type alias.
    final ConfigurationType configurationTypeAlias =
        ConfigurationTypeHelper.resolveConfigurationTypeAlias(configurationType, easyNPC);

    // Get menu type for configuration type.
    final MenuType<? extends ConfigurationMenu> menuType =
        getMenuTypeByConfigurationType(configurationTypeAlias);
    if (menuType == null) {
      log.error(
          "Unknown configuration {} for {} from {}", configurationTypeAlias, easyNPC, serverPlayer);
      return;
    }

    // Additional data for specific configuration menu.
    final ScreenData screenData =
        ConfigurationMenuHandler.getScreenData(
            configurationTypeAlias, easyNPC, serverPlayer, pageIndex);

    // Get menu provider for configuration type and open configuration menu.
    final MenuProvider menuProvider =
        ConfigurationMenuHandler.getMenuProvider(
            configurationTypeAlias, easyNPC, menuType, screenData);
    final UUID npcUUID = easyNPC.getUUID();
    MenuManager.openMenu(npcUUID, menuProvider, serverPlayer, screenData.encode());
  }

  default void openEditorMenu(
      final EditorType editorType,
      final ServerPlayer serverPlayer,
      final EasyNPC<?> easyNPC,
      final UUID dialogId,
      final UUID dialogButtonId,
      final UUID actionDataEntryId,
      final ActionEventType actionEventType,
      final ConfigurationType configurationType,
      final EditorType formerEditorType,
      final int pageIndex) {
    CompoundTag additionalSyncData = new CompoundTag();
    AdditionalScreenData.addActionEventType(additionalSyncData, actionEventType);
    AdditionalScreenData.addConfigurationType(additionalSyncData, configurationType);
    AdditionalScreenData.addEditorType(additionalSyncData, formerEditorType);
    openEditorMenu(
        editorType,
        serverPlayer,
        easyNPC,
        dialogId,
        dialogButtonId,
        actionDataEntryId,
        pageIndex,
        additionalSyncData);
  }

  default void openEditorMenu(
      final EditorType editorType,
      final ServerPlayer serverPlayer,
      final EasyNPC<?> easyNPC,
      final UUID dialogId,
      final int pageIndex) {
    openEditorMenu(
        editorType, serverPlayer, easyNPC, dialogId, null, null, pageIndex, new CompoundTag());
  }

  default void openEditorMenu(
      final EditorType editorType,
      final ServerPlayer serverPlayer,
      final EasyNPC<?> easyNPC,
      final UUID dialogId,
      final UUID dialogButtonId,
      final int pageIndex) {
    openEditorMenu(
        editorType,
        serverPlayer,
        easyNPC,
        dialogId,
        dialogButtonId,
        null,
        pageIndex,
        new CompoundTag());
  }

  default void openEditorMenu(
      final EditorType editorType,
      final ServerPlayer serverPlayer,
      final EasyNPC<?> easyNPC,
      final UUID dialogId,
      final UUID dialogButtonId,
      final UUID actionDataEntryId,
      final int pageIndex,
      CompoundTag additionalSyncData) {

    // Get menu type for configuration type.
    final MenuType<? extends EditorMenu> menuType = getMenuTypeByEditorType(editorType);
    if (menuType == null) {
      log.error("Unknown editor {} for {} from {}", editorType, easyNPC, serverPlayer);
      return;
    }

    // Additional data for specific configuration menu.
    final ScreenData screenData =
        EditorMenuHandler.getScreenData(
            editorType,
            easyNPC,
            dialogId,
            dialogButtonId,
            actionDataEntryId,
            pageIndex,
            additionalSyncData);

    // Get menu provider for configuration type and open configuration menu.
    final MenuProvider menuProvider =
        EditorMenuHandler.getMenuProvider(editorType, easyNPC, menuType, screenData);
    final UUID npcUUID = easyNPC.getUUID();
    MenuManager.openMenu(npcUUID, menuProvider, serverPlayer, screenData.encode());
  }

  default void openDialogMenu(
      final ServerPlayer serverPlayer,
      final EasyNPC<?> easyNPC,
      final UUID dialogId,
      int pageIndex) {

    // Get menu type for configuration type.
    final MenuType<? extends DialogMenu> menuType = getDialogMenuType();
    if (menuType == null) {
      log.error("Unknown dialog {} for {} from {}", menuType, easyNPC, serverPlayer);
      return;
    }

    // Additional data for specific configuration menu.
    final ScreenData screenData = DialogMenuHandler.getScreenData(easyNPC, dialogId, pageIndex);

    // Get menu provider for configuration type and open configuration menu.
    final MenuProvider menuProvider =
        DialogMenuHandler.getMenuProvider(easyNPC, menuType, screenData);
    final UUID npcUUID = easyNPC.getUUID();
    MenuManager.openMenu(npcUUID, menuProvider, serverPlayer, screenData.encode());
  }

  MenuType<? extends ConfigurationMenu> getMenuTypeByConfigurationType(
      ConfigurationType configurationType);

  MenuType<? extends EditorMenu> getMenuTypeByEditorType(EditorType editorType);

  MenuType<? extends DialogMenu> getDialogMenuType();
}
