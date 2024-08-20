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

package de.markusbordihn.easynpc.menu.editor;

import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.MenuType;

public class EditorMenuHandler {

  private EditorMenuHandler() {}

  public static MenuProvider getMenuProvider(
      final EditorType editorType,
      final EasyNPC<?> easyNPC,
      final MenuType<? extends EditorMenu> menuType,
      ScreenData screenData) {
    final Component displayName = editorType.getEditorTitle(easyNPC);

    return new MenuProvider() {
      @Override
      public AbstractContainerMenu createMenu(
          int containerId, Inventory playerInventory, Player player) {
        return new EditorMenu(menuType, containerId, playerInventory, screenData.encode());
      }

      @Override
      public Component getDisplayName() {
        return displayName;
      }

      @Override
      public String toString() {
        return "EditorMenuHandler{"
            + "editorType="
            + editorType
            + ", easyNPC="
            + easyNPC
            + ", screenData="
            + screenData
            + '}';
      }
    };
  }

  public static ScreenData getScreenData(
      final EditorType editorType,
      final EasyNPC<?> easyNPC,
      final UUID dialogId,
      final UUID dialogButtonId,
      final UUID actionDataEntryId,
      final int pageIndex,
      CompoundTag additionalSyncData) {

    // Get basic data for configuration menu.
    final UUID npcUUID = easyNPC.getUUID();

    // Additional data for specific configuration menu.
    switch (editorType) {
      case DIALOG, DIALOG_BUTTON, DIALOG_TEXT ->
          AdditionalScreenData.addDialogDataSet(additionalSyncData, easyNPC);
      case ACTION_DATA, ACTION_DATA_ENTRY -> {
        AdditionalScreenData.addActionEventSet(additionalSyncData, easyNPC);
        AdditionalScreenData.addDialogDataSet(additionalSyncData, easyNPC);
      }
      default -> {
        // Do nothing
      }
    }
    return new ScreenData(
        npcUUID, dialogId, dialogButtonId, actionDataEntryId, pageIndex, additionalSyncData);
  }
}
