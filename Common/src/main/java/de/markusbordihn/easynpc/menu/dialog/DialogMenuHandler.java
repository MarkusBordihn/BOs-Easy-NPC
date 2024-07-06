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

package de.markusbordihn.easynpc.menu.dialog;

import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.editor.EditorMenu;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.MenuType;

public class DialogMenuHandler {

  public static MenuProvider getMenuProvider(
      final EasyNPC<?> easyNPC,
      final MenuType<? extends DialogMenu> menuType,
      ScreenData screenData) {
    final Component displayName = easyNPC.getEntity().getName();

    // Default editor menu
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
    };
  }

  public static ScreenData getScreenData(
      final EasyNPC<?> easyNPC, final UUID dialogId, final int pageIndex) {

    // Get basic data for configuration menu.
    final UUID npcUUID = easyNPC.getUUID();

    // Additional data for specific configuration menu.
    final CompoundTag additionalSyncData = new CompoundTag();
    AdditionalScreenData.addActionEventSet(additionalSyncData, easyNPC);
    AdditionalScreenData.addDialogDataSet(additionalSyncData, easyNPC);

    return new ScreenData(npcUUID, dialogId, null, null, pageIndex, additionalSyncData);
  }
}
