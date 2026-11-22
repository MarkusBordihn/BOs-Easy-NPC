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
import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.InventoryDataCapable;
import de.markusbordihn.easynpc.menu.dialog.DialogMenu;
import de.markusbordihn.easynpc.menu.dialog.DialogMenuHandler;
import de.markusbordihn.easynpc.menu.inventory.NPCInventoryMenu;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.inventory.MenuType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface MenuHandlerInterface {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  default void openDialogMenu(
      final ServerPlayer serverPlayer,
      final EasyNPC<?> easyNPC,
      final UUID dialogId,
      int pageIndex) {
    final MenuType<? extends DialogMenu> menuType = this.getDialogMenuType();
    if (menuType == null) {
      log.error("Unknown dialog for {} from {}", easyNPC, serverPlayer);
      return;
    }

    final ScreenData screenData =
        DialogMenuHandler.getScreenData(easyNPC, dialogId, pageIndex, serverPlayer);

    final MenuProvider menuProvider =
        DialogMenuHandler.getMenuProvider(easyNPC, menuType, screenData);
    final UUID npcUUID = easyNPC.getEntityUUID();
    MenuManager.openMenu(npcUUID, menuProvider, serverPlayer, screenData.encode());
  }

  default UUID openNPCInventoryMenu(final ServerPlayer serverPlayer, final EasyNPC<?> easyNPC) {
    if (serverPlayer.isSpectator()) {
      log.debug("Spectator {} cannot open the inventory of {}", serverPlayer, easyNPC);
      return null;
    }

    final MenuType<? extends NPCInventoryMenu> menuType = this.getNPCInventoryMenuType();
    final InventoryDataCapable<?> inventoryData = easyNPC.getEasyNPCInventoryData();
    if (menuType == null || inventoryData == null) {
      log.error("Unable to open the inventory of {} for {}", easyNPC, serverPlayer);
      return null;
    }

    final CompoundTag additionalData = new CompoundTag();
    AdditionalScreenData.addPlayerSlotCount(
        additionalData, inventoryData.getInventoryAttributes().playerSlotCount());
    final UUID npcUUID = easyNPC.getEntityUUID();
    final CompoundTag screenData =
        new ScreenData(npcUUID, null, null, null, null, 0, additionalData).encode();
    final MenuProvider menuProvider =
        new SimpleMenuProvider(
            (containerId, playerInventory, player) ->
                new NPCInventoryMenu(menuType, containerId, playerInventory, screenData),
            easyNPC.getEntity().getName());
    return MenuManager.openMenu(npcUUID, menuProvider, serverPlayer, screenData);
  }

  MenuType<? extends DialogMenu> getDialogMenuType();

  MenuType<? extends NPCInventoryMenu> getNPCInventoryMenuType();
}
