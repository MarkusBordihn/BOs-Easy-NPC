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
import de.markusbordihn.easynpc.data.screen.ScreenContainerData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.testing.TestMenu;
import de.markusbordihn.easynpc.network.ClientNetworkMessageHandlerInterface;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import java.util.UUID;
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

  private static void syncServerSideData(UUID npcUUID, ServerPlayer serverPlayer) {
    ClientNetworkMessageHandlerInterface networkMessageHandler =
        NetworkMessageHandlerManager.getClientHandler();
    if (networkMessageHandler != null) {
      log.debug("Sync server side data for {}", serverPlayer);
      networkMessageHandler.syncActionEventSet(npcUUID, serverPlayer);
      networkMessageHandler.syncDialogDataSet(npcUUID, serverPlayer);
      networkMessageHandler.syncObjectiveDataSet(npcUUID, serverPlayer);
    }
  }

  private static void openEasyNpcMenu(
      ServerPlayer serverPlayer,
      MenuType<?> menuType,
      EasyNPC<?> easyNPC,
      UUID dialogId,
      int pageIndex) {
    openEasyNpcMenu(
        serverPlayer, menuType, easyNPC, dialogId, pageIndex, easyNPC.getEntity().getName());
  }

  private static void openEasyNpcMenu(
      final ServerPlayer serverPlayer,
      final MenuType<?> menuType,
      final EasyNPC<?> easyNPC,
      final UUID dialogId,
      final int pageIndex,
      final Component menuName) {
    syncServerSideData(easyNPC.getUUID(), serverPlayer);
    final UUID npcUUID = easyNPC.getUUID();
    final Component displayName = menuName != null ? menuName : easyNPC.getEntity().getName();
    final ScreenContainerData screenContainerData =
        new ScreenContainerData(npcUUID, dialogId, pageIndex);
    final MenuProvider menuProvider =
        new MenuProvider() {
          @Override
          public AbstractContainerMenu createMenu(
              int containerId, Inventory playerInventory, Player player) {
            return new EasyNPCMenu(menuType, containerId, playerInventory, screenContainerData);
          }

          @Override
          public Component getDisplayName() {
            return displayName;
          }
        };
    serverPlayer.openMenu(menuProvider);
  }

  void openConfigurationMenu(
      ConfigurationType configurationType,
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      int pageIndex);

  void openDialogEditorMenu(
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      UUID dialogId,
      ConfigurationType formerConfigurationType,
      int pageIndex);

  void openDialogButtonEditorMenu(
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      UUID dialogId,
      UUID dialogButtonId,
      ConfigurationType formerConfigurationType,
      int pageIndex);

  void openDialogTextEditorMenu(
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      UUID dialogId,
      ConfigurationType formerConfigurationType,
      int pageIndex);

  default void openConfigurationMenu(
      ConfigurationType configurationType, ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    openConfigurationMenu(configurationType, serverPlayer, easyNPC, 0);
  }

  void openDialogMenu(ServerPlayer serverPlayer, EasyNPC<?> easyNPC, UUID dialogId, int pageIndex);

  default void openDialogMenu(
      ServerPlayer serverPlayer,
      MenuType<?> menuType,
      EasyNPC<?> easyNPC,
      UUID dialogId,
      int pageIndex) {
    openEasyNpcMenu(serverPlayer, menuType, easyNPC, dialogId, pageIndex);
  }

  void openTestMenu(ServerPlayer serverPlayer, UUID npcUUID);

  default void openTestMenu(ServerPlayer serverPlayer, MenuType<?> menuType, UUID npcUUID) {
    syncServerSideData(npcUUID, serverPlayer);
    ScreenContainerData screenContainerData = new ScreenContainerData(npcUUID, UUID.randomUUID());
    log.info(
        "Open test menu for {}({}) and {}",
        npcUUID,
        screenContainerData.getNpcUUID(),
        screenContainerData.getDialogUUID());
    MenuProvider menuProvider =
        new MenuProvider() {
          @Override
          public AbstractContainerMenu createMenu(
              int containerId, Inventory playerInventory, Player player) {
            return new TestMenu(menuType, containerId, playerInventory, screenContainerData);
          }

          @Override
          public Component getDisplayName() {
            return serverPlayer.getName();
          }
        };
    serverPlayer.openMenu(menuProvider);
  }
}
