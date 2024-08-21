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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.menu.dialog.DialogMenu;
import de.markusbordihn.easynpc.menu.dialog.DialogMenuHandler;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.phys.Vec3;

public class DialogScreenTestHelper {

  private DialogScreenTestHelper() {}

  public static UUID mockOpenDialog(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC, MenuType<? extends DialogMenu> menuType) {

    // Define the menu provider and open the menu.
    MenuProvider menuProvider =
        DialogMenuHandler.getMenuProvider(
            easyNPC,
            menuType,
            new ScreenData(
                easyNPC.getUUID(),
                easyNPC.getEasyNPCDialogData().getDialogDataSet().getDefaultDialogId()));
    UUID menuId = MenuManager.registerMenu(easyNPC.getUUID(), menuProvider, serverPlayer);
    MenuManager.openMenu(menuId, serverPlayer);
    return menuId;
  }

  public static void testDialogScreen(
      GameTestHelper helper,
      DialogDataSet dialogDataSet,
      EntityType<? extends PathfinderMob> npcEntityType,
      MenuType<? extends DialogMenu> menuType) {
    // Get a mock player and spawn a humanoid NPC.
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, npcEntityType, new Vec3(2, 2, 2));

    // Close previous dialog, if any.
    if (serverPlayer.hasContainerOpen()) {
      serverPlayer.closeContainer();
    }

    // Add dialog data to NPC
    GameTestHelpers.assertNotNull(helper, "DialogDataSet is null!", dialogDataSet);
    easyNPC.getEasyNPCDialogData().setDialogDataSet(dialogDataSet);
    GameTestHelpers.assertNotNull(helper, "DialogData is null!", easyNPC.getEasyNPCDialogData());

    // Open Dialog
    UUID dialogId = mockOpenDialog(serverPlayer, easyNPC, menuType);
    GameTestHelpers.assertNotNull(helper, "DialogId is null!", dialogId);

    // Check if dialog is open.
    GameTestHelpers.assertTrue(
        helper,
        "Dialog Screen " + menuType + " is not open!",
        serverPlayer.containerMenu instanceof DialogMenu);
    GameTestHelpers.assertEquals(
        helper,
        "Wrong Dialog type! Expected: "
            + menuType
            + " but got: "
            + serverPlayer.containerMenu.getType(),
        menuType,
        serverPlayer.containerMenu.getType());
  }
}
