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

import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.MenuHandler;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import java.util.Map;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.phys.Vec3;

public class ConfigurationScreenTest {

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenMainConfigurationScreen(GameTestHelper helper) {

    // Get a mock player and spawn a humanoid NPC.
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC =
        GameTestHelpers.mockEasyNPC(helper, ModEntityType.HUMANOID.get(), new Vec3(2, 2, 2));

    // Prepare and open Dialog
    UUID dialogId =
        ConfigurationScreenTestHelper.mockOpenConfigurationScreen(
            serverPlayer,
            ConfigurationType.MAIN,
            easyNPC,
            ModMenuTypes.MAIN_CONFIGURATION_MENU.get());
    GameTestHelpers.assertNotNull(helper, "DialogId is null!", dialogId);

    // Check if dialog is open.
    GameTestHelpers.assertTrue(
        helper, "Dialog is not open!", serverPlayer.containerMenu instanceof ConfigurationMenu);

    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testAllConfigurationScreen(GameTestHelper helper) {

    // Get a mock player and spawn a humanoid NPC.
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC =
        GameTestHelpers.mockEasyNPC(helper, ModEntityType.HUMANOID.get(), new Vec3(2, 2, 2));

    // Test all known configuration screens
    for (Map.Entry<ConfigurationType, MenuType<? extends ConfigurationMenu>> entry :
        MenuHandler.configurationMenuMap().entrySet()) {

      // Close previous dialog
      if (serverPlayer.hasContainerOpen()) {
        serverPlayer.closeContainer();
      }

      // Prepare and open Dialog
      UUID dialogId =
          ConfigurationScreenTestHelper.mockOpenConfigurationScreen(
              serverPlayer, entry.getKey(), easyNPC, entry.getValue());
      GameTestHelpers.assertNotNull(helper, "DialogId is null!", dialogId);

      // Check if dialog is open.
      GameTestHelpers.assertTrue(
          helper,
          "Dialog " + entry.getValue() + " is not open!",
          serverPlayer.containerMenu instanceof ConfigurationMenu);
      GameTestHelpers.assertEquals(
          helper,
          "Wrong Dialog type! Expected: "
              + entry.getValue()
              + " but got: "
              + serverPlayer.containerMenu.getType(),
          entry.getValue(),
          serverPlayer.containerMenu.getType());

      helper.succeed();
    }
  }
}
