/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.clienttest;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.TestArea;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.api.Player;
import java.util.Locale;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class InteractionClientTest extends ClientTestBase {

  private static final String TEST_TAG = "easynpc_clienttest";
  private static final String DIALOG_SCREEN_ID =
      "de.markusbordihn.easynpc.client.screen.dialog.dialogscreenwrapper";
  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final TestArea PLATFORM = TestArea.between(-2, PLATFORM_Y, -1, 2, PLATFORM_Y, 4);
  private static final String NPC_SUMMON_POSITION = "0.5 " + VIEW_Y + " 2.5";
  private static final String DIALOG_DATA =
      "DialogData:{Type:\"STANDARD\",DialogDataSet:[{Label:\"default\",Texts:[{Text:\"Hi\"}]}]}";
  private static final String INTERACTION_ACTION_DATA =
      "ActionData:{ActionEventSet:{ON_INTERACTION:[{Type:\"OPEN_DEFAULT_DIALOG\"}]}}";
  private static final int BOW_SLOT = 0;
  private static final int ARROW_SLOT = 9;
  private static final long BOW_DRAW_TICKS = 25;

  private String previousGameMode;

  @BeforeEach
  void faceTheNpcPosition() {
    awaitWorldSetupCapability();
    this.previousGameMode =
        client.state().playerGet().player().map(Player::gameMode).orElse("creative");
    client.fill(PLATFORM, "minecraft:stone");
    client.teleport(0.5, VIEW_Y, 0.5, 0, 0);
  }

  @Test
  @DisplayName("Right-click with a bow opens the dialog of an NPC with an interaction action")
  void bowOpensDialogWhenNpcHasInteractionAction() {
    summonNpc(INTERACTION_ACTION_DATA + "," + DIALOG_DATA);
    holdInHotbar("minecraft:bow");

    rightMouseButton("click");

    client.await(Until.screen(DIALOG_SCREEN_ID));
    client.saveScreenshot("bow_opens_dialog");
    client.closeScreen();
    client.await(Until.noScreen());
  }

  @Test
  @DisplayName("Right-click with a bow shoots through an NPC without an interaction action")
  void bowShootsWhenNpcHasNoInteractionAction() {
    client.runCommand("gamemode survival @s");
    summonNpc(DIALOG_DATA);
    holdInHotbar("minecraft:bow");
    client.setItemInSlot(ARROW_SLOT, "minecraft:arrow", 2);

    rightMouseButton("press");
    client.await(Until.ticksElapsed(BOW_DRAW_TICKS));
    rightMouseButton("release");

    client.await(Until.itemInSlot(ARROW_SLOT, "minecraft:arrow", 1));
  }

  @AfterEach
  void restorePlayer() {
    client.control().releaseAll();
    client.runCommand("kill @e[tag=" + TEST_TAG + "]");
    client.runCommand("kill @e[type=minecraft:arrow]");
    client.clearInventory();
    client.runCommand("gamemode " + this.previousGameMode.toLowerCase(Locale.ROOT) + " @s");
  }

  private static void summonNpc(String npcData) {
    client.runCommand(
        "summon easy_npc:humanoid "
            + NPC_SUMMON_POSITION
            + " {Tags:[\""
            + TEST_TAG
            + "\"],CustomName:\"Tester\","
            + npcData
            + "}");
    client.await(Until.entityPresent(Parameters.of("type", "easy_npc:humanoid")));
  }

  private static void holdInHotbar(String item) {
    client.setItemInSlot(BOW_SLOT, item);
    client.control().hotbarSelect(Parameters.of().with("slot", BOW_SLOT));
    client.await(Until.hotbarSlot(BOW_SLOT));
  }

  private static void rightMouseButton(String action) {
    client.input().mouseButton(Parameters.of("button", "right").with("action", action));
  }
}
