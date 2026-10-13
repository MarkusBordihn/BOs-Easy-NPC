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
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 * OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.clienttest;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.By;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DialogScreenClientTest extends ClientTestBase {

  private static final String TEST_TAG = "easynpc_clienttest";
  private static final String TEST_NPC_SELECTOR = "@e[tag=" + TEST_TAG + ",limit=1]";
  private static final String DIALOG_SCREEN_ID =
      "de.markusbordihn.easynpc.client.screen.dialog.dialogscreenwrapper";
  private static final String BANK_TELLER_PRESET =
      "easy_npc:default_preset/villager/bank_teller.npc.snbt";
  private static final String BANK_OBJECTIVE = "easy_npc_bank";
  private static final String BANK_TELLER_FIVE_EMERALDS = "5 Emeralds";

  @Test
  @DisplayName("Dialog button resolves @npc and closes the dialog on click")
  void dialogButtonResolvesNpcNameAndCloses() {
    client.runCommand(
        "summon easy_npc:humanoid ~ ~ ~3 {Tags:[\""
            + TEST_TAG
            + "\"],CustomName:\"Tester\",DialogData:{Type:\"STANDARD\",DialogDataSet:[{"
            + "Label:\"default\",Texts:[{Text:\"Hi\"}],Buttons:[{Name:\"Talk to @npc\","
            + "Label:\"talk\",Actions:[{Type:\"CLOSE_DIALOG\"}]}]}]}}");
    client.runCommand(
        "easy_npc dialog open " + TEST_NPC_SELECTOR + " @s",
        Until.screen(DIALOG_SCREEN_ID),
        Until.widgetPresent(By.text("Talk to Tester")));

    client.saveScreenshot("dialog");
    client.click(By.text("Talk to Tester"), Until.NO_SCREEN_ID);
  }

  @Test
  @DisplayName(
      "Bank Teller preset locks only the withdraw button, not the deposit button of the same label")
  void equalButtonLabelsLockPerDialog() {
    client.runCommand("clear @s minecraft:emerald");
    client.runCommand("give @s minecraft:emerald 5");
    client.runCommand("scoreboard objectives add " + BANK_OBJECTIVE + " dummy");
    client.runCommand("scoreboard players set @s " + BANK_OBJECTIVE + " 0");
    client.runCommand("easy_npc preset import_new default " + BANK_TELLER_PRESET + " ~ ~ ~3");
    client.runCommand("tag @e[type=easy_npc:villager,sort=nearest,limit=1] add " + TEST_TAG);

    client.runCommand(
        "easy_npc dialog open " + TEST_NPC_SELECTOR + " @s deposit",
        Until.screen(DIALOG_SCREEN_ID),
        Until.widgetPresent(By.text(BANK_TELLER_FIVE_EMERALDS)));
    client.saveScreenshot("dialog_deposit");
    assertTrue(
        isEnabledWidget(By.text(BANK_TELLER_FIVE_EMERALDS)),
        "The deposit button is locked although the player carries 5 emeralds");

    client.closeScreen();
    client.await(Until.noScreen());
    client.runCommand(
        "easy_npc dialog open " + TEST_NPC_SELECTOR + " @s withdraw",
        Until.screen(DIALOG_SCREEN_ID),
        Until.widgetPresent(By.text(BANK_TELLER_FIVE_EMERALDS)));
    client.saveScreenshot("dialog_withdraw");
    assertFalse(
        isEnabledWidget(By.text(BANK_TELLER_FIVE_EMERALDS)),
        "The withdraw button is not locked although the balance is 0");

    client.closeScreen();
    client.await(Until.noScreen());
  }

  @AfterEach
  void removeTestNpc() {
    client.runCommand("kill @e[tag=" + TEST_TAG + "]");
    client.runCommand("clear @s minecraft:emerald");
    client.runCommand("scoreboard objectives remove " + BANK_OBJECTIVE);
  }
}
