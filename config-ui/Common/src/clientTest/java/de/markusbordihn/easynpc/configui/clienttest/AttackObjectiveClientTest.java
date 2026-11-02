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

package de.markusbordihn.easynpc.configui.clienttest;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.By;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class AttackObjectiveClientTest extends ClientTestBase {

  private static final String ATTACK_OBJECTIVE_SCREEN = "AttackObjectiveConfigurationScreen";
  private static final String ATTACK_STYLE_BUTTON_CLASS =
      "de.markusbordihn.easynpc.configui.client.screen.components.ItemToggleButton";
  private static final By MELEE_ATTACK_BUTTON =
      By.className(ATTACK_STYLE_BUTTON_CLASS).withIndex(0);
  private static final By ZOMBIE_ATTACK_BUTTON =
      By.className(ATTACK_STYLE_BUTTON_CLASS).withIndex(1);
  private static final By BOW_ATTACK_BUTTON = By.className(ATTACK_STYLE_BUTTON_CLASS).withIndex(2);
  private static final By ATTACK_MOB_CHECKBOX = configurationButton("attack_target_mob");
  private static final By SPARE_CREEPER_CHECKBOX =
      configurationButton("attack_target_spare_creeper");

  private static void openAttackObjectives(UUID testNpc) {
    openMainConfiguration(testNpc);
    openSubPage("objective", "attack", ATTACK_OBJECTIVE_SCREEN);
  }

  private static void clickAndSettle(By locator) {
    client.click(locator);
    client.await(Until.settled(5));
  }

  private static void awaitAttackListMessage(UUID testNpc, String expectedMessage) {
    runCommandAndAwaitFeedback("easy_npc objective " + testNpc + " list attack", expectedMessage);
  }

  @Test
  @DisplayName("Zombie attack replaces melee attack while bow attack combines with it")
  void meleeAndZombieAttackExcludeEachOther() {
    UUID testNpc = spawnTestNpc();
    openAttackObjectives(testNpc);

    clickAndSettle(MELEE_ATTACK_BUTTON);
    clickAndSettle(ZOMBIE_ATTACK_BUTTON);
    clickAndSettle(BOW_ATTACK_BUTTON);
    client.saveScreenshot("zombie_and_bow_without_weapon");

    awaitAttackListMessage(testNpc, ": bow, zombie");
    openAttackObjectives(testNpc);
    client.saveScreenshot("reopened");
  }

  @Test
  @DisplayName("Hostile mobs except creepers is saved as a single attack target")
  void spareCreeperIsSavedAsSingleTarget() {
    UUID testNpc = spawnTestNpc();
    openAttackObjectives(testNpc);
    assertFalse(
        isEnabledWidget(SPARE_CREEPER_CHECKBOX),
        "Except creepers is enabled although hostile mobs are not attacked");

    clickAndSettle(ATTACK_MOB_CHECKBOX);
    assertTrue(
        isEnabledWidget(SPARE_CREEPER_CHECKBOX),
        "Except creepers stays disabled after choosing hostile mobs");
    clickAndSettle(SPARE_CREEPER_CHECKBOX);

    awaitAttackListMessage(testNpc, "> mob_without_creeper");
    openAttackObjectives(testNpc);
    assertTrue(
        isEnabledWidget(SPARE_CREEPER_CHECKBOX),
        "Hostile mobs were not shown as attacked after reopening");
    client.saveScreenshot("reopened");
  }
}
