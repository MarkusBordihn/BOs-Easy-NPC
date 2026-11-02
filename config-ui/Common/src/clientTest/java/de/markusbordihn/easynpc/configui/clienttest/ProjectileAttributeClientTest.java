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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.By;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.JsonValue;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ProjectileAttributeClientTest extends ClientTestBase {

  private static final String COMBAT_ATTRIBUTE_SCREEN = "CombatAttributeConfigurationScreen";
  private static final String PROJECTILE_ATTRIBUTE_SCREEN =
      "ProjectileAttributeConfigurationScreen";
  private static final By PROJECTILE_SETTINGS_BUTTON = configurationButton("projectile_settings");
  private static final By PROJECTILE_TYPE_SELECT_BOX =
      By.className("de.markusbordihn.easynpc.configui.client.screen.components.SelectBox")
          .withIndex(0);
  private static final By PROJECTILE_ITEM_TEXT_FIELD =
      By.className("de.markusbordihn.easynpc.client.screen.components.TextField").withIndex(0);
  private static final By PROJECTILE_ITEM_SAVE_BUTTON =
      By.className("de.markusbordihn.easynpc.configui.client.screen.components.SaveButton")
          .withIndex(0);
  private static final By PROJECTILE_PAGE_BUTTON =
      configurationButton("projectile_page.projectile");
  private static final By EFFECTS_PAGE_BUTTON = configurationButton("projectile_page.effects");
  private static final String ITEM_PROJECTILE_SEARCH_TEXT = "Custom";
  private static final int SELECT_BOX_ENTRY_HEIGHT = 14;
  private static final String SAVED_PROJECTILE_ITEM = "minecraft:diamond_sword";
  private static final String UNSAVED_PROJECTILE_ITEM = "minecraft:diamond";
  private static final String INVALID_PROJECTILE_ITEM = "minecraft:unknown_item";

  private static String openProjectileAttributes(UUID testNpc) {
    openMainConfiguration(testNpc);
    openSubPage("attributes", "combat", COMBAT_ATTRIBUTE_SCREEN);
    String screenId = clickAndAwaitScreenChange(PROJECTILE_SETTINGS_BUTTON);
    assertScreenOfClass(screenId, PROJECTILE_ATTRIBUTE_SCREEN);
    return screenId;
  }

  private static void reopen(UUID testNpc) {
    client.closeScreen();
    client.await(Until.noScreen());
    openProjectileAttributes(testNpc);
  }

  private static void selectItemProjectileType() {
    JsonValue selectBoxBounds = client.widget(PROJECTILE_TYPE_SELECT_BOX).get("bounds");
    client.click(PROJECTILE_TYPE_SELECT_BOX);
    client.input().text(Parameters.of().with("text", ITEM_PROJECTILE_SEARCH_TEXT));
    double firstEntryY =
        selectBoxBounds.number("y")
            + selectBoxBounds.number("height")
            + 1
            + SELECT_BOX_ENTRY_HEIGHT
            + SELECT_BOX_ENTRY_HEIGHT / 2.0;
    client
        .input()
        .mouseButton(
            Parameters.of()
                .with("x", selectBoxBounds.number("x") + 10)
                .with("y", firstEntryY));
    client.await(Until.settled(5));
  }

  @Test
  @DisplayName("Custom item projectile keeps only the saved item ID after reopening")
  void customItemProjectileSurvivesReopen() {
    UUID testNpc = spawnTestNpc();
    openProjectileAttributes(testNpc);
    assertFalse(
        isEnabledWidget(PROJECTILE_ITEM_TEXT_FIELD),
        "Item ID is editable although the projectile type is not a custom item");
    assertFalse(isEnabledWidget(PROJECTILE_ITEM_SAVE_BUTTON), "Item ID save button is active");
    client.saveScreenshot("default");

    selectItemProjectileType();
    assertTrue(
        isEnabledWidget(PROJECTILE_ITEM_TEXT_FIELD),
        "Item ID stays disabled after selecting the custom item projectile");
    client.type(PROJECTILE_ITEM_TEXT_FIELD, INVALID_PROJECTILE_ITEM, true);
    assertFalse(
        isEnabledWidget(PROJECTILE_ITEM_SAVE_BUTTON), "Invalid item ID can be saved");
    client.saveScreenshot("invalid_item");

    client.type(PROJECTILE_ITEM_TEXT_FIELD, SAVED_PROJECTILE_ITEM, true);
    assertTrue(
        isEnabledWidget(PROJECTILE_ITEM_SAVE_BUTTON), "Changed valid item ID cannot be saved");
    client.saveScreenshot("changed_item");
    client.click(PROJECTILE_ITEM_SAVE_BUTTON);
    client.await(Until.settled(5));
    assertFalse(
        isEnabledWidget(PROJECTILE_ITEM_SAVE_BUTTON), "Save button stays active after saving");
    client.saveScreenshot("saved_item");

    client.type(PROJECTILE_ITEM_TEXT_FIELD, UNSAVED_PROJECTILE_ITEM, true);
    reopen(testNpc);

    assertTrue(
        isEnabledWidget(PROJECTILE_ITEM_TEXT_FIELD),
        "Custom item projectile type was not kept after reopening");
    assertEquals(SAVED_PROJECTILE_ITEM, client.widget(PROJECTILE_ITEM_TEXT_FIELD).string("value"));
    client.saveScreenshot("reopened");
  }

  @Test
  @DisplayName("Switching to the effects page keeps an unsaved item ID")
  void pageSwitchKeepsUnsavedItem() {
    UUID testNpc = spawnTestNpc();
    openProjectileAttributes(testNpc);
    selectItemProjectileType();
    client.type(PROJECTILE_ITEM_TEXT_FIELD, SAVED_PROJECTILE_ITEM, true);

    client.click(EFFECTS_PAGE_BUTTON);
    client.await(Until.settled(5));
    client.saveScreenshot("effects_page");
    client.click(PROJECTILE_PAGE_BUTTON);
    client.await(Until.settled(5));

    assertEquals(SAVED_PROJECTILE_ITEM, client.widget(PROJECTILE_ITEM_TEXT_FIELD).string("value"));
    assertTrue(
        isEnabledWidget(PROJECTILE_ITEM_SAVE_BUTTON),
        "Unsaved item ID cannot be saved after switching pages");
  }
}
