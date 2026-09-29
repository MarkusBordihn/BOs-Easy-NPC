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

import static org.junit.jupiter.api.Assumptions.assumeTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.By;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import java.io.IOException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class EditorScreenClientTest extends ClientTestBase {

  private static final String TEXT_FIELD_CLASS =
      "de.markusbordihn.easynpc.client.screen.components.TextField";
  private static final String NEW_FACTION_NAME = "clienttest";
  private static final String NEW_ACTION_COMMAND = "say clienttest";

  private static void openEditor(By button, String screenClassName) {
    assumeTrue(isEnabledWidget(button), button + " is not available on " + client.screenId());
    assertScreenOfClass(clickAndAwaitScreenChange(button), screenClassName);
  }

  private static void typeIntoTextField(String text, By enabledButton) {
    client.type(By.className(TEXT_FIELD_CLASS), text, true);
    client.await(Until.widgetEnabled(enabledButton));
  }

  private static void openNewDialogEditor() {
    summonTestNpc();
    openMainConfiguration();
    openSubPage("dialog", "advanced", "AdvancedDialogConfigurationScreen");
    openEditor(configurationButton("dialog.add"), "DialogEditorScreen");
  }

  private static void openNewActionDataEntryEditor() {
    summonTestNpc();
    openMainConfiguration();
    openSubPage("actions", "basic", "BasicActionConfigurationScreen");
    openEditor(
        By.translationKey("text.easy_npc.config.add_action", 0),
        "ActionDataEntryEditorContainerScreen");
  }

  private static void openConditionDataEditorOfNewDialog() {
    openNewDialogEditor();
    openEditor(configurationButton("add_condition"), "ConditionDataEditorContainerScreen");
  }

  private static void openFactionsEditor() {
    summonTestNpc();
    openMainConfiguration();
    openSubPage("attributes", "misc", "MiscAttributeConfigurationScreen");
    openEditor(configurationButton("new"), "FactionsEditorScreen");
  }

  @Test
  @DisplayName("Dialog editor renders for a new dialog")
  void dialogEditorRenders() throws IOException {
    openNewDialogEditor();

    captureScreen("dialog_editor");
  }

  @Test
  @DisplayName("Dialog text editor renders")
  void dialogTextEditorRenders() throws IOException {
    openNewDialogEditor();

    openEditor(configurationButton("dialog.edit_text"), "DialogTextEditorScreen");

    captureScreen("dialog_text_editor");
  }

  @Test
  @DisplayName("Dialog options editor renders")
  void dialogOptionsEditorRenders() throws IOException {
    openNewDialogEditor();

    openEditor(configurationButton("dialog.edit_options"), "DialogOptionsEditorScreen");

    captureScreen("dialog_options_editor");
  }

  @Test
  @DisplayName("Dialog button editor renders for a new button")
  void dialogButtonEditorRenders() throws IOException {
    openNewDialogEditor();

    openEditor(configurationButton("dialog.add_button"), "DialogButtonEditorScreen");

    captureScreen("dialog_button_editor");
  }

  @Test
  @DisplayName("Condition editor renders for a dialog")
  void conditionDataEditorRenders() throws IOException {
    openConditionDataEditorOfNewDialog();

    captureScreen("condition_data_editor");
  }

  @Test
  @DisplayName("Condition entry editor renders for a new condition")
  void conditionDataEntryEditorRenders() throws IOException {
    openConditionDataEditorOfNewDialog();

    openEditor(configurationButton("condition.add"), "ConditionDataEntryEditorContainerScreen");

    captureScreen("condition_data_entry_editor");
  }

  @Test
  @DisplayName("Action entry editor renders for a new action")
  void actionDataEntryEditorRenders() throws IOException {
    openNewActionDataEntryEditor();

    captureScreen("action_data_entry_editor");
  }

  @Test
  @DisplayName("Action editor renders after saving an action")
  void actionDataEditorRenders() throws IOException {
    openNewActionDataEntryEditor();
    typeIntoTextField(NEW_ACTION_COMMAND, configurationButton("save"));

    openEditor(configurationButton("save"), "ActionDataEditorContainerScreen");

    captureScreen("action_data_editor");
  }

  @Test
  @DisplayName("Factions editor renders")
  void factionsEditorRenders() throws IOException {
    openFactionsEditor();

    captureScreen("factions_editor");
  }

  @Test
  @DisplayName("Faction editor renders for a new faction")
  void factionEditorRenders() throws IOException {
    openFactionsEditor();
    typeIntoTextField(NEW_FACTION_NAME, configurationButton("add_faction"));

    openEditor(configurationButton("add_faction"), "FactionEditorScreen");

    captureScreen("faction_editor");
  }
}
