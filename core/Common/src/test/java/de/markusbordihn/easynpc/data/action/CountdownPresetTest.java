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

package de.markusbordihn.easynpc.data.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.preset.PresetData;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.TagParser;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class CountdownPresetTest {

  private static final Path COUNTDOWN_PRESET =
      Path.of(
          "src",
          "main",
          "resources",
          "data",
          "easy_npc",
          "default_preset",
          "humanoid",
          "countdown_timekeeper.npc.snbt");

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private CompoundTag loadEntityData() throws Exception {
    CompoundTag preset =
        TagParser.parseTag(Files.readString(COUNTDOWN_PRESET, StandardCharsets.UTF_8));
    return preset.getCompound(PresetData.DATA_TAG);
  }

  private List<ActionDataEntry> loadCountdownActions() throws Exception {
    DialogDataSet dialogDataSet =
        new DialogDataSet(this.loadEntityData().getCompound("DialogData"));
    DialogDataEntry dialogDataEntry = dialogDataSet.getDialog("default");
    for (DialogButtonEntry dialogButtonEntry : dialogDataEntry.getDialogButtons()) {
      if ("button_countdown".equals(dialogButtonEntry.label())) {
        return dialogButtonEntry.actionDataSet().getOrderedEntries();
      }
    }

    return List.of();
  }

  @Test
  @DisplayName("The countdown preset is shipped and readable")
  void testCountdownPresetIsShipped() {
    assertTrue(
        Files.isRegularFile(COUNTDOWN_PRESET),
        "The countdown preset " + COUNTDOWN_PRESET.toAbsolutePath() + " must exist");
  }

  @Test
  @DisplayName("The countdown button keeps every wait action of its chain")
  void testCountdownKeepsEveryWaitAction() throws Exception {
    List<ActionDataEntry> actionDataEntries = this.loadCountdownActions();
    assertEquals(ActionDataType.CLOSE_DIALOG, actionDataEntries.get(0).actionDataType());

    List<String> counted = new ArrayList<>();
    int waitActions = 0;
    for (ActionDataEntry actionDataEntry : actionDataEntries) {
      if (actionDataEntry.actionDataType() == ActionDataType.WAIT) {
        assertTrue(
            WaitDuration.parse(actionDataEntry.command()).isValid(),
            "Wait duration " + actionDataEntry.command() + " must be readable");
        waitActions++;
      } else if (actionDataEntry.actionDataType() == ActionDataType.MESSAGE) {
        counted.addAll(actionDataEntry.messageActionData().texts());
      }
    }

    assertEquals(10, waitActions);
    assertTrue(counted.contains("One."));
    assertTrue(counted.contains("<gold>Ten!</gold>"));
    assertEquals(
        ActionDataType.MESSAGE,
        actionDataEntries.get(actionDataEntries.size() - 1).actionDataType());
  }

  @Test
  @DisplayName("The idle lines of the countdown preset run in order")
  void testIdleLinesRunInOrder() throws Exception {
    ActionDataSet idleLines =
        new ActionEventSet(this.loadEntityData().getCompound("ActionData"))
            .getActionEvents(ActionEventType.ON_INTERVAL_VERY_LONG);

    List<ActionDataEntry> actionDataEntries = idleLines.getOrderedEntries();
    assertEquals(3, actionDataEntries.size());
    assertEquals(ActionDataType.MESSAGE, actionDataEntries.get(0).actionDataType());
    assertEquals(ActionDataType.WAIT, actionDataEntries.get(1).actionDataType());
    assertEquals(ActionDataType.MESSAGE, actionDataEntries.get(2).actionDataType());
    assertTrue(idleLines.hasActionDataType(ActionDataType.WAIT));
  }
}
