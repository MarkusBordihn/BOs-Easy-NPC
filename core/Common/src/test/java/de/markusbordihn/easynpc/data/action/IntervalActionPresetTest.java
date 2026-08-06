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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.preset.PresetData;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;
import java.util.stream.Collectors;
import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.TagParser;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class IntervalActionPresetTest {

  private static final Path COMPANION_PRESET =
      Path.of(
          "src",
          "main",
          "resources",
          "data",
          "easy_npc",
          "default_preset",
          "humanoid",
          "wandering_companion.npc.snbt");

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private ActionEventSet loadActionEventSet() throws Exception {
    CompoundTag preset =
        TagParser.parseCompoundFully(Files.readString(COMPANION_PRESET, StandardCharsets.UTF_8));
    CompoundTag entityData = preset.getCompoundOrEmpty(PresetData.DATA_TAG);
    return new ActionEventSet(entityData.getCompoundOrEmpty("ActionData"));
  }

  @Test
  @DisplayName("The companion preset is shipped and readable")
  void testCompanionPresetIsShipped() throws IOException {
    assertTrue(
        Files.isRegularFile(COMPANION_PRESET),
        "The companion preset " + COMPANION_PRESET.toAbsolutePath() + " must exist");
  }

  @Test
  @DisplayName("The companion preset defines idle lines without a single command action")
  void testCompanionPresetUsesMessageActionsOnly() throws Exception {
    ActionEventSet actionEventSet = this.loadActionEventSet();

    assertTrue(actionEventSet.hasActionEvent(ActionEventType.ON_INTERVAL_NORMAL));
    assertTrue(actionEventSet.hasActionEvent(ActionEventType.ON_INTERVAL_VERY_LONG));
    assertTrue(actionEventSet.hasActionEvent(ActionEventType.ON_WEATHER_CHANGE));
    assertTrue(actionEventSet.hasActionEvent(ActionEventType.ON_OWNER_LOGIN));

    for (ActionEventType actionEventType : ActionEventType.values()) {
      ActionDataSet actionDataSet = actionEventSet.getActionEvents(actionEventType);
      if (actionDataSet == null) {
        continue;
      }

      for (ActionDataEntry actionDataEntry : actionDataSet.getEntries()) {
        assertFalse(
            actionDataEntry.actionDataType() == ActionDataType.COMMAND,
            actionEventType + " must not need a command action");
        if (actionDataEntry.actionDataType() == ActionDataType.MESSAGE) {
          assertTrue(actionDataEntry.command().isEmpty());
          assertTrue(actionDataEntry.messageActionData().hasTexts());
        }
      }
    }
  }

  @Test
  @DisplayName("The weather and time lines survive the preset round trip with their conditions")
  void testIdleLineConditionsSurviveTheRoundTrip() throws Exception {
    ActionDataSet idleLines =
        this.loadActionEventSet().getActionEvents(ActionEventType.ON_INTERVAL_NORMAL);
    assertEquals(4, idleLines.size());

    boolean hasRainLine = false;
    boolean hasNightLine = false;
    for (ActionDataEntry actionDataEntry : idleLines.getEntries()) {
      assertEquals(ActionDataType.MESSAGE, actionDataEntry.actionDataType());

      Set<ConditionType> conditionTypes =
          actionDataEntry.conditionDataSet().getConditions().stream()
              .map(ConditionDataEntry::conditionType)
              .collect(Collectors.toSet());
      if (conditionTypes.contains(ConditionType.WEATHER)
          && actionDataEntry.messageActionData().texts().stream()
              .anyMatch(text -> text.contains("wet"))) {
        hasRainLine = true;
        assertTrue(conditionTypes.contains(ConditionType.CHANCE));
      }
      if (conditionTypes.contains(ConditionType.TIME_OF_DAY)) {
        hasNightLine = true;
        assertTrue(actionDataEntry.messageActionData().showAsSpeechBubble());
        assertFalse(actionDataEntry.messageActionData().showInNearbyChat());
      }
    }

    assertTrue(hasRainLine, "The rain line must keep its weather and chance condition");
    assertTrue(hasNightLine, "The night line must keep its time condition and bubble target");
  }

  @Test
  @DisplayName("Every chance condition of the preset is a valid percentage")
  void testChanceConditionsAreValid() throws Exception {
    ActionEventSet actionEventSet = this.loadActionEventSet();

    for (ActionEventType actionEventType : ActionEventType.values()) {
      ActionDataSet actionDataSet = actionEventSet.getActionEvents(actionEventType);
      if (actionDataSet == null) {
        continue;
      }

      for (ActionDataEntry actionDataEntry : actionDataSet.getEntries()) {
        for (ConditionDataEntry conditionDataEntry :
            actionDataEntry.conditionDataSet().getConditions()) {
          if (conditionDataEntry.conditionType() == ConditionType.CHANCE) {
            assertTrue(
                conditionDataEntry.isValid(),
                "The chance condition of "
                    + actionDataEntry.messageActionData().texts()
                    + " must be valid");
          }
        }
      }
    }
  }
}
