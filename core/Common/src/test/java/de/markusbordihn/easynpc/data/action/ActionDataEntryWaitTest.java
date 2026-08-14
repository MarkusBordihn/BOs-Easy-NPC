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

import java.util.List;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ActionDataEntryWaitTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static ActionDataSet actionDataSet(ActionDataEntry... actionDataEntries) {
    ActionDataSet actionDataSet = new ActionDataSet();
    for (ActionDataEntry actionDataEntry : actionDataEntries) {
      actionDataSet.add(actionDataEntry);
    }

    return actionDataSet;
  }

  @Test
  @DisplayName("A wait action needs a duration to be valid")
  void testWaitActionValidity() {
    assertTrue(new ActionDataEntry(ActionDataType.WAIT, "20s").isValidAndNotEmpty());
    assertTrue(new ActionDataEntry(ActionDataType.WAIT, "400t").isValidAndNotEmpty());
    assertFalse(new ActionDataEntry(ActionDataType.WAIT, "").isValidAndNotEmpty());
    assertFalse(new ActionDataEntry(ActionDataType.WAIT, "soon").isValidAndNotEmpty());
    assertTrue(ActionDataType.WAIT.requiresArgument());
  }

  @Test
  @DisplayName("A saved action set keeps its wait action and its order")
  void testWaitActionSurvivesSave() {
    ActionDataSet actionDataSet =
        actionDataSet(
            new ActionDataEntry(ActionDataType.COMMAND, "say a"),
            new ActionDataEntry(ActionDataType.WAIT, "20s"),
            new ActionDataEntry(ActionDataType.COMMAND, "say b"));

    ActionDataSet loadedDataSet =
        new ActionDataSet(actionDataSet.createTag(), ActionDataSet.ACTION_DATA_SET_TAG);
    List<ActionDataEntry> loadedEntries = loadedDataSet.getOrderedEntries();

    assertEquals(3, loadedEntries.size());
    assertEquals(ActionDataType.COMMAND, loadedEntries.get(0).actionDataType());
    assertEquals(ActionDataType.WAIT, loadedEntries.get(1).actionDataType());
    assertEquals("20s", loadedEntries.get(1).command());
    assertEquals(ActionDataType.COMMAND, loadedEntries.get(2).actionDataType());
    assertTrue(loadedDataSet.hasActionDataType(ActionDataType.WAIT));
  }

  @Test
  @DisplayName("A saved action set drops a wait action without a usable duration")
  void testInvalidWaitActionIsDropped() {
    ActionDataSet actionDataSet =
        actionDataSet(
            new ActionDataEntry(ActionDataType.COMMAND, "say a"),
            new ActionDataEntry(ActionDataType.WAIT, "soon"),
            new ActionDataEntry(ActionDataType.COMMAND, "say b"));

    ActionDataSet loadedDataSet =
        new ActionDataSet(actionDataSet.createTag(), ActionDataSet.ACTION_DATA_SET_TAG);
    List<ActionDataEntry> loadedEntries = loadedDataSet.getOrderedEntries();

    assertEquals(2, loadedEntries.size());
    assertEquals("say a", loadedEntries.get(0).command());
    assertEquals("say b", loadedEntries.get(1).command());
    assertFalse(loadedDataSet.hasActionDataType(ActionDataType.WAIT));
  }

  @Test
  @DisplayName("An action set reports a wait action only for a usable duration")
  void testHasActionDataType() {
    assertFalse(
        actionDataSet(new ActionDataEntry(ActionDataType.WAIT, "soon"))
            .hasActionDataType(ActionDataType.WAIT));
    assertTrue(
        actionDataSet(new ActionDataEntry(ActionDataType.WAIT, "5m"))
            .hasActionDataType(ActionDataType.WAIT));
    assertFalse(
        actionDataSet(new ActionDataEntry(ActionDataType.COMMAND, "say a"))
            .hasActionDataType(ActionDataType.WAIT));
  }
}
