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

import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ActionEventTypeTest {

  @Test
  @DisplayName("Every distance event belongs to its own action group")
  void testDistanceEventsHaveTheirOwnGroup() {
    Set<ActionGroup> actionGroups =
        Arrays.stream(ActionEventType.values())
            .filter(ActionEventType::isDistanceEvent)
            .map(ActionEventType::getActionGroup)
            .collect(Collectors.toSet());

    assertEquals(
        Arrays.stream(ActionEventType.values()).filter(ActionEventType::isDistanceEvent).count(),
        actionGroups.size());
    assertFalse(actionGroups.contains(ActionGroup.NONE));
  }

  @Test
  @DisplayName("Only distance events carry a trigger distance")
  void testOnlyDistanceEventsHaveATriggerDistance() {
    for (ActionEventType actionEventType : ActionEventType.values()) {
      if (actionEventType.isDistanceEvent()) {
        assertTrue(
            actionEventType.getTriggerDistance() > 0.0D,
            actionEventType + " must have a trigger distance");
      } else {
        assertEquals(
            0.0D,
            actionEventType.getTriggerDistance(),
            actionEventType + " must not have a trigger distance");
      }
    }
  }

  @Test
  @DisplayName("Only interval events carry an interval")
  void testOnlyIntervalEventsHaveAnInterval() {
    for (ActionEventType actionEventType : ActionEventType.values()) {
      if (actionEventType.isIntervalEvent()) {
        assertTrue(
            actionEventType.getIntervalSeconds() > 0, actionEventType + " must have an interval");
        assertFalse(
            actionEventType.isDistanceEvent(),
            actionEventType + " must not be a distance event at the same time");
      } else {
        assertEquals(
            0,
            actionEventType.getIntervalSeconds(),
            actionEventType + " must not have an interval");
      }
    }
  }

  @Test
  @DisplayName("The interval events are checked from the shortest to the longest interval")
  void testIntervalEventsAreCheckedShortestFirst() {
    List<ActionEventType> checkOrder = ActionHandler.INTERVAL_ACTION_EVENT_TYPES;

    assertEquals(
        EnumSet.copyOf(
            Arrays.stream(ActionEventType.values())
                .filter(ActionEventType::isIntervalEvent)
                .toList()),
        EnumSet.copyOf(checkOrder));
    for (int i = 1; i < checkOrder.size(); i++) {
      assertTrue(
          checkOrder.get(i - 1).getIntervalSeconds() < checkOrder.get(i).getIntervalSeconds(),
          checkOrder.get(i - 1) + " must be checked before " + checkOrder.get(i));
    }
  }

  @Test
  @DisplayName("Every interval event has its own ticker")
  void testIntervalEventsHaveTheirOwnTicker() {
    assertEquals(
        ActionHandler.INTERVAL_ACTION_EVENT_TYPES.size(),
        Set.copyOf(ActionHandler.INTERVAL_ACTION_TICKERS.values()).size());
    for (ActionEventType actionEventType : ActionHandler.INTERVAL_ACTION_EVENT_TYPES) {
      assertTrue(
          ActionHandler.INTERVAL_ACTION_TICKERS.containsKey(actionEventType),
          actionEventType + " must have a ticker");
    }
  }

  @Test
  @DisplayName("The distance events are checked from the widest to the narrowest range")
  void testDistanceEventsAreCheckedWidestFirst() {
    List<ActionEventType> checkOrder = ActionHandler.DISTANCE_ACTION_EVENT_TYPES;

    assertEquals(
        EnumSet.copyOf(
            Arrays.stream(ActionEventType.values())
                .filter(ActionEventType::isDistanceEvent)
                .toList()),
        EnumSet.copyOf(checkOrder));
    for (int i = 1; i < checkOrder.size(); i++) {
      assertTrue(
          checkOrder.get(i - 1).getTriggerDistance() > checkOrder.get(i).getTriggerDistance(),
          checkOrder.get(i - 1) + " must be checked before " + checkOrder.get(i));
    }
  }

  @Test
  @DisplayName("A distance action set is kept for every distance event")
  void testDistanceActionSetIsKept() {
    for (ActionEventType actionEventType : ActionHandler.DISTANCE_ACTION_EVENT_TYPES) {
      ActionDataSet actionDataSet = new ActionDataSet();
      actionDataSet.add(new ActionDataEntry(ActionDataType.COMMAND, "say hi"));

      ActionEventSet actionEventSet = new ActionEventSet();
      actionEventSet.setActionEvent(actionEventType, actionDataSet);

      assertTrue(
          actionEventSet.hasActionEvent(actionEventType),
          actionEventType + " must keep its action data");
    }
  }

  @Test
  @DisplayName("The fastest interval only allows state and custom actions")
  void testInstantIntervalLimitsActionDataTypes() {
    assertTrue(ActionEventType.ON_INTERVAL_INSTANT.allowsActionDataType(ActionDataType.NPC_STATE));
    assertTrue(ActionEventType.ON_INTERVAL_INSTANT.allowsActionDataType(ActionDataType.CUSTOM));
    assertFalse(ActionEventType.ON_INTERVAL_INSTANT.allowsActionDataType(ActionDataType.COMMAND));
    assertFalse(ActionEventType.ON_INTERVAL_INSTANT.allowsActionDataType(ActionDataType.MESSAGE));
  }

  @Test
  @DisplayName("Every other event allows all action data types")
  void testOtherEventsAllowAllActionDataTypes() {
    for (ActionEventType actionEventType : ActionEventType.values()) {
      if (actionEventType == ActionEventType.ON_INTERVAL_INSTANT
          || actionEventType == ActionEventType.ON_DEATH) {
        continue;
      }
      for (ActionDataType actionDataType : ActionDataType.values()) {
        assertTrue(
            actionEventType.allowsActionDataType(actionDataType),
            actionEventType + " must allow " + actionDataType);
      }
    }
  }

  @Test
  @DisplayName("The death event allows every action data type but a wait")
  void testDeathEventDisallowsWaitActionDataType() {
    for (ActionDataType actionDataType : ActionDataType.values()) {
      if (actionDataType == ActionDataType.WAIT) {
        assertFalse(ActionEventType.ON_DEATH.allowsActionDataType(actionDataType));
        continue;
      }

      assertTrue(
          ActionEventType.ON_DEATH.allowsActionDataType(actionDataType),
          ActionEventType.ON_DEATH + " must allow " + actionDataType);
    }
  }
}
