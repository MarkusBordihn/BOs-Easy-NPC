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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class PendingActionChainTest {

  private static final UUID INITIATOR_UUID =
      UUID.fromString("11111111-2222-3333-4444-555555555555");
  private static final UUID DUPLICATE_ENTRY_UUID =
      UUID.fromString("66666666-7777-8888-9999-aaaaaaaaaaaa");

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static ActionDataEntry command(String command) {
    return new ActionDataEntry(ActionDataType.COMMAND, command);
  }

  private static ActionDataEntry commandWithId(UUID id, String command) {
    return new ActionDataEntry(
        id,
        ActionDataType.COMMAND,
        new ConditionDataSet(),
        command,
        null,
        BlockPos.ZERO,
        false,
        false,
        0,
        MessageActionData.DEFAULT);
  }

  private static void assertSameActions(
      List<ActionDataEntry> expected, List<ActionDataEntry> actual) {
    assertEquals(expected.size(), actual.size());
    for (int i = 0; i < expected.size(); i++) {
      assertEquals(expected.get(i).id(), actual.get(i).id());
      assertEquals(expected.get(i).actionDataType(), actual.get(i).actionDataType());
      assertEquals(expected.get(i).command(), actual.get(i).command());
    }
  }

  @Test
  @DisplayName("A chain keeps its event, remaining ticks, initiator and actions over a save")
  void testNbtRoundTrip() {
    List<ActionDataEntry> remainingActions =
        List.of(command("say a"), command("say b"), command("say c"));
    List<ActionDataEntry> fallbackActions = List.of(command("say fallback"));
    PendingActionChain pendingActionChain =
        new PendingActionChain(
            ActionEventType.ON_OWNER_LOGIN,
            123,
            INITIATOR_UUID,
            remainingActions,
            fallbackActions,
            ActionExecutionState.EMPTY);

    PendingActionChain loadedChain = PendingActionChain.fromTag(pendingActionChain.createTag());

    assertNotNull(loadedChain);
    assertEquals(ActionEventType.ON_OWNER_LOGIN, loadedChain.actionEventType());
    assertEquals(123, loadedChain.remainingTicks());
    assertEquals(INITIATOR_UUID, loadedChain.initiatorUUID());
    assertSameActions(remainingActions, loadedChain.remainingActions());
    assertSameActions(fallbackActions, loadedChain.fallbackActions());
  }

  @Test
  @DisplayName("A chain keeps identical actions instead of collapsing them into one")
  void testDuplicateActionsAreKept() {
    List<ActionDataEntry> remainingActions =
        List.of(
            commandWithId(DUPLICATE_ENTRY_UUID, "say twice"),
            commandWithId(DUPLICATE_ENTRY_UUID, "say twice"),
            command("say once"));
    assertEquals(remainingActions.get(0), remainingActions.get(1));

    PendingActionChain loadedChain =
        PendingActionChain.fromTag(
            new PendingActionChain(
                    ActionEventType.ON_INTERVAL_SHORT,
                    20,
                    null,
                    remainingActions,
                    List.of(),
                    ActionExecutionState.EMPTY)
                .createTag());

    assertNotNull(loadedChain);
    assertNull(loadedChain.initiatorUUID());
    assertSameActions(remainingActions, loadedChain.remainingActions());
    assertTrue(loadedChain.fallbackActions().isEmpty());
  }

  @Test
  @DisplayName("A chain keeps the set rules and the deferred dialog close over a save")
  void testExecutionStateRoundTrip() {
    ActionDataEntry closeDialogAction = new ActionDataEntry(ActionDataType.CLOSE_DIALOG);
    ActionExecutionState executionState =
        ActionExecutionState.EMPTY
            .withAnyRegularFired()
            .withScreenAction()
            .withDeferredCloseDialogAction(closeDialogAction);

    PendingActionChain loadedChain =
        PendingActionChain.fromTag(
            new PendingActionChain(
                    ActionEventType.ON_CLOSE_DIALOG,
                    5,
                    null,
                    List.of(command("say a")),
                    List.of(),
                    executionState)
                .createTag());

    assertNotNull(loadedChain);
    assertTrue(loadedChain.executionState().anyRegularFired());
    assertTrue(loadedChain.executionState().hasScreenAction());
    assertTrue(loadedChain.executionState().hasDeferredCloseDialogAction());
    assertEquals(
        closeDialogAction.id(), loadedChain.executionState().deferredCloseDialogAction().id());
  }

  @Test
  @DisplayName("An empty chain stays empty over a save")
  void testEmptyChainRoundTrip() {
    PendingActionChain loadedChain =
        PendingActionChain.fromTag(
            new PendingActionChain(null, 0, null, null, null, null).createTag());

    assertNotNull(loadedChain);
    assertEquals(ActionEventType.NONE, loadedChain.actionEventType());
    assertTrue(loadedChain.remainingActions().isEmpty());
    assertTrue(loadedChain.fallbackActions().isEmpty());
    assertEquals(ActionExecutionState.EMPTY, loadedChain.executionState());
    assertTrue(loadedChain.isDue());
  }

  @Test
  @DisplayName("An oversized duration or action list is shortened when it is read")
  void testOversizedChainIsLimited() {
    List<ActionDataEntry> remainingActions = new ArrayList<>();
    for (int i = 0; i < PendingActionChain.MAX_CHAIN_LENGTH + 10; i++) {
      remainingActions.add(command("say " + i));
    }

    PendingActionChain pendingActionChain =
        new PendingActionChain(
            ActionEventType.ON_SPAWN,
            Integer.MAX_VALUE,
            null,
            remainingActions,
            List.of(),
            ActionExecutionState.EMPTY);

    assertEquals(WaitDuration.MAX_TICKS, pendingActionChain.remainingTicks());
    assertEquals(PendingActionChain.MAX_CHAIN_LENGTH, pendingActionChain.remainingActions().size());

    CompoundTag compoundTag = pendingActionChain.createTag();
    compoundTag.putInt("Ticks", Integer.MAX_VALUE);
    PendingActionChain loadedChain = PendingActionChain.fromTag(compoundTag);

    assertNotNull(loadedChain);
    assertEquals(WaitDuration.MAX_TICKS, loadedChain.remainingTicks());
    assertEquals(PendingActionChain.MAX_CHAIN_LENGTH, loadedChain.remainingActions().size());
  }

  @Test
  @DisplayName("A chain counts down until it is due")
  void testTickUntilDue() {
    PendingActionChain pendingActionChain =
        new PendingActionChain(
            ActionEventType.ON_SPAWN,
            2,
            null,
            List.of(command("say a")),
            List.of(),
            ActionExecutionState.EMPTY);

    assertFalse(pendingActionChain.isDue());
    assertFalse(pendingActionChain.tick().isDue());
    assertTrue(pendingActionChain.tick().tick().isDue());
    assertEquals(0, pendingActionChain.tick().tick().tick().remainingTicks());
  }

  @Test
  @DisplayName("A set keeps one chain per event and hands over the due ones")
  void testPendingActionSetRoundTrip() {
    PendingActionSet pendingActionSet = new PendingActionSet();
    pendingActionSet.schedule(
        new PendingActionChain(
            ActionEventType.ON_SPAWN,
            300,
            INITIATOR_UUID,
            List.of(command("say spawn")),
            List.of(),
            ActionExecutionState.EMPTY));
    pendingActionSet.schedule(
        new PendingActionChain(
            ActionEventType.ON_INTERVAL_SHORT,
            40,
            null,
            List.of(command("say interval")),
            List.of(),
            ActionExecutionState.EMPTY));
    pendingActionSet.schedule(
        new PendingActionChain(
            ActionEventType.ON_SPAWN,
            1,
            null,
            List.of(command("say spawn again")),
            List.of(),
            ActionExecutionState.EMPTY));

    PendingActionSet loadedSet = new PendingActionSet(pendingActionSet.createTag());

    assertTrue(loadedSet.has(ActionEventType.ON_SPAWN, null));
    assertTrue(loadedSet.has(ActionEventType.ON_INTERVAL_SHORT, null));

    List<PendingActionChain> dueChains = loadedSet.tickAndRemoveDueChains();
    assertEquals(1, dueChains.size());
    assertEquals(ActionEventType.ON_SPAWN, dueChains.get(0).actionEventType());
    assertEquals("say spawn again", dueChains.get(0).remainingActions().get(0).command());
    assertFalse(loadedSet.has(ActionEventType.ON_SPAWN, null));
    assertTrue(loadedSet.has(ActionEventType.ON_INTERVAL_SHORT, null));

    loadedSet.cancel(ActionEventType.ON_INTERVAL_SHORT, null);
    assertTrue(loadedSet.isEmpty());
    assertTrue(loadedSet.createTag().isEmpty());
  }
}
