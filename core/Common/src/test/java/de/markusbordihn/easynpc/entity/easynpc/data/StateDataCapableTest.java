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

package de.markusbordihn.easynpc.entity.easynpc.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.api.event.EasyNPCEventRegistry;
import de.markusbordihn.easynpc.api.event.StateEventListener;
import de.markusbordihn.easynpc.data.state.StateDataSet;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.server.player.FakePlayer;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import net.minecraft.core.BlockPos;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class StateDataCapableTest {

  private static final Identifier QUEST =
      Identifier.fromNamespaceAndPath("test_mod", "quest_progress");
  private static final Identifier STAGE = Identifier.fromNamespaceAndPath("test_mod", "stage");

  @Test
  void testNumberTextAndFlagAccessors() {
    TestStateData stateData = new TestStateData();

    stateData.setState(QUEST, 3);
    stateData.setState(STAGE, "intro");

    assertEquals(3, stateData.getStateNumber(QUEST));
    assertEquals("", stateData.getStateText(QUEST));
    assertTrue(stateData.getStateFlag(QUEST));
    assertEquals("intro", stateData.getStateText(STAGE));
    assertEquals(0, stateData.getStateNumber(STAGE));
    assertTrue(stateData.hasState(STAGE));
  }

  @Test
  @DisplayName("A state that was never set reads as zero without being created")
  void testUnsetState() {
    TestStateData stateData = new TestStateData();

    assertEquals(0, stateData.getStateNumber(QUEST));
    assertEquals("", stateData.getStateText(QUEST));
    assertFalse(stateData.getStateFlag(QUEST));
    assertFalse(stateData.hasState(QUEST));
    assertNull(stateData.getState(QUEST));
  }

  @Test
  void testRemoveAndClear() {
    TestStateData stateData = new TestStateData();
    stateData.setState(QUEST, 3);
    stateData.setState(STAGE, "intro");

    stateData.removeState(QUEST);
    assertFalse(stateData.hasState(QUEST));
    assertTrue(stateData.hasState(STAGE));

    stateData.clearStateDataSet();
    assertTrue(stateData.getStateDataSet().isEmpty());
  }

  @Test
  @DisplayName("A changed state notifies a listener with the previous and the new value")
  void testStateChangeIsReported() {
    TestStateData stateData = new TestStateData();
    List<String> reportedChanges = new ArrayList<>();
    StateEventListener stateEventListener =
        (easyNPC, stateId, previousStateEntry, currentStateEntry) ->
            reportedChanges.add(
                stateId
                    + ":"
                    + (previousStateEntry != null ? previousStateEntry.numberValue() : "unset")
                    + "->"
                    + (currentStateEntry != null ? currentStateEntry.numberValue() : "unset"));
    EasyNPCEventRegistry.registerStateEventListener(stateEventListener);

    try {
      stateData.setState(QUEST, 1);
      stateData.setState(QUEST, 2);
      stateData.removeState(QUEST);
    } finally {
      EasyNPCEventRegistry.unregisterStateEventListener(stateEventListener);
    }

    assertEquals(
        List.of(
            "test_mod:quest_progress:unset->1",
            "test_mod:quest_progress:1->2",
            "test_mod:quest_progress:2->unset"),
        reportedChanges);
  }

  @Test
  @DisplayName("Setting the value a state already has reports nothing")
  void testUnchangedStateIsNotReported() {
    TestStateData stateData = new TestStateData();
    stateData.setState(QUEST, 1);

    AtomicInteger reportedChanges = new AtomicInteger();
    StateEventListener stateEventListener =
        (easyNPC, stateId, previousStateEntry, currentStateEntry) ->
            reportedChanges.incrementAndGet();
    EasyNPCEventRegistry.registerStateEventListener(stateEventListener);

    try {
      stateData.setState(QUEST, 1);
      stateData.removeState(STAGE);
    } finally {
      EasyNPCEventRegistry.unregisterStateEventListener(stateEventListener);
    }

    assertEquals(0, reportedChanges.get());
  }

  @Test
  @DisplayName("A failing listener does not stop the state from being written")
  void testFailingListenerDoesNotBlockTheChange() {
    TestStateData stateData = new TestStateData();
    StateEventListener failingListener =
        (easyNPC, stateId, previousStateEntry, currentStateEntry) -> {
          throw new IllegalStateException("listener is broken");
        };
    EasyNPCEventRegistry.registerStateEventListener(failingListener);

    try {
      stateData.setState(QUEST, 4);
    } finally {
      EasyNPCEventRegistry.unregisterStateEventListener(failingListener);
    }

    assertEquals(4, stateData.getStateNumber(QUEST));
  }

  private static final class TestStateData implements EasyNPC<Mob>, StateDataCapable<Mob> {

    private StateDataSet stateDataSet = new StateDataSet();
    private int npcDataVersion;

    @Override
    public StateDataSet getStateDataSet() {
      return this.stateDataSet;
    }

    @Override
    public void setStateDataSet(StateDataSet stateDataSet) {
      this.stateDataSet = stateDataSet;
    }

    @Override
    public void clearStateDataSet() {
      this.stateDataSet = new StateDataSet();
    }

    @Override
    public int getNPCDataVersion() {
      return this.npcDataVersion;
    }

    @Override
    public void setNPCDataVersion(int version) {
      this.npcDataVersion = version;
    }

    @Override
    public FakePlayer getFakePlayer(ServerLevel level, BlockPos blockPos) {
      return null;
    }

    @Override
    public <T> void defineSynchedEntityData(
        SynchedEntityData.Builder builder, SynchedDataIndex synchedDataIndex, T defaultData) {}

    @Override
    public <T> void setSynchedEntityData(
        SynchedDataIndex synchedDataIndex, T data, boolean forceUpdate) {}

    @Override
    public <T> T getSynchedEntityData(SynchedDataIndex synchedDataIndex) {
      return null;
    }

    @Override
    public GoalSelector getEntityGoalSelector() {
      return null;
    }

    @Override
    public GoalSelector getEntityTargetSelector() {
      return null;
    }
  }
}
