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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.MoveActionData;
import de.markusbordihn.easynpc.data.action.MoveTargetType;
import de.markusbordihn.easynpc.data.attribute.MovementAttributeType;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.MoveToPositionGoal;
import de.markusbordihn.easynpc.entity.easynpc.handlers.PendingActionHandler;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ai.goal.WrappedGoal;
import net.minecraft.world.phys.Vec3;

public class MoveActionTestHelper {

  private static final ResourceLocation FIRST_STATE = StateIdentifier.parse("gametest_move_first");
  private static final ResourceLocation SECOND_STATE =
      StateIdentifier.parse("gametest_move_second");
  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final BlockPos NEARBY_OFFSET = new BlockPos(0, 1, 0);
  private static final BlockPos UNREACHABLE_OFFSET = new BlockPos(0, 40, 0);
  private static final int TIMEOUT_TICKS = MoveActionData.MIN_TIMEOUT_TICKS;
  private static final int BACKSTOP_TICKS = TIMEOUT_TICKS + 200;

  private MoveActionTestHelper() {}

  private static ActionDataEntry increaseState(ResourceLocation stateIdentifier) {
    return new ActionDataEntry(
        ActionDataType.NPC_STATE, "increase " + stateIdentifier.getPath() + " 1");
  }

  private static ActionDataEntry moveAction(ActionDataType actionDataType, BlockPos offset) {
    return new ActionDataEntry(actionDataType)
        .withMoveActionData(
            new MoveActionData(
                MoveTargetType.RELATIVE,
                MoveActionData.DEFAULT_SPEED_MODIFIER,
                MoveActionData.DEFAULT_ARRIVAL_RADIUS,
                TIMEOUT_TICKS,
                false))
        .withBlockPos(offset);
  }

  private static ActionDataSet movementChain(ActionDataType actionDataType, BlockPos offset) {
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(increaseState(FIRST_STATE));
    actionDataSet.add(moveAction(actionDataType, offset));
    actionDataSet.add(increaseState(SECOND_STATE));
    return actionDataSet;
  }

  private static void executeChain(EasyNPC<?> easyNPC, ActionDataSet actionDataSet) {
    easyNPC
        .getEasyNPCActionHandler()
        .executeActions(actionDataSet, ActionContext.of(ActionEventType.ON_SPAWN, null, null));
  }

  private static int stateNumber(EasyNPC<?> easyNPC, ResourceLocation stateIdentifier) {
    return easyNPC.getEasyNPCStateData().getStateNumber(stateIdentifier);
  }

  private static boolean hasPendingChain(EasyNPC<?> easyNPC) {
    return easyNPC
        .getEasyNPCPendingActionHandler()
        .hasPendingAction(ActionEventType.ON_SPAWN, null);
  }

  private static int moveGoals(EasyNPC<?> easyNPC) {
    int moveGoals = 0;
    for (WrappedGoal wrappedGoal : easyNPC.getEntityGoalSelector().getAvailableGoals()) {
      if (wrappedGoal.getGoal() instanceof MoveToPositionGoal<?>) {
        moveGoals++;
      }
    }

    return moveGoals;
  }

  private static void tick(EasyNPC<?> easyNPC, int ticks) {
    PendingActionHandler<?> pendingActionHandler = easyNPC.getEasyNPCPendingActionHandler();
    for (int i = 0; i < ticks; i++) {
      easyNPC.getEntityGoalSelector().tick();
      pendingActionHandler.tickPendingActions();
    }
  }

  public static void assertMoveKeepsTheChainRunning(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    executeChain(easyNPC, movementChain(ActionDataType.MOVE_TO, UNREACHABLE_OFFSET));

    GameTestHelpers.assertEquals(
        helper, "The action before the move runs", 1, stateNumber(easyNPC, FIRST_STATE));
    GameTestHelpers.assertEquals(
        helper,
        "The action after a move without waiting runs right away",
        1,
        stateNumber(easyNPC, SECOND_STATE));
    GameTestHelpers.assertTrue(
        helper, "A move without waiting must not park a chain", !hasPendingChain(easyNPC));
    GameTestHelpers.assertEquals(helper, "The move goal is started", 1, moveGoals(easyNPC));
  }

  public static void assertMoveAndWaitParksTheChain(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    executeChain(easyNPC, movementChain(ActionDataType.MOVE_TO_AND_WAIT, UNREACHABLE_OFFSET));

    GameTestHelpers.assertEquals(
        helper, "The action before the move runs", 1, stateNumber(easyNPC, FIRST_STATE));
    GameTestHelpers.assertEquals(
        helper,
        "The action after the move waits for the arrival",
        0,
        stateNumber(easyNPC, SECOND_STATE));
    GameTestHelpers.assertTrue(helper, "The chain must be parked", hasPendingChain(easyNPC));
  }

  public static void assertArrivalResumesTheChain(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    executeChain(easyNPC, movementChain(ActionDataType.MOVE_TO_AND_WAIT, NEARBY_OFFSET));
    tick(easyNPC, 1);

    GameTestHelpers.assertEquals(
        helper,
        "A target within the arrival radius resumes the chain right away",
        1,
        stateNumber(easyNPC, SECOND_STATE));
    GameTestHelpers.assertTrue(
        helper, "The parked chain must be gone once it is done", !hasPendingChain(easyNPC));
  }

  public static void assertTimeoutResumesTheChain(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    executeChain(easyNPC, movementChain(ActionDataType.MOVE_TO_AND_WAIT, UNREACHABLE_OFFSET));
    tick(easyNPC, BACKSTOP_TICKS);

    GameTestHelpers.assertEquals(
        helper,
        "An unreachable target must not stall the chain forever",
        1,
        stateNumber(easyNPC, SECOND_STATE));
    GameTestHelpers.assertTrue(
        helper, "The parked chain must be gone after the timeout", !hasPendingChain(easyNPC));
  }

  public static void assertImmovableNPCDoesNotBlockTheChain(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setMovementAttribute(easyNPC, MovementAttributeType.IS_IMMOVABLE, true);

    executeChain(easyNPC, movementChain(ActionDataType.MOVE_TO_AND_WAIT, UNREACHABLE_OFFSET));

    GameTestHelpers.assertEquals(
        helper,
        "An immovable NPC runs the rest of the chain instead of waiting",
        1,
        stateNumber(easyNPC, SECOND_STATE));
    GameTestHelpers.assertEquals(
        helper, "An immovable NPC must not get a move goal", 0, moveGoals(easyNPC));
  }

  public static void assertSecondMoveReplacesTheFirstGoal(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    executeChain(easyNPC, movementChain(ActionDataType.MOVE_TO, UNREACHABLE_OFFSET));
    executeChain(easyNPC, movementChain(ActionDataType.MOVE_TO, UNREACHABLE_OFFSET));

    GameTestHelpers.assertEquals(
        helper, "A second move action must replace the first goal", 1, moveGoals(easyNPC));
  }

  public static void assertMoveWithoutTargetIsSkipped(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(
        new ActionDataEntry(ActionDataType.MOVE_TO_AND_WAIT)
            .withMoveActionData(new MoveActionData(MoveTargetType.INITIATOR)));
    actionDataSet.add(increaseState(SECOND_STATE));

    executeChain(easyNPC, actionDataSet);

    GameTestHelpers.assertEquals(
        helper,
        "A move without a resolvable target must not park the chain",
        1,
        stateNumber(easyNPC, SECOND_STATE));
    GameTestHelpers.assertEquals(
        helper, "A move without a resolvable target adds no goal", 0, moveGoals(easyNPC));
  }
}
