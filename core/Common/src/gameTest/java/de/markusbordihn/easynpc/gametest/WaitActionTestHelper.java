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
import de.markusbordihn.easynpc.data.action.PendingActionChain;
import de.markusbordihn.easynpc.data.action.PendingActionSet;
import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.RelationshipType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.PendingActionHandler;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import java.util.List;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class WaitActionTestHelper {

  private static final ResourceLocation FIRST_STATE = StateIdentifier.parse("gametest_wait_first");
  private static final ResourceLocation SECOND_STATE =
      StateIdentifier.parse("gametest_wait_second");
  private static final ResourceLocation FALLBACK_STATE =
      StateIdentifier.parse("gametest_wait_fallback");
  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 SECOND_NPC_POSITION = new Vec3(1, 2, 2);
  private static final Vec3 PLAYER_POSITION = new Vec3(2, 2, 1);
  private static final int WAIT_TICKS = 20;

  private WaitActionTestHelper() {}

  private static ActionDataEntry increaseState(ResourceLocation stateIdentifier) {
    return new ActionDataEntry(
        ActionDataType.NPC_STATE, "increase " + stateIdentifier.getPath() + " 1");
  }

  private static ActionDataEntry waitAction() {
    return new ActionDataEntry(ActionDataType.WAIT, "1s");
  }

  private static ActionDataSet actionDataSet(ActionDataEntry... actionDataEntries) {
    ActionDataSet actionDataSet = new ActionDataSet();
    for (ActionDataEntry actionDataEntry : actionDataEntries) {
      actionDataSet.add(actionDataEntry);
    }

    return actionDataSet;
  }

  private static ActionDataSet delayedGreeting() {
    return actionDataSet(increaseState(FIRST_STATE), waitAction(), increaseState(SECOND_STATE));
  }

  private static int stateNumber(EasyNPC<?> easyNPC, ResourceLocation stateIdentifier) {
    return easyNPC.getEasyNPCStateData().getStateNumber(stateIdentifier);
  }

  private static void tick(EasyNPC<?> easyNPC, int ticks) {
    PendingActionHandler<?> pendingActionHandler = easyNPC.getEasyNPCPendingActionHandler();
    for (int i = 0; i < ticks; i++) {
      pendingActionHandler.tickPendingActions();
    }
  }

  public static void assertWaitDelaysTheFollowingActions(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    easyNPC
        .getEasyNPCActionHandler()
        .executeActions(delayedGreeting(), ActionContext.of(ActionEventType.ON_SPAWN, null, null));

    GameTestHelpers.assertEquals(
        helper, "The first action runs right away", 1, stateNumber(easyNPC, FIRST_STATE));
    GameTestHelpers.assertEquals(
        helper, "The second action waits", 0, stateNumber(easyNPC, SECOND_STATE));

    tick(easyNPC, WAIT_TICKS - 1);

    GameTestHelpers.assertEquals(
        helper,
        "The second action must not run before the wait is over",
        0,
        stateNumber(easyNPC, SECOND_STATE));

    tick(easyNPC, 1);

    GameTestHelpers.assertEquals(
        helper, "The second action runs after the wait", 1, stateNumber(easyNPC, SECOND_STATE));
    GameTestHelpers.assertTrue(
        helper,
        "The chain must be gone once it is done",
        !easyNPC.getEasyNPCPendingActionHandler().hasPendingAction(ActionEventType.ON_SPAWN, null));
  }

  public static void assertRemainingWaitSurvivesSaveAndLoad(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC
        .getEasyNPCActionHandler()
        .executeActions(delayedGreeting(), ActionContext.of(ActionEventType.ON_SPAWN, null, null));
    tick(easyNPC, 5);

    CompoundTag compoundTag = new CompoundTag();
    easyNPC.getEasyNPCActionEventData().addAdditionalActionData(compoundTag);

    EasyNPC<?> reloadedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION);
    reloadedNPC.getEasyNPCActionEventData().readAdditionalActionData(compoundTag);

    GameTestHelpers.assertTrue(
        helper,
        "The chain must survive the save",
        reloadedNPC
            .getEasyNPCPendingActionHandler()
            .hasPendingAction(ActionEventType.ON_SPAWN, null));

    tick(reloadedNPC, WAIT_TICKS - 6);

    GameTestHelpers.assertEquals(
        helper,
        "The remaining wait must be kept, not restarted",
        0,
        stateNumber(reloadedNPC, SECOND_STATE));

    tick(reloadedNPC, 1);

    GameTestHelpers.assertEquals(
        helper,
        "The chain continues where it was saved",
        1,
        stateNumber(reloadedNPC, SECOND_STATE));
  }

  public static void assertSecondTriggerIsDiscarded(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ActionHandler<?> actionHandler = easyNPC.getEasyNPCActionHandler();
    ActionContext actionContext = ActionContext.of(ActionEventType.ON_SPAWN, null, null);

    actionHandler.executeActions(delayedGreeting(), actionContext);
    tick(easyNPC, 5);
    actionHandler.executeActions(delayedGreeting(), actionContext);

    GameTestHelpers.assertEquals(
        helper,
        "A second trigger during the wait must be discarded",
        1,
        stateNumber(easyNPC, FIRST_STATE));

    tick(easyNPC, WAIT_TICKS);

    GameTestHelpers.assertEquals(
        helper, "The running chain finishes once", 1, stateNumber(easyNPC, SECOND_STATE));
    GameTestHelpers.assertEquals(
        helper, "The discarded trigger left nothing behind", 1, stateNumber(easyNPC, FIRST_STATE));
  }

  public static void assertDifferentEventsRunInParallel(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ActionHandler<?> actionHandler = easyNPC.getEasyNPCActionHandler();

    actionHandler.executeActions(
        actionDataSet(waitAction(), increaseState(FIRST_STATE)),
        ActionContext.of(ActionEventType.ON_SPAWN, null, null));
    actionHandler.executeActions(
        actionDataSet(waitAction(), increaseState(SECOND_STATE)),
        ActionContext.of(ActionEventType.ON_INTERVAL_SHORT, null, null));

    tick(easyNPC, WAIT_TICKS);

    GameTestHelpers.assertEquals(
        helper, "The chain of the first event finishes", 1, stateNumber(easyNPC, FIRST_STATE));
    GameTestHelpers.assertEquals(
        helper, "The chain of the second event finishes", 1, stateNumber(easyNPC, SECOND_STATE));
  }

  public static void assertIntervalSetWithWaitRunsInOrder(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);

    easyNPC
        .getEasyNPCActionHandler()
        .executeRandomAction(
            delayedGreeting(),
            ActionContext.of(
                ActionEventType.ON_INTERVAL_SHORT, serverPlayer, List.of(serverPlayer)));
    tick(easyNPC, WAIT_TICKS);

    GameTestHelpers.assertEquals(
        helper,
        "An interval set with a wait runs its first entry",
        1,
        stateNumber(easyNPC, FIRST_STATE));
    GameTestHelpers.assertEquals(
        helper,
        "An interval set with a wait runs its later entries as well",
        1,
        stateNumber(easyNPC, SECOND_STATE));
  }

  public static void assertIntervalSetWithoutWaitPicksOneEntry(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);

    easyNPC
        .getEasyNPCActionHandler()
        .executeRandomAction(
            actionDataSet(increaseState(FIRST_STATE), increaseState(SECOND_STATE)),
            ActionContext.of(
                ActionEventType.ON_INTERVAL_SHORT, serverPlayer, List.of(serverPlayer)));

    GameTestHelpers.assertEquals(
        helper,
        "An interval set without a wait stays a random pool",
        1,
        stateNumber(easyNPC, FIRST_STATE) + stateNumber(easyNPC, SECOND_STATE));
  }

  public static void assertScreenActionIsKeptOverTheWait(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);
    DialogDataSet dialogDataSet = new DialogDataSet();
    dialogDataSet.addDialog(new DialogDataEntry("default", "Default", "Hello"));
    easyNPC.getEasyNPCDialogData().setDialogDataSet(dialogDataSet);

    easyNPC
        .getEasyNPCActionHandler()
        .executeActions(
            actionDataSet(
                new ActionDataEntry(ActionDataType.OPEN_DEFAULT_DIALOG),
                waitAction(),
                new ActionDataEntry(ActionDataType.OPEN_TRADING_SCREEN)),
            ActionContext.of(ActionEventType.ON_INTERACTION, serverPlayer, List.of(serverPlayer)));

    PendingActionChain pendingActionChain =
        dueChain(easyNPC.getEasyNPCActionEventData().getPendingActionSet());

    GameTestHelpers.assertNotNull(helper, "The chain must be parked", pendingActionChain);
    GameTestHelpers.assertTrue(
        helper,
        "The opened screen must be remembered, so a second one stays closed",
        pendingActionChain.executionState().hasScreenAction());
  }

  public static void assertFallbackRunsOnceAfterTheWait(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    easyNPC
        .getEasyNPCActionHandler()
        .executeActions(
            actionDataSet(
                waitAction(),
                withCondition(
                    increaseState(FIRST_STATE),
                    new ConditionDataEntry(ConditionType.RELATIONSHIP)
                        .withSubType(RelationshipType.OWNER)),
                withCondition(
                    increaseState(FALLBACK_STATE), new ConditionDataEntry(ConditionType.FALLBACK))),
            ActionContext.of(ActionEventType.ON_SPAWN, null, null));
    tick(easyNPC, WAIT_TICKS);

    GameTestHelpers.assertEquals(
        helper,
        "The action with an unmet condition stays out",
        0,
        stateNumber(easyNPC, FIRST_STATE));
    GameTestHelpers.assertEquals(
        helper,
        "The fallback runs once at the end of the chain",
        1,
        stateNumber(easyNPC, FALLBACK_STATE));
  }

  public static void assertPresetImportCancelsTheChain(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC
        .getEasyNPCActionHandler()
        .executeActions(delayedGreeting(), ActionContext.of(ActionEventType.ON_SPAWN, null, null));

    CompoundTag presetTag = new CompoundTag();
    presetTag.put(ActionEventDataCapable.DATA_ACTION_DATA_TAG, new CompoundTag());
    easyNPC.getEasyNPCPresetData().importPresetData(presetTag);
    tick(easyNPC, WAIT_TICKS);

    GameTestHelpers.assertEquals(
        helper,
        "A preset import must stop the chain of the replaced action set",
        0,
        stateNumber(easyNPC, SECOND_STATE));
  }

  public static void assertDeathCancelsTheChain(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC
        .getEasyNPCActionHandler()
        .executeActions(delayedGreeting(), ActionContext.of(ActionEventType.ON_SPAWN, null, null));

    AttributeHandler.setCombatAttribute(easyNPC, CombatAttributeType.IS_INVULNERABLE, false);
    easyNPC.getEntity().hurt(helper.getLevel().damageSources().generic(), Float.MAX_VALUE);

    GameTestHelpers.assertTrue(
        helper,
        "A dead NPC must not keep a running chain",
        !easyNPC.getEasyNPCPendingActionHandler().hasPendingAction(ActionEventType.ON_SPAWN, null));
  }

  private static ActionDataEntry withCondition(
      ActionDataEntry actionDataEntry, ConditionDataEntry conditionDataEntry) {
    ConditionDataSet conditionDataSet = new ConditionDataSet();
    conditionDataSet.add(conditionDataEntry);

    return actionDataEntry.withConditionDataSet(conditionDataSet);
  }

  private static PendingActionChain dueChain(PendingActionSet pendingActionSet) {
    for (int i = 0; i < WAIT_TICKS; i++) {
      List<PendingActionChain> dueChains = pendingActionSet.tickAndRemoveDueChains();
      if (!dueChains.isEmpty()) {
        return dueChains.get(0);
      }
    }

    return null;
  }
}
