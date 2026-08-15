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

import de.markusbordihn.easynpc.api.handler.EasyNPCPauseHandler;
import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.BaseTickHandler;
import de.markusbordihn.easynpc.handler.PauseManager;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.Identifier;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.level.storage.TagValueOutput;
import net.minecraft.world.phys.Vec3;

public class PauseTestHelper {

  private static final Identifier DELAYED_STATE = StateIdentifier.parse("gametest_pause_delayed");
  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 SECOND_NPC_POSITION = new Vec3(1, 2, 2);
  private static final int WAIT_TICKS = 20;

  private PauseTestHelper() {}

  private static void baseTick(EasyNPC<?> easyNPC, int ticks) {
    for (int i = 0; i < ticks; i++) {
      ((BaseTickHandler<?>) easyNPC).handleBaseTick();
    }
  }

  private static int delayedStateNumber(EasyNPC<?> easyNPC) {
    return easyNPC.getEasyNPCStateData().getStateNumber(DELAYED_STATE);
  }

  private static void scheduleDelayedState(EasyNPC<?> easyNPC) {
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(new ActionDataEntry(ActionDataType.WAIT, "1s"));
    actionDataSet.add(
        new ActionDataEntry(
            ActionDataType.NPC_STATE, "increase " + DELAYED_STATE.getPath() + " 1"));

    easyNPC
        .getEasyNPCActionHandler()
        .executeActions(actionDataSet, ActionContext.of(ActionEventType.ON_SPAWN, null, null));
  }

  public static void assertPauseAndResumeOfASingleNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    PauseManager.reset();
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    GameTestHelpers.assertTrue(
        helper, "A new NPC must not be paused", !EasyNPCPauseHandler.isPaused(easyNPC));

    GameTestHelpers.assertTrue(
        helper, "Pausing an NPC must be accepted", EasyNPCPauseHandler.pause(easyNPC));
    GameTestHelpers.assertTrue(
        helper, "A paused NPC must report its state", EasyNPCPauseHandler.isPaused(easyNPC));
    GameTestHelpers.assertTrue(helper, "A paused NPC must stop its AI", easyNPC.getMob().isNoAi());

    GameTestHelpers.assertTrue(
        helper, "Resuming an NPC must be accepted", EasyNPCPauseHandler.resume(easyNPC));
    GameTestHelpers.assertTrue(
        helper, "A resumed NPC must run again", !EasyNPCPauseHandler.isPaused(easyNPC));
    GameTestHelpers.assertTrue(
        helper, "A resumed NPC must get its AI back", !easyNPC.getMob().isNoAi());
  }

  public static void assertPausedNPCSkipsItsBaseTick(
      GameTestHelper helper, EntityType<?> entityType) {
    PauseManager.reset();
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    scheduleDelayedState(easyNPC);
    EasyNPCPauseHandler.pause(easyNPC);

    baseTick(easyNPC, WAIT_TICKS * 2);

    GameTestHelpers.assertEquals(
        helper,
        "A paused NPC must not continue its pending actions",
        0,
        delayedStateNumber(easyNPC));

    EasyNPCPauseHandler.resume(easyNPC);
    baseTick(easyNPC, WAIT_TICKS);

    GameTestHelpers.assertEquals(
        helper, "A resumed NPC must continue its pending actions", 1, delayedStateNumber(easyNPC));
  }

  public static void assertPauseSurvivesSaveAndLoad(
      GameTestHelper helper, EntityType<?> entityType) {
    PauseManager.reset();
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    EasyNPCPauseHandler.pause(easyNPC);

    TagValueOutput valueOutput =
        TagValueOutput.createWithContext(
            ProblemReporter.DISCARDING, helper.getLevel().registryAccess());
    easyNPC.getEasyNPCStatusData().addAdditionalStatusData(valueOutput);

    EasyNPC<?> reloadedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION);
    reloadedNPC
        .getEasyNPCStatusData()
        .readAdditionalStatusData(
            TagValueInput.create(
                ProblemReporter.DISCARDING,
                helper.getLevel().registryAccess(),
                valueOutput.buildResult()));

    GameTestHelpers.assertTrue(
        helper,
        "A paused NPC must still be paused after a save and load",
        reloadedNPC.getEasyNPCStatusData().getStatusDataFlag(StatusDataType.PAUSED));

    baseTick(reloadedNPC, 1);

    GameTestHelpers.assertTrue(
        helper, "A loaded paused NPC must stop its AI again", reloadedNPC.getMob().isNoAi());
  }

  public static void assertGlobalPauseCoversEveryNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    PauseManager.reset();
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    EasyNPCPauseHandler.setGlobalPause(true);

    GameTestHelpers.assertTrue(
        helper, "The global pause must be reported", EasyNPCPauseHandler.isGlobalPause());
    GameTestHelpers.assertTrue(
        helper,
        "A global pause must pause an NPC which is not paused itself",
        EasyNPCPauseHandler.isPaused(easyNPC));
    GameTestHelpers.assertTrue(
        helper, "A global pause must stop the AI of a loaded NPC", easyNPC.getMob().isNoAi());

    EasyNPC<?> lateNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION);
    GameTestHelpers.assertTrue(
        helper,
        "A global pause must also cover an NPC which is loaded later",
        EasyNPCPauseHandler.isPaused(lateNPC));

    EasyNPCPauseHandler.setGlobalPause(false);

    GameTestHelpers.assertTrue(
        helper, "A global resume must release the NPC", !EasyNPCPauseHandler.isPaused(easyNPC));
    GameTestHelpers.assertTrue(
        helper, "A global resume must give the AI back", !easyNPC.getMob().isNoAi());
  }

  public static void assertGlobalResumeKeepsIndividualPause(
      GameTestHelper helper, EntityType<?> entityType) {
    PauseManager.reset();
    EasyNPC<?> pausedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    EasyNPC<?> runningNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION);
    EasyNPCPauseHandler.pause(pausedNPC);

    EasyNPCPauseHandler.setGlobalPause(true);
    EasyNPCPauseHandler.setGlobalPause(false);

    GameTestHelpers.assertTrue(
        helper,
        "An individually paused NPC must stay paused after a global resume",
        EasyNPCPauseHandler.isPaused(pausedNPC));
    GameTestHelpers.assertTrue(
        helper,
        "An individually paused NPC must keep its AI off after a global resume",
        pausedNPC.getMob().isNoAi());
    GameTestHelpers.assertTrue(
        helper,
        "Every other NPC must run again after a global resume",
        !EasyNPCPauseHandler.isPaused(runningNPC));
  }

  public static void assertGlobalPauseEndsWithTheServer(
      GameTestHelper helper, EntityType<?> entityType) {
    PauseManager.reset();
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    EasyNPCPauseHandler.setGlobalPause(true);

    PauseManager.reset();

    GameTestHelpers.assertTrue(
        helper, "A global pause must not survive the server", !EasyNPCPauseHandler.isGlobalPause());
    GameTestHelpers.assertTrue(
        helper,
        "An NPC must run again once the global pause is gone",
        !EasyNPCPauseHandler.isPaused(easyNPC));
  }
}
