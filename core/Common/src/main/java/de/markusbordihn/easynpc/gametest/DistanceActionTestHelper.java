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

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.ActionManager;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.StateDataCapable;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.phys.Vec3;

public class DistanceActionTestHelper {

  private static final ResourceLocation FAR_STATE = StateIdentifier.parse("gametest_distance_far");
  private static final ResourceLocation TOUCH_STATE =
      StateIdentifier.parse("gametest_distance_touch");
  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 TOUCH_POSITION = new Vec3(1, 2, 1.5);
  private static final Vec3 OUT_OF_TOUCH_POSITION = new Vec3(4, 2, 4);

  private DistanceActionTestHelper() {}

  public static void assertDistanceActionsTriggerPerRange(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = createNPCWithDistanceActions(helper, entityType);
    Mob mob = easyNPC.getMob();
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, TOUCH_POSITION);
    StateDataCapable<?> stateData = easyNPC.getEasyNPCStateData();

    easyNPC.getEasyNPCActionHandler().checkDistanceActions();

    GameTestHelpers.assertTrue(
        helper,
        "A player next to the NPC must trigger the touch action",
        ActionManager.containsPlayer(
            mob, ActionEventType.ON_DISTANCE_TOUCH.getActionGroup(), serverPlayer));
    GameTestHelpers.assertTrue(
        helper,
        "A player next to the NPC must also trigger the far action",
        ActionManager.containsPlayer(
            mob, ActionEventType.ON_DISTANCE_FAR.getActionGroup(), serverPlayer));
    GameTestHelpers.assertEquals(
        helper, "The touch action must have run once", 1, stateData.getStateNumber(TOUCH_STATE));
    GameTestHelpers.assertTrue(
        helper, "The far action must have run", stateData.getStateNumber(FAR_STATE) > 0);
  }

  public static void assertDistanceActionsRunOncePerPlayer(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = createNPCWithDistanceActions(helper, entityType);
    GameTestHelpers.mockServerPlayer(helper, TOUCH_POSITION);

    easyNPC.getEasyNPCActionHandler().checkDistanceActions();
    easyNPC.getEasyNPCActionHandler().checkDistanceActions();

    GameTestHelpers.assertEquals(
        helper,
        "A player that stays in range must not trigger the action again",
        1,
        easyNPC.getEasyNPCStateData().getStateNumber(TOUCH_STATE));
  }

  public static void assertDistanceActionsTriggerAgainAfterLeaving(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = createNPCWithDistanceActions(helper, entityType);
    Mob mob = easyNPC.getMob();
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, TOUCH_POSITION);

    easyNPC.getEasyNPCActionHandler().checkDistanceActions();
    serverPlayer.setPos(helper.absoluteVec(OUT_OF_TOUCH_POSITION));
    easyNPC.getEasyNPCActionHandler().checkDistanceActions();

    GameTestHelpers.assertTrue(
        helper,
        "A player that left the touch range must be released from its action group",
        !ActionManager.containsPlayer(
            mob, ActionEventType.ON_DISTANCE_TOUCH.getActionGroup(), serverPlayer));
    GameTestHelpers.assertTrue(
        helper,
        "A player that is still in the far range must stay in its action group",
        ActionManager.containsPlayer(
            mob, ActionEventType.ON_DISTANCE_FAR.getActionGroup(), serverPlayer));

    serverPlayer.setPos(helper.absoluteVec(TOUCH_POSITION));
    easyNPC.getEasyNPCActionHandler().checkDistanceActions();

    GameTestHelpers.assertEquals(
        helper,
        "A player that returns must trigger the touch action again",
        2,
        easyNPC.getEasyNPCStateData().getStateNumber(TOUCH_STATE));
  }

  private static EasyNPC<?> createNPCWithDistanceActions(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    GameTestHelpers.assertNotNull(helper, "NPC must support states", easyNPC.getEasyNPCStateData());

    ActionEventSet actionEventSet = new ActionEventSet();
    actionEventSet.setActionEvent(
        ActionEventType.ON_DISTANCE_TOUCH, stateAction("increase gametest_distance_touch 1"));
    actionEventSet.setActionEvent(
        ActionEventType.ON_DISTANCE_FAR, stateAction("increase gametest_distance_far 1"));
    easyNPC.getEasyNPCActionEventData().setActionEventSet(actionEventSet);

    return easyNPC;
  }

  private static ActionDataSet stateAction(String command) {
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(new ActionDataEntry(ActionDataType.NPC_STATE, command));
    return actionDataSet;
  }
}
