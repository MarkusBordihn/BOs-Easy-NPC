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
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.RelationshipType;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import java.util.List;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class IntervalActionAudienceTestHelper {

  private static final ResourceLocation GREETED_STATE =
      StateIdentifier.parse("gametest_interval_greeted");
  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 NEAR_POSITION = new Vec3(2, 2, 1);
  private static final Vec3 FAR_POSITION = new Vec3(1, 2, 3);

  private IntervalActionAudienceTestHelper() {}

  public static void assertAudienceContainsEveryPlayerInRange(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer nearPlayer = GameTestHelpers.mockServerPlayer(helper, NEAR_POSITION, "near");
    ServerPlayer farPlayer = GameTestHelpers.mockServerPlayer(helper, FAR_POSITION, "far");
    ActionHandler<?> actionHandler = easyNPC.getEasyNPCActionHandler();

    List<ServerPlayer> audience =
        actionHandler.getServerPlayersInRange(ActionHandler.INTERVAL_ACTION_RANGE);

    GameTestHelpers.assertTrue(
        helper, "The audience must contain the nearby player", audience.contains(nearPlayer));
    GameTestHelpers.assertTrue(
        helper,
        "The audience must contain the second player in range",
        audience.contains(farPlayer));
    GameTestHelpers.assertTrue(
        helper,
        "The nearest player comes first",
        audience.indexOf(nearPlayer) < audience.indexOf(farPlayer));

    List<ServerPlayer> narrowAudience = actionHandler.getServerPlayersInRange(1.5D);

    GameTestHelpers.assertTrue(
        helper,
        "A narrow range must still contain the nearby player",
        narrowAudience.contains(nearPlayer));
    GameTestHelpers.assertTrue(
        helper,
        "A narrow range must leave out the player behind it",
        !narrowAudience.contains(farPlayer));
  }

  public static void assertOwnerIsPreferredAsInitiator(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer stranger = GameTestHelpers.mockServerPlayer(helper, NEAR_POSITION, "stranger");
    ServerPlayer owner = GameTestHelpers.mockServerPlayer(helper, FAR_POSITION, "owner");
    easyNPC.getEasyNPCOwnerData().setNPCOwner(owner);

    ActionHandler<?> actionHandler = easyNPC.getEasyNPCActionHandler();

    GameTestHelpers.assertEquals(
        helper,
        "The owner is preferred over the nearer stranger",
        owner,
        actionHandler.getPreferredServerPlayer(List.of(stranger, owner)));
    GameTestHelpers.assertEquals(
        helper,
        "Without the owner the nearest player stays the initiator",
        stranger,
        actionHandler.getPreferredServerPlayer(List.of(stranger)));
  }

  public static void assertOwnerConditionLimitsTheEvent(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer stranger = GameTestHelpers.mockServerPlayer(helper, NEAR_POSITION, "stranger");
    ServerPlayer owner = GameTestHelpers.mockServerPlayer(helper, FAR_POSITION, "owner");
    easyNPC.getEasyNPCOwnerData().setNPCOwner(owner);

    ActionHandler<?> actionHandler = easyNPC.getEasyNPCActionHandler();
    actionHandler.executeRandomAction(
        ownerOnlyGreeting(),
        ActionContext.of(ActionEventType.ON_INTERVAL_SHORT, stranger, List.of(stranger, owner)));

    GameTestHelpers.assertEquals(
        helper,
        "The owner condition must run the action for the owner even with a stranger closer by",
        1,
        easyNPC.getEasyNPCStateData().getStateNumber(GREETED_STATE));
  }

  public static void assertNothingFiresWithoutTheOwner(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer stranger = GameTestHelpers.mockServerPlayer(helper, NEAR_POSITION, "stranger");
    ServerPlayer owner = GameTestHelpers.mockServerPlayer(helper, FAR_POSITION, "owner");
    easyNPC.getEasyNPCOwnerData().setNPCOwner(owner);

    ActionHandler<?> actionHandler = easyNPC.getEasyNPCActionHandler();
    actionHandler.executeRandomAction(
        ownerOnlyGreeting(),
        ActionContext.of(ActionEventType.ON_INTERVAL_SHORT, stranger, List.of(stranger)));

    GameTestHelpers.assertEquals(
        helper,
        "Without the owner in range nothing must happen",
        0,
        easyNPC.getEasyNPCStateData().getStateNumber(GREETED_STATE));
  }

  private static ActionDataSet ownerOnlyGreeting() {
    ConditionDataSet conditionDataSet = new ConditionDataSet();
    conditionDataSet.add(
        new ConditionDataEntry(ConditionType.RELATIONSHIP).withSubType(RelationshipType.OWNER));

    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(
        new ActionDataEntry(ActionDataType.NPC_STATE, "increase gametest_interval_greeted 1")
            .withConditionDataSet(conditionDataSet));
    return actionDataSet;
  }
}
