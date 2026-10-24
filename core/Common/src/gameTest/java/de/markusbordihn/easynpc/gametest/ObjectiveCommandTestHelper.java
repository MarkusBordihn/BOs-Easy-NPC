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

import de.markusbordihn.easynpc.api.handler.EasyNPCAttributeHandler;
import de.markusbordihn.easynpc.api.handler.EasyNPCObjectiveHandler;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.List;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.WrappedGoal;
import net.minecraft.world.phys.Vec3;

public class ObjectiveCommandTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final int EXPLICIT_PRIORITY = 4;
  private static final float SHOULDER_MAX_DISTANCE = 4.0F;
  private static final int SHOULDER_COOLDOWN_TICKS = 40;

  private ObjectiveCommandTestHelper() {}

  private static EasyNPC<?> mockSpawnedNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();
    return easyNPC;
  }

  private static int executeConsoleCommand(GameTestHelper helper, String command) {
    return GameTestHelpers.executeCommand(
        helper, GameTestHelpers.consoleCommandSource(helper, NPC_POSITION), command);
  }

  private static List<WrappedGoal> getRegisteredGoals(EasyNPC<?> easyNPC, Goal goal) {
    return easyNPC.getEntityGoalSelector().getAvailableGoals().stream()
        .filter(availableGoal -> availableGoal.getGoal() == goal)
        .toList();
  }

  private static EasyNPC<?> mockNPCFollowingOwner(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);
    ServerPlayer owner =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), "objective_owner");
    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(owner.getUUID());

    int result =
        executeConsoleCommand(
            helper, "easy_npc objective " + easyNPC.getEntityUUID() + " set follow owner");

    GameTestHelpers.assertEquals(helper, "Follow owner command result", 1, result);
    return easyNPC;
  }

  public static void assertShoulderObjectiveCommandSetsOptions(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);
    EasyNPCAttributeHandler.setInteractionAttribute(
        easyNPC, InteractionAttributeType.CAN_SIT_ON_SHOULDER, true);

    int result =
        executeConsoleCommand(
            helper,
            "easy_npc objective "
                + easyNPC.getEntityUUID()
                + " set owner_shoulder "
                + SHOULDER_MAX_DISTANCE
                + " "
                + SHOULDER_COOLDOWN_TICKS
                + " true");

    GameTestHelpers.assertEquals(helper, "Shoulder objective command result", 1, result);
    ObjectiveDataEntry objectiveDataEntry =
        EasyNPCObjectiveHandler.getObjective(easyNPC, ObjectiveType.SIT_ON_OWNER_SHOULDER.name());
    GameTestHelpers.assertNotNull(
        helper, "Shoulder objective is missing after the command", objectiveDataEntry);
    GameTestHelpers.assertEquals(
        helper,
        "Shoulder objective max distance",
        SHOULDER_MAX_DISTANCE,
        objectiveDataEntry.getStartDistance());
    GameTestHelpers.assertEquals(
        helper,
        "Shoulder objective cooldown",
        SHOULDER_COOLDOWN_TICKS,
        objectiveDataEntry.getCooldownTicks());
    GameTestHelpers.assertTrue(
        helper,
        "Shoulder objective ignores only out of combat",
        objectiveDataEntry.getOnlyOutOfCombat());
  }

  public static void assertShoulderObjectiveCommandRequiresEnabledShoulder(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);

    int result =
        executeConsoleCommand(
            helper, "easy_npc objective " + easyNPC.getEntityUUID() + " set owner_shoulder");

    GameTestHelpers.assertEquals(helper, "Shoulder objective command result", 0, result);
    GameTestHelpers.assertTrue(
        helper,
        "Shoulder objective command added the objective while sitting on shoulders is disabled",
        !EasyNPCObjectiveHandler.hasObjective(easyNPC, ObjectiveType.SIT_ON_OWNER_SHOULDER));
  }

  public static void assertPriorityCommandReachesGoalSelector(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockNPCFollowingOwner(helper, entityType);

    int result =
        executeConsoleCommand(
            helper,
            "easy_npc objective "
                + easyNPC.getEntityUUID()
                + " priority follow_owner "
                + EXPLICIT_PRIORITY);

    GameTestHelpers.assertEquals(helper, "Objective priority command result", 1, result);
    ObjectiveDataEntry objectiveDataEntry =
        EasyNPCObjectiveHandler.getObjective(easyNPC, ObjectiveType.FOLLOW_OWNER.name());
    GameTestHelpers.assertNotNull(
        helper, "Follow owner objective is missing after the command", objectiveDataEntry);
    List<WrappedGoal> registeredGoals =
        getRegisteredGoals(easyNPC, objectiveDataEntry.getGoal(easyNPC));
    GameTestHelpers.assertEquals(
        helper, "Registered follow owner goals", 1, registeredGoals.size());
    GameTestHelpers.assertEquals(
        helper,
        "Follow owner goal ignores the priority command",
        EXPLICIT_PRIORITY,
        registeredGoals.get(0).getPriority());
  }

  public static void assertPriorityCommandRejectsUnknownObjective(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);

    int result =
        executeConsoleCommand(
            helper,
            "easy_npc objective " + easyNPC.getEntityUUID() + " priority unknown_objective 4");

    GameTestHelpers.assertEquals(helper, "Objective priority command result", 0, result);
  }

  public static void assertRepeatedFollowCommandKeepsOneGoal(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockNPCFollowingOwner(helper, entityType);
    Goal replacedGoal =
        EasyNPCObjectiveHandler.getObjective(easyNPC, ObjectiveType.FOLLOW_OWNER.name())
            .getGoal(easyNPC);

    int result =
        executeConsoleCommand(
            helper, "easy_npc objective " + easyNPC.getEntityUUID() + " set follow owner");

    GameTestHelpers.assertEquals(helper, "Repeated follow owner command result", 1, result);
    GameTestHelpers.assertTrue(
        helper,
        "Goal of the replaced follow owner objective is still registered",
        getRegisteredGoals(easyNPC, replacedGoal).isEmpty());
    Goal currentGoal =
        EasyNPCObjectiveHandler.getObjective(easyNPC, ObjectiveType.FOLLOW_OWNER.name())
            .getGoal(easyNPC);
    GameTestHelpers.assertEquals(
        helper,
        "Registered follow owner goals",
        1,
        getRegisteredGoals(easyNPC, currentGoal).size());

    int removeResult =
        executeConsoleCommand(
            helper, "easy_npc objective " + easyNPC.getEntityUUID() + " remove follow owner");

    GameTestHelpers.assertEquals(helper, "Follow owner remove command result", 1, removeResult);
    GameTestHelpers.assertTrue(
        helper,
        "Follow owner goal is still registered after removing the objective",
        getRegisteredGoals(easyNPC, currentGoal).isEmpty());
  }

  public static void assertAttackCommandAddsAndRemovesObjective(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);

    int setResult =
        executeConsoleCommand(
            helper, "easy_npc objective " + easyNPC.getEntityUUID() + " set attack target monster");

    GameTestHelpers.assertEquals(helper, "Attack set command result", 1, setResult);
    GameTestHelpers.assertTrue(
        helper,
        "Attack set command did not add the objective",
        EasyNPCObjectiveHandler.hasObjective(easyNPC, ObjectiveType.ATTACK_MONSTER));

    int removeResult =
        executeConsoleCommand(
            helper,
            "easy_npc objective " + easyNPC.getEntityUUID() + " remove attack target monster");

    GameTestHelpers.assertEquals(helper, "Attack remove command result", 1, removeResult);
    GameTestHelpers.assertTrue(
        helper,
        "Attack remove command did not remove the objective",
        !EasyNPCObjectiveHandler.hasObjective(easyNPC, ObjectiveType.ATTACK_MONSTER));
  }
}
