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

import de.markusbordihn.easynpc.api.handler.EasyNPCObjectiveHandler;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.objective.ObjectiveUtils;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.animal.Wolf;
import net.minecraft.world.phys.Vec3;

public class AssistOwnerTestHelper {

  // Forge 1.21.1 places the structure one block higher; y=2 puts the eyes into the barrier ceiling.
  private static final Vec3 NPC_POSITION = new Vec3(2, 1, 1);
  private static final Vec3 OWNER_POSITION = new Vec3(2, 1, 2);
  private static final Vec3 TARGET_POSITION = new Vec3(1, 1, 1);
  private static final int OWNER_HURT_TICK = 1;

  private AssistOwnerTestHelper() {}

  public static void assertAssistOwnerTargetsWhatTheOwnerAttacks(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer owner =
        GameTestHelpers.mockSurvivalServerPlayer(helper, OWNER_POSITION, "assist-owner");
    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(owner.getUUID());
    GameTestHelpers.assertTrue(
        helper,
        "The Assist Owner objective must be stored",
        EasyNPCObjectiveHandler.addObjective(easyNPC, ObjectiveType.OWNER_HURT_TARGET));

    LivingEntity target = spawnTarget(helper);
    // Like vanilla OwnerHurtTargetGoal, the goal ignores a hurt timestamp equal to its initial 0.
    owner.tickCount = OWNER_HURT_TICK;
    owner.setLastHurtMob(target);

    Goal assistOwnerGoal = createAssistOwnerGoal(easyNPC);
    GameTestHelpers.assertNotNull(helper, "The Assist Owner goal must exist", assistOwnerGoal);
    GameTestHelpers.assertTrue(
        helper, "The NPC must react to a mob its owner attacked", assistOwnerGoal.canUse());

    assistOwnerGoal.start();
    GameTestHelpers.assertTrue(
        helper,
        "The NPC must target the mob its owner attacked",
        easyNPC.getMob().getTarget() == target);
  }

  public static void assertAssistOwnerRequiresOwner(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    Goal assistOwnerGoal = createAssistOwnerGoal(easyNPC);
    GameTestHelpers.assertNotNull(helper, "The Assist Owner goal must exist", assistOwnerGoal);
    GameTestHelpers.assertTrue(
        helper, "An NPC without an owner must not assist anyone", !assistOwnerGoal.canUse());
  }

  public static void assertOwnerGoalsIgnoreAllies(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer owner =
        GameTestHelpers.mockSurvivalServerPlayer(helper, OWNER_POSITION, "assist-ally-owner");
    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(owner.getUUID());

    EasyNPC<?> alliedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, TARGET_POSITION);
    alliedNPC.getEasyNPCOwnerData().setNPCOwnerUUID(owner.getUUID());
    Wolf tamedWolf = GameTestHelpers.spawnEntityType(helper, EntityType.WOLF);
    tamedWolf.setPos(helper.absoluteVec(TARGET_POSITION));
    tamedWolf.setNoAi(true);
    tamedWolf.tame(owner);

    owner.tickCount = OWNER_HURT_TICK;
    for (LivingEntity ally : new LivingEntity[] {alliedNPC.getLivingEntity(), tamedWolf}) {
      owner.setLastHurtMob(ally);
      owner.setLastHurtByMob(ally);
      GameTestHelpers.assertTrue(
          helper,
          "The NPC must not attack an ally its owner attacked: " + ally,
          !createAssistOwnerGoal(easyNPC).canUse());
      GameTestHelpers.assertTrue(
          helper,
          "The NPC must not attack an ally which attacked its owner: " + ally,
          !createObjectiveGoal(easyNPC, ObjectiveType.OWNER_HURT_BY_TARGET).canUse());
    }
  }

  private static Goal createAssistOwnerGoal(EasyNPC<?> easyNPC) {
    return createObjectiveGoal(easyNPC, ObjectiveType.OWNER_HURT_TARGET);
  }

  private static Goal createObjectiveGoal(EasyNPC<?> easyNPC, ObjectiveType objectiveType) {
    return ObjectiveUtils.createObjectiveTarget(new ObjectiveDataEntry(objectiveType), easyNPC);
  }

  private static LivingEntity spawnTarget(GameTestHelper helper) {
    Mob target = GameTestHelpers.spawnEntityType(helper, EntityType.ZOMBIE);
    target.setPos(helper.absoluteVec(TARGET_POSITION));
    target.setNoAi(true);
    return target;
  }
}
