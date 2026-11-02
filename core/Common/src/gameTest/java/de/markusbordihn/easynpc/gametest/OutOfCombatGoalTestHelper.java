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

import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.objective.ObjectiveUtils;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.control.JumpEasyNPCMoveControl;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.LookAtAttackTargetGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.MoveBackToHomeGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.OutOfCombatGoal;
import de.markusbordihn.easynpc.handler.ObjectiveHandler;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.WrappedGoal;
import net.minecraft.world.entity.monster.zombie.Zombie;
import net.minecraft.world.phys.Vec3;

public class OutOfCombatGoalTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final BlockPos ZOMBIE_POSITION = new BlockPos(2, 2, 1);
  private static final double WANTED_POSITION_DISTANCE = 5.0D;
  private static final float EAST_FACING_Y_ROT = -90.0F;
  private static final float LOOK_Y_HEAD_ROT = 45.0F;
  private static final float ROTATION_TOLERANCE_DEGREES = 1.0F;
  private static final int LOOK_AT_TARGET_TICKS = 20;
  private static final int HOME_DISTANCE = 20;
  private static final int ALWAYS_TRY_INTERVAL = 1;
  private static final int RETRY_DELAY_TICKS = 40;

  private static final Set<ObjectiveType> OUT_OF_COMBAT_OBJECTIVES =
      EnumSet.of(
          ObjectiveType.MOVE_BACK_TO_HOME,
          ObjectiveType.RANDOM_STROLL,
          ObjectiveType.WATER_AVOIDING_RANDOM_STROLL,
          ObjectiveType.RANDOM_STROLL_AROUND_HOME,
          ObjectiveType.LOOK_AT_PLAYER,
          ObjectiveType.LOOK_RANDOM_AROUND,
          ObjectiveType.LOOK_AT_RESET);

  private static final Set<ObjectiveType> COMBAT_OBJECTIVES =
      EnumSet.of(ObjectiveType.MELEE_ATTACK, ObjectiveType.BOW_ATTACK, ObjectiveType.PANIC);

  private OutOfCombatGoalTestHelper() {}

  public static void assertMoveBackToHomeGoalWaitsBeforeRetry(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC
        .getEasyNPCNavigationData()
        .setNPCHomePosition(
            helper.absolutePos(BlockPos.containing(NPC_POSITION)).east(HOME_DISTANCE));
    MoveBackToHomeGoal<?> moveBackToHomeGoal =
        new MoveBackToHomeGoal<>(easyNPC, 1.0D, 1.0F, ALWAYS_TRY_INTERVAL);
    GameTestHelpers.assertTrue(
        helper, "The goal starts when the NPC is away from home", moveBackToHomeGoal.canUse());

    moveBackToHomeGoal.stop();
    GameTestHelpers.assertTrue(
        helper,
        "The goal waits before the next attempt after stopping away from home",
        !moveBackToHomeGoal.canUse());

    easyNPC.getLivingEntity().tickCount += RETRY_DELAY_TICKS;
    GameTestHelpers.assertTrue(
        helper, "The goal tries again after the retry delay", moveBackToHomeGoal.canUse());
  }

  public static void assertIdleObjectivesPauseInCombat(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    for (ObjectiveType objectiveType : OUT_OF_COMBAT_OBJECTIVES) {
      Goal goal =
          ObjectiveUtils.createObjectiveGoal(new ObjectiveDataEntry(objectiveType), easyNPC);
      GameTestHelpers.assertTrue(
          helper,
          objectiveType + " goal keeps running in combat",
          goal instanceof OutOfCombatGoal);
    }

    for (ObjectiveType objectiveType : COMBAT_OBJECTIVES) {
      Goal goal =
          ObjectiveUtils.createObjectiveGoal(new ObjectiveDataEntry(objectiveType), easyNPC);
      GameTestHelpers.assertTrue(
          helper,
          objectiveType + " goal pauses in combat",
          goal != null && !(goal instanceof OutOfCombatGoal));
    }
  }

  public static void assertOutOfCombatGoalWaitsForCombatCooldown(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Mob mob = easyNPC.getMob();
    Zombie zombie = helper.spawnWithNoFreeWill(EntityType.ZOMBIE, ZOMBIE_POSITION);
    OutOfCombatGoal outOfCombatGoal =
        new OutOfCombatGoal(
            new Goal() {
              @Override
              public boolean canUse() {
                return true;
              }
            },
            mob);

    GameTestHelpers.assertTrue(helper, "Goal is blocked without combat", outOfCombatGoal.canUse());

    mob.setTarget(zombie);
    GameTestHelpers.assertTrue(
        helper, "Goal starts while the NPC has a target", !outOfCombatGoal.canUse());

    mob.setTarget(null);
    GameTestHelpers.assertTrue(
        helper, "Goal starts right after the target is gone", !outOfCombatGoal.canUse());

    mob.tickCount += OutOfCombatGoal.COMBAT_COOLDOWN_TICKS;
    GameTestHelpers.assertTrue(
        helper, "Goal is still blocked after the combat cooldown", outOfCombatGoal.canUse());

    mob.setLastHurtByMob(zombie);
    GameTestHelpers.assertTrue(
        helper, "Goal continues after the NPC was hurt", !outOfCombatGoal.canContinueToUse());

    mob.tickCount += OutOfCombatGoal.COMBAT_COOLDOWN_TICKS;
    GameTestHelpers.assertTrue(
        helper, "Goal is still blocked long after the NPC was hurt", outOfCombatGoal.canUse());

    mob.setLastHurtMob(zombie);
    GameTestHelpers.assertTrue(
        helper, "Goal starts right after the NPC hit a mob", !outOfCombatGoal.canUse());

    mob.tickCount += OutOfCombatGoal.COMBAT_COOLDOWN_TICKS;
    GameTestHelpers.assertTrue(
        helper, "Goal is still blocked long after the NPC hit a mob", outOfCombatGoal.canUse());
  }

  public static void assertLookAtTargetGoalFacesTarget(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Mob mob = easyNPC.getMob();
    Zombie zombie = helper.spawnWithNoFreeWill(EntityType.ZOMBIE, ZOMBIE_POSITION);

    ObjectiveHandler.addOrUpdateCustomObjective(
        easyNPC, new ObjectiveDataEntry(ObjectiveType.HURT_BY_TARGET));
    ObjectiveHandler.addOrUpdateCustomObjective(
        easyNPC, new ObjectiveDataEntry(ObjectiveType.HURT_BY_TARGET));
    List<Goal> lookAtTargetGoals =
        easyNPC.getEntityGoalSelector().getAvailableGoals().stream()
            .map(WrappedGoal::getGoal)
            .filter(LookAtAttackTargetGoal.class::isInstance)
            .toList();
    GameTestHelpers.assertEquals(
        helper, "Target objective registers look at target goals", 1, lookAtTargetGoals.size());

    Goal lookAtTargetGoal = lookAtTargetGoals.get(0);
    GameTestHelpers.assertTrue(
        helper, "Look at target goal starts without a target", !lookAtTargetGoal.canUse());

    mob.setTarget(zombie);
    GameTestHelpers.assertTrue(
        helper, "Look at target goal does not start with a target", lookAtTargetGoal.canUse());

    mob.yHeadRot = 0.0F;
    mob.yBodyRot = 0.0F;
    for (int i = 0; i < LOOK_AT_TARGET_TICKS; i++) {
      lookAtTargetGoal.tick();
      mob.getLookControl().tick();
    }
    double targetDistanceX = zombie.getX() - mob.getX();
    double targetDistanceZ = zombie.getZ() - mob.getZ();
    float targetYRot =
        (float) (Mth.atan2(targetDistanceZ, targetDistanceX) * (180.0 / Math.PI)) - 90.0F;
    GameTestHelpers.assertTrue(
        helper,
        "NPC does not look at its target, yHeadRot " + mob.yHeadRot + " expected " + targetYRot,
        Mth.degreesDifferenceAbs(mob.yHeadRot, targetYRot) < ROTATION_TOLERANCE_DEGREES);
  }

  public static void assertJumpMoveControlFollowsWantedPosition(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Mob mob = easyNPC.getMob();
    GameTestHelpers.assertTrue(
        helper,
        entityType + " does not use the jump move control",
        mob.getMoveControl() instanceof JumpEasyNPCMoveControl);
    JumpEasyNPCMoveControl moveControl = (JumpEasyNPCMoveControl) mob.getMoveControl();
    mob.setYRot(0.0F);

    moveControl.setWantedPosition(
        mob.getX() + WANTED_POSITION_DISTANCE, mob.getY(), mob.getZ(), 1.0D);
    moveControl.tick();
    GameTestHelpers.assertTrue(
        helper,
        "Jumping NPC does not turn towards its navigation target, yRot " + mob.getYRot(),
        Mth.degreesDifferenceAbs(mob.getYRot(), EAST_FACING_Y_ROT) < ROTATION_TOLERANCE_DEGREES);

    moveControl.tick();
    mob.yHeadRot = LOOK_Y_HEAD_ROT;
    moveControl.tick();
    GameTestHelpers.assertEquals(
        helper,
        "Idle jump move control overrides the head rotation",
        LOOK_Y_HEAD_ROT,
        mob.yHeadRot);
  }
}
