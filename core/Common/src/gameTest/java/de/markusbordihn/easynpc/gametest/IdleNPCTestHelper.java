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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomLookAtPlayerGoal;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.phys.Vec3;

public class IdleNPCTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final BlockPos GROUND_POSITION = new BlockPos(1, 1, 1);
  private static final Vec3 NEARBY_PLAYER_POSITION = new Vec3(1, 2, 2);
  private static final double NEARBY_PLAYER_DISTANCE = 1.0;
  private static final Vec3 WALKING_INPUT = new Vec3(0, 0, 0.5);
  private static final Vec3 HORIZONTAL_KNOCKBACK = new Vec3(0.2, 0, 0);
  private static final Vec3 JUMP_MOVEMENT = new Vec3(0, 0.42, 0);
  private static final int HURT_TICKS = 10;
  private static final int SEARCH_START_TICK = 100;
  private static final int FAILED_SEARCH_COOLDOWN_TICKS = 4;

  private IdleNPCTestHelper() {}

  public static void assertIdleNPCSkipsTravel(GameTestHelper helper, EntityType<?> entityType) {
    Mob mob = spawnIdleNPC(helper, entityType).getMob();
    Vec3 positionBefore = mob.position();

    mob.travel(Vec3.ZERO);

    GameTestHelpers.assertEquals(
        helper,
        "A skipped travel must keep the idle NPC in place",
        positionBefore,
        mob.position());
    GameTestHelpers.assertEquals(
        helper,
        "A skipped travel must not apply gravity to the idle NPC",
        Vec3.ZERO,
        mob.getDeltaMovement());
  }

  public static void assertPeriodicTickRunsTravel(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnIdleNPC(helper, entityType);
    Mob mob = easyNPC.getMob();
    setTickCountWithPhysicsStep(mob);

    GameTestHelpers.assertTrue(
        helper,
        "The periodic physics tick must not skip the travel of an idle NPC",
        !easyNPC.getEasyNPCNavigationData().canSkipIdleTravel(Vec3.ZERO));
    mob.travel(Vec3.ZERO);
    GameTestHelpers.assertTrue(
        helper,
        "The periodic physics tick must apply gravity to the idle NPC",
        mob.getDeltaMovement().y < 0);
  }

  public static void assertMovingNPCRunsTravel(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnIdleNPC(helper, entityType);
    NavigationDataCapable<?> navigationData = easyNPC.getEasyNPCNavigationData();
    Mob mob = easyNPC.getMob();

    GameTestHelpers.assertTrue(
        helper,
        "Movement input must not skip the travel",
        !navigationData.canSkipIdleTravel(WALKING_INPUT));

    mob.setDeltaMovement(HORIZONTAL_KNOCKBACK);
    GameTestHelpers.assertTrue(
        helper,
        "A horizontal movement must not skip the travel",
        !navigationData.canSkipIdleTravel(Vec3.ZERO));

    mob.setDeltaMovement(JUMP_MOVEMENT);
    GameTestHelpers.assertTrue(
        helper,
        "An upward movement must not skip the travel",
        !navigationData.canSkipIdleTravel(Vec3.ZERO));
  }

  public static void assertAirborneOrHurtNPCRunsTravel(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnIdleNPC(helper, entityType);
    NavigationDataCapable<?> navigationData = easyNPC.getEasyNPCNavigationData();
    Mob mob = easyNPC.getMob();

    mob.setOnGround(false);
    GameTestHelpers.assertTrue(
        helper,
        "An NPC in the air must not skip the travel",
        !navigationData.canSkipIdleTravel(Vec3.ZERO));

    mob.setOnGround(true);
    mob.hurtTime = HURT_TICKS;
    GameTestHelpers.assertTrue(
        helper,
        "A hurt NPC must not skip the travel",
        !navigationData.canSkipIdleTravel(Vec3.ZERO));
  }

  public static void assertNearbyPlayerKeepsTravel(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnIdleNPC(helper, entityType);
    Mob mob = easyNPC.getMob();
    GameTestHelpers.mockServerPlayer(helper, NEARBY_PLAYER_POSITION);

    GameTestHelpers.assertTrue(
        helper,
        "A player within the given range must be found",
        CustomLookAtPlayerGoal.hasPlayerInRange(mob, NEARBY_PLAYER_DISTANCE + 0.5));
    GameTestHelpers.assertTrue(
        helper,
        "A player outside the given range must not be found",
        !CustomLookAtPlayerGoal.hasPlayerInRange(mob, NEARBY_PLAYER_DISTANCE - 0.5));
    GameTestHelpers.assertTrue(
        helper,
        "A player within the look distance must not skip the travel",
        !easyNPC.getEasyNPCNavigationData().canSkipIdleTravel(Vec3.ZERO));
  }

  public static void assertSpectatorIsIgnored(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = spawnIdleNPC(helper, entityType);
    GameTestHelpers.mockSpectatorServerPlayer(helper, NEARBY_PLAYER_POSITION, "test-spectator");

    GameTestHelpers.assertTrue(
        helper,
        "A spectator must not count as a player in range",
        !CustomLookAtPlayerGoal.hasPlayerInRange(
            easyNPC.getMob(), ObjectiveDataEntry.DEFAULT_LOOK_DISTANCE));
    GameTestHelpers.assertTrue(
        helper,
        "A spectator must not stop an idle NPC from skipping the travel",
        easyNPC.getEasyNPCNavigationData().canSkipIdleTravel(Vec3.ZERO));
  }

  public static void assertLookGoalWaitsAfterFailedSearch(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Mob mob = easyNPC.getMob();
    CustomLookAtPlayerGoal<?> lookAtPlayerGoal =
        new CustomLookAtPlayerGoal<>(
            easyNPC, Player.class, ObjectiveDataEntry.DEFAULT_LOOK_DISTANCE, 1.0F);

    mob.tickCount = SEARCH_START_TICK;
    GameTestHelpers.assertTrue(
        helper, "Without a player the look goal must not start", !lookAtPlayerGoal.canUse());

    GameTestHelpers.mockServerPlayer(helper, NEARBY_PLAYER_POSITION);
    mob.tickCount = SEARCH_START_TICK + FAILED_SEARCH_COOLDOWN_TICKS - 1;
    GameTestHelpers.assertTrue(
        helper,
        "The look goal must not search again before its cooldown ended",
        !lookAtPlayerGoal.canUse());

    mob.tickCount = SEARCH_START_TICK + FAILED_SEARCH_COOLDOWN_TICKS;
    GameTestHelpers.assertTrue(
        helper,
        "The look goal must find the new player once its cooldown ended",
        lookAtPlayerGoal.canUse());
  }

  private static EasyNPC<?> spawnIdleNPC(GameTestHelper helper, EntityType<?> entityType) {
    helper.setBlock(GROUND_POSITION, Blocks.STONE);
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Mob mob = easyNPC.getMob();
    mob.setOnGround(true);
    mob.setDeltaMovement(Vec3.ZERO);
    setTickCountWithoutPhysicsStep(mob);
    GameTestHelpers.assertTrue(
        helper,
        "An idle NPC on the ground must skip the travel",
        easyNPC.getEasyNPCNavigationData().canSkipIdleTravel(Vec3.ZERO));
    return easyNPC;
  }

  private static void setTickCountWithPhysicsStep(Mob mob) {
    mob.tickCount = physicsStepOffset(mob);
  }

  private static void setTickCountWithoutPhysicsStep(Mob mob) {
    mob.tickCount = physicsStepOffset(mob) + 1;
  }

  private static int physicsStepOffset(Mob mob) {
    int interval = NavigationDataCapable.IDLE_TRAVEL_PHYSICS_INTERVAL;
    return interval - Math.floorMod(mob.getId(), interval);
  }
}
