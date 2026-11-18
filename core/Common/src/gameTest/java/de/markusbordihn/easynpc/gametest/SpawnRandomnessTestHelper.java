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

import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import java.util.List;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LightningBolt;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.PatrollingMonster;
import net.minecraft.world.entity.projectile.hurtingprojectile.LargeFireball;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.phys.Vec3;

public class SpawnRandomnessTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 1, 1);

  private static final Vec3 PLAYER_POSITION = new Vec3(2, 1, 1);

  private static final int SPAWN_ROUNDS = 100;

  private static final float REFLECTED_FIREBALL_DAMAGE = 6.0F;

  private SpawnRandomnessTestHelper() {}

  public static void assertPiglinNPCSpawnsWithoutRandomBabyOrEquipment(
      GameTestHelper helper, EntityType<?> entityType) {
    Mob piglin = mockNPC(helper, entityType);
    for (int spawnRound = 0; spawnRound < SPAWN_ROUNDS; spawnRound++) {
      finalizeSpawn(helper, piglin);
      GameTestHelpers.assertTrue(
          helper, "A piglin NPC must not become a baby on spawn", !piglin.isBaby());
      for (EquipmentSlot equipmentSlot : EquipmentSlot.values()) {
        GameTestHelpers.assertTrue(
            helper,
            "A piglin NPC must not get random equipment in " + equipmentSlot,
            piglin.getItemBySlot(equipmentSlot).isEmpty());
      }
    }
  }

  public static void assertPiglinNPCKeepsConfiguredAgeAndEquipmentOnSpawn(
      GameTestHelper helper, EntityType<?> entityType) {
    Mob adultPiglin = mockNPC(helper, entityType);
    adultPiglin.setItemSlot(EquipmentSlot.MAINHAND, new ItemStack(Items.STICK));
    finalizeSpawn(helper, adultPiglin);
    GameTestHelpers.assertTrue(
        helper,
        "A piglin NPC must keep its configured main hand item and age on spawn",
        adultPiglin.getMainHandItem().is(Items.STICK) && !adultPiglin.isBaby());

    Mob babyPiglin = mockNPC(helper, entityType);
    babyPiglin.setBaby(true);
    finalizeSpawn(helper, babyPiglin);
    GameTestHelpers.assertTrue(
        helper, "A baby piglin NPC must stay a baby on spawn", babyPiglin.isBaby());
  }

  public static void assertSpiderNPCSpawnsWithoutJockeyOrEffect(
      GameTestHelper helper, EntityType<?> entityType) {
    Mob spider = mockNPC(helper, entityType);
    Entity configuredPassenger =
        EntityType.CHICKEN.create(helper.getLevel(), EntitySpawnReason.TRIGGERED);
    GameTestHelpers.assertNotNull(helper, "Configured passenger", configuredPassenger);
    configuredPassenger.startRiding(spider);
    for (int spawnRound = 0; spawnRound < SPAWN_ROUNDS; spawnRound++) {
      finalizeSpawn(helper, spider);
      GameTestHelpers.assertEquals(
          helper,
          "A spider NPC must keep only its configured passenger on spawn",
          List.of(configuredPassenger),
          spider.getPassengers());
      GameTestHelpers.assertTrue(
          helper,
          "A spider NPC must not get a random effect on spawn",
          spider.getActiveEffects().isEmpty());
    }
  }

  public static void assertIllagerNPCNeverBecomesPatrolLeader(
      GameTestHelper helper, EntityType<?> entityType) {
    PatrollingMonster illager = (PatrollingMonster) mockNPC(helper, entityType);
    for (int spawnRound = 0; spawnRound < SPAWN_ROUNDS; spawnRound++) {
      finalizeSpawn(helper, illager);
      GameTestHelpers.assertTrue(
          helper,
          "An illager NPC must not become a patrol leader with a banner on spawn",
          !illager.isPatrolLeader() && illager.getItemBySlot(EquipmentSlot.HEAD).isEmpty());
    }
  }

  public static void assertCreeperNPCStaysUnchargedByLightning(
      GameTestHelper helper, EntityType<?> entityType) {
    Creeper creeper = (Creeper) mockNPC(helper, entityType);
    LightningBolt lightningBolt =
        EntityType.LIGHTNING_BOLT.create(helper.getLevel(), EntitySpawnReason.TRIGGERED);
    creeper.thunderHit(helper.getLevel(), lightningBolt);

    GameTestHelpers.assertTrue(
        helper, "Lightning must not charge a creeper NPC", !creeper.isPowered());
  }

  public static void assertGhastNPCSurvivesReflectedFireball(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setCombatAttribute(
        easyNPC, CombatAttributeType.IS_ATTACKABLE_BY_PLAYERS, true);
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);
    LargeFireball largeFireball =
        EntityType.FIREBALL.create(helper.getLevel(), EntitySpawnReason.TRIGGERED);
    DamageSource reflectedFireball =
        helper.getLevel().damageSources().fireball(largeFireball, serverPlayer);
    Mob ghast = easyNPC.getMob();
    ghast.hurtServer(helper.getLevel(), reflectedFireball, REFLECTED_FIREBALL_DAMAGE);

    GameTestHelpers.assertTrue(
        helper,
        "A reflected fireball must hurt a ghast NPC like any other attack, not kill it",
        ghast.isAlive() && ghast.getHealth() < ghast.getMaxHealth());
  }

  private static Mob mockNPC(GameTestHelper helper, EntityType<?> entityType) {
    return GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION).getMob();
  }

  private static void finalizeSpawn(GameTestHelper helper, Mob mob) {
    ServerLevel serverLevel = helper.getLevel();
    mob.finalizeSpawn(
        serverLevel,
        serverLevel.getCurrentDifficultyAt(mob.blockPosition()),
        EntitySpawnReason.SPAWN_ITEM_USE,
        null);
  }
}
