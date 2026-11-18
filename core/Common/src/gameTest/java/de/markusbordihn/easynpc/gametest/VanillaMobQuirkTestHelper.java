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

import de.markusbordihn.easynpc.api.pose.ModelPoseAPI;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.Difficulty;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LightningBolt;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.animal.Chicken;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.monster.Slime;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.phys.Vec3;

public class VanillaMobQuirkTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 1, 1);

  private static final double SEARCH_RADIUS = 3.0D;

  private static final int SLIME_SIZE_THAT_SPLITS = 4;

  private static final double SLIME_ATTACK_DAMAGE = 4.0D;

  private static final int AI_STEPS_BEYOND_FOX_EATING_DELAY = 620;

  private static final int CAT_AI_STEPS = 5;

  private static final int SPAWNS_BEYOND_RANDOM_FOX_ITEM_CHANCE = 50;

  private VanillaMobQuirkTestHelper() {}

  public static void assertNPCSurvivesLightning(GameTestHelper helper, EntityType<?> entityType) {
    Mob mob = mockNPC(helper, entityType);
    ServerLevel serverLevel = helper.getLevel();
    LightningBolt lightningBolt = EntityType.LIGHTNING_BOLT.create(serverLevel);
    GameTestHelpers.assertNotNull(helper, "Lightning bolt", lightningBolt);
    MinecraftServer server = serverLevel.getServer();
    Difficulty difficulty = serverLevel.getDifficulty();
    server.setDifficulty(Difficulty.NORMAL, true);
    try {
      mob.thunderHit(serverLevel, lightningBolt);
    } finally {
      server.setDifficulty(difficulty, true);
    }

    GameTestHelpers.assertTrue(
        helper, "A lightning strike must not replace the NPC " + entityType, !mob.isRemoved());
  }

  public static void assertSlimeNPCDoesNotSplitOnDeath(
      GameTestHelper helper, EntityType<?> entityType) {
    Slime slime = (Slime) mockNPC(helper, entityType);
    slime.setSize(SLIME_SIZE_THAT_SPLITS, true);
    slime.setHealth(0.0F);
    slime.remove(Entity.RemovalReason.KILLED);

    GameTestHelpers.assertTrue(
        helper,
        "A dying slime NPC must not split into new slime NPCs",
        helper
            .getLevel()
            .getEntitiesOfClass(
                Slime.class, slime.getBoundingBox().inflate(SEARCH_RADIUS), Entity::isAlive)
            .isEmpty());
  }

  public static void assertSlimeNPCDealsNoTouchDamage(
      GameTestHelper helper, EntityType<?> entityType) {
    Slime slime = (Slime) mockNPC(helper, entityType);
    slime.setSize(SLIME_SIZE_THAT_SPLITS, true);
    slime.getAttribute(Attributes.ATTACK_DAMAGE).setBaseValue(SLIME_ATTACK_DAMAGE);
    LivingEntity ironGolem = GameTestHelpers.spawnEntityType(helper, EntityType.IRON_GOLEM);
    ironGolem.setPos(slime.position());
    float ironGolemHealth = ironGolem.getHealth();
    slime.push(ironGolem);

    GameTestHelpers.assertEquals(
        helper,
        "A slime NPC must not hurt an entity by touching it",
        ironGolemHealth,
        ironGolem.getHealth());
  }

  public static void assertChickenNPCLaysNoEggs(GameTestHelper helper, EntityType<?> entityType) {
    Chicken chicken = (Chicken) mockNPC(helper, entityType);
    chicken.eggTime = 1;
    chicken.aiStep();

    GameTestHelpers.assertTrue(
        helper,
        "A chicken NPC must not lay eggs",
        helper
            .getLevel()
            .getEntitiesOfClass(ItemEntity.class, chicken.getBoundingBox().inflate(SEARCH_RADIUS))
            .isEmpty());
  }

  public static void assertFoxNPCKeepsFoodInItsMouth(
      GameTestHelper helper, EntityType<?> entityType) {
    Mob fox = mockNPC(helper, entityType);
    fox.setItemSlot(EquipmentSlot.MAINHAND, new ItemStack(Items.SWEET_BERRIES));
    for (int aiStep = 0; aiStep < AI_STEPS_BEYOND_FOX_EATING_DELAY; aiStep++) {
      fox.setOnGround(true);
      fox.aiStep();
    }

    GameTestHelpers.assertTrue(
        helper,
        "A fox NPC must not eat the item in its mouth",
        fox.getMainHandItem().is(Items.SWEET_BERRIES));
  }

  public static void assertCatNPCKeepsItsVanillaPose(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ModelPoseAPI.setVanillaPose(easyNPC, Pose.CROUCHING);
    Mob cat = easyNPC.getMob();
    for (int aiStep = 0; aiStep < CAT_AI_STEPS; aiStep++) {
      cat.aiStep();
    }

    GameTestHelpers.assertEquals(
        helper, "The vanilla pose of a cat NPC", Pose.CROUCHING, cat.getPose());
  }

  public static void assertFoxNPCGetsNoRandomItemOnSpawn(
      GameTestHelper helper, EntityType<?> entityType) {
    Mob fox = mockNPC(helper, entityType);
    finalizeSpawnRepeatedly(helper, fox);
    GameTestHelpers.assertTrue(
        helper, "A spawned fox NPC must not get a random item", fox.getMainHandItem().isEmpty());

    fox.setItemSlot(EquipmentSlot.MAINHAND, new ItemStack(Items.SWEET_BERRIES));
    finalizeSpawnRepeatedly(helper, fox);
    GameTestHelpers.assertTrue(
        helper,
        "Spawning must not replace the configured item of a fox NPC",
        fox.getMainHandItem().is(Items.SWEET_BERRIES));
  }

  private static Mob mockNPC(GameTestHelper helper, EntityType<?> entityType) {
    return GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION).getMob();
  }

  private static void finalizeSpawnRepeatedly(GameTestHelper helper, Mob mob) {
    ServerLevel serverLevel = helper.getLevel();
    for (int spawn = 0; spawn < SPAWNS_BEYOND_RANDOM_FOX_ITEM_CHANCE; spawn++) {
      mob.finalizeSpawn(
          serverLevel,
          serverLevel.getCurrentDifficultyAt(mob.blockPosition()),
          MobSpawnType.SPAWN_EGG,
          null,
          null);
    }
  }
}
