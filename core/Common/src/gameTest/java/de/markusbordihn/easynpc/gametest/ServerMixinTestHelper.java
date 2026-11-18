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

import de.markusbordihn.easynpc.access.LivingEntitySwingStateAccessHelper;
import de.markusbordihn.easynpc.access.SpawnerAccessHelper;
import de.markusbordihn.easynpc.access.WalkAnimationAccessHelper;
import de.markusbordihn.easynpc.data.attribute.InventoryAttributeType;
import de.markusbordihn.easynpc.data.spawner.SpawnerData;
import de.markusbordihn.easynpc.data.spawner.SpawnerType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPCBase;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import de.markusbordihn.easynpc.level.BaseEasyNPCSpawner;
import java.util.List;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.ConversionParams;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EntityTypes;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.WalkAnimationState;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.goal.MeleeAttackGoal;
import net.minecraft.world.entity.ai.goal.RangedBowAttackGoal;
import net.minecraft.world.entity.ai.goal.WrappedGoal;
import net.minecraft.world.entity.ai.goal.target.NearestAttackableTargetGoal;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.Witch;
import net.minecraft.world.entity.monster.cubemob.Slime;
import net.minecraft.world.entity.monster.zombie.Zombie;
import net.minecraft.world.entity.npc.villager.Villager;
import net.minecraft.world.entity.npc.villager.VillagerProfession;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.component.SwingAnimation;
import net.minecraft.world.phys.Vec3;

public class ServerMixinTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 1, 1);

  private static final Vec3 SECOND_NPC_POSITION = new Vec3(2, 1, 2);

  private static final Vec3 PLAYER_POSITION = new Vec3(2, 1, 1);

  private static final int TICKS_BEYOND_CREEPER_MAX_SWELL = 40;

  private static final int WITCH_AI_STEPS = 200;

  private static final int SLIME_SIZE_NOT_PRODUCED_BY_VANILLA_SPAWN = 3;

  private static final int SWING_TICKS = 2;

  private ServerMixinTestHelper() {}

  public static void assertCreeperNPCDoesNotExplode(
      GameTestHelper helper, EntityType<?> entityType) {
    Creeper creeper = (Creeper) mockNPC(helper, entityType);
    creeper.setSwellDir(1);
    for (int tick = 0; tick < TICKS_BEYOND_CREEPER_MAX_SWELL; tick++) {
      creeper.tick();
    }

    GameTestHelpers.assertTrue(
        helper,
        "A swelling creeper NPC must not explode",
        creeper.isAlive() && !creeper.isRemoved());
  }

  public static void assertPiglinNPCSkipsVanillaActivityUpdate(
      GameTestHelper helper, EntityType<?> entityType) {
    Mob piglin = mockNPCWithServerAi(helper, entityType);
    piglin.setAggressive(true);
    piglin.aiStep();

    GameTestHelpers.assertTrue(
        helper,
        "The vanilla piglin activity update must not reset the aggressive state of an NPC",
        piglin.isAggressive());
  }

  public static void assertFoxNPCGetsNoVanillaTargetGoals(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    finalizeSpawn(helper, easyNPC.getMob());

    GameTestHelpers.assertTrue(
        helper,
        "A fox NPC must not get the vanilla fox target goals",
        easyNPC.getEntityTargetSelector().getAvailableGoals().stream()
            .noneMatch(wrappedGoal ->
                wrappedGoal.getGoal() == null
                    || wrappedGoal.getGoal() instanceof NearestAttackableTargetGoal<?>));
  }

  public static void assertWitchNPCDrinksNoPotions(
      GameTestHelper helper, EntityType<?> entityType) {
    Witch witch = (Witch) mockNPCWithServerAi(helper, entityType);
    witch.setHealth(witch.getMaxHealth() / 2);
    ItemStack mainHandItem = witch.getMainHandItem().copy();
    for (int aiStep = 0; aiStep < WITCH_AI_STEPS; aiStep++) {
      witch.aiStep();
      GameTestHelpers.assertTrue(
          helper,
          "A hurt witch NPC must not drink a potion",
          !witch.isDrinkingPotion() && ItemStack.matches(mainHandItem, witch.getMainHandItem()));
    }
  }

  public static void assertVillagerNPCKeepsItsTradingPlayer(
      GameTestHelper helper, EntityType<?> entityType) {
    Villager villager = (Villager) mockNPCWithServerAi(helper, entityType);
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);
    villager.setVillagerData(
        villager
            .getVillagerData()
            .withProfession(helper.getLevel().registryAccess(), VillagerProfession.NONE));
    villager.setTradingPlayer(serverPlayer);
    villager.aiStep();

    GameTestHelpers.assertTrue(
        helper,
        "The vanilla villager AI must not stop the trading of an NPC without trades",
        villager.getTradingPlayer() == serverPlayer);
  }

  public static void assertSlimeNPCKeepsItsSizeOnFinalizeSpawn(
      GameTestHelper helper, EntityType<?> entityType) {
    Slime slime = (Slime) mockNPC(helper, entityType);
    slime.setSize(SLIME_SIZE_NOT_PRODUCED_BY_VANILLA_SPAWN, true);
    finalizeSpawn(helper, slime);

    GameTestHelpers.assertEquals(
        helper,
        "Finalize spawn must not change the size of a slime NPC",
        SLIME_SIZE_NOT_PRODUCED_BY_VANILLA_SPAWN,
        slime.getSize());
  }

  public static void assertWalkAnimationCopyTakesEveryValue(GameTestHelper helper) {
    WalkAnimationState sourceWalkAnimation = new WalkAnimationState();
    sourceWalkAnimation.setSpeed(0.25F);
    sourceWalkAnimation.update(0.75F, 0.5F, 1.0F);
    WalkAnimationState targetWalkAnimation = new WalkAnimationState();
    ((WalkAnimationAccessHelper) targetWalkAnimation).copyFrom(sourceWalkAnimation);

    GameTestHelpers.assertEquals(
        helper, "Copied walk speed", sourceWalkAnimation.speed(), targetWalkAnimation.speed());
    GameTestHelpers.assertEquals(
        helper,
        "Copied interpolated walk speed",
        sourceWalkAnimation.speed(0.5F),
        targetWalkAnimation.speed(0.5F));
    GameTestHelpers.assertEquals(
        helper,
        "Copied walk position",
        sourceWalkAnimation.position(),
        targetWalkAnimation.position());
  }

  public static void assertSwingStateCopyTakesEveryValue(
      GameTestHelper helper, EntityType<?> entityType) {
    Mob sourceEntity = mockNPC(helper, entityType);
    Mob targetEntity =
        GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION).getMob();
    sourceEntity.swing(InteractionHand.MAIN_HAND, SwingAnimation.DEFAULT, false);
    for (int tick = 0; tick < SWING_TICKS; tick++) {
      sourceEntity.tick();
    }
    GameTestHelpers.assertTrue(
        helper,
        "Source NPC must swing and target NPC must not swing before the copy",
        sourceEntity.isSwinging() && !targetEntity.isSwinging());

    ((LivingEntitySwingStateAccessHelper) targetEntity).copySwingStateFrom(sourceEntity);

    GameTestHelpers.assertEquals(
        helper,
        "Copied swing description",
        sourceEntity.getCurrentSwing(),
        targetEntity.getCurrentSwing());
    GameTestHelpers.assertEquals(
        helper,
        "Copied interpolated swing animation",
        sourceEntity.getSwingAnimation(0.5F),
        targetEntity.getSwingAnimation(0.5F));
  }

  public static void assertSpawnerDataMatchesEverySpawnerType(GameTestHelper helper) {
    for (SpawnerType spawnerType : SpawnerType.values()) {
      SpawnerAccessHelper spawnerAccess = (SpawnerAccessHelper) new BaseEasyNPCSpawner(spawnerType);
      GameTestHelpers.assertEquals(
          helper, "Spawner type", spawnerType, spawnerAccess.getSpawnerType());
      GameTestHelpers.assertEquals(
          helper,
          "Spawner data of " + spawnerType,
          SpawnerData.fromSpawnerType(spawnerType),
          new SpawnerData(
              spawnerAccess.getSpawnDelay(),
              spawnerAccess.getMinSpawnDelay(),
              spawnerAccess.getMaxSpawnDelay(),
              spawnerAccess.getSpawnCount(),
              spawnerAccess.getMaxNearbyEntities(),
              spawnerAccess.getRequiredPlayerRange(),
              spawnerAccess.getSpawnRange()));
      GameTestHelpers.assertNotNull(
          helper, "Spawn data of " + spawnerType, spawnerAccess.getSpawnDataDirect());
    }
  }

  public static void assertNPCIsNotReplacedByVanillaConversion(
      GameTestHelper helper, EntityType<?> entityType) {
    Mob mob = mockNPC(helper, entityType);

    GameTestHelpers.assertTrue(
        helper,
        "A vanilla conversion must not replace an NPC",
        mob.convertTo(
                    EntityTypes.DROWNED,
                    ConversionParams.single(mob, true, true),
                    convertedMob -> {})
                == null
            && mob.isAlive()
            && !mob.isRemoved());
  }

  public static void assertZombieNPCGetsNoRandomSpawnExtras(
      GameTestHelper helper, EntityType<?> entityType) {
    Zombie zombie = (Zombie) mockNPC(helper, entityType);
    finalizeSpawn(helper, zombie);

    GameTestHelpers.assertTrue(
        helper,
        "A zombie NPC must not spawn as a baby or chicken jockey",
        ((EasyNPCBase<?>) zombie).getVanillaSpawnGroupData(null)
                instanceof Zombie.ZombieGroupData zombieGroupData
            && !zombieGroupData.isBaby
            && !zombieGroupData.canSpawnJockey);
    GameTestHelpers.assertTrue(
        helper,
        "A zombie NPC must not get the random vanilla spawn bonus",
        zombie.getAttribute(Attributes.KNOCKBACK_RESISTANCE).getModifiers().isEmpty());
    GameTestHelpers.assertTrue(
        helper,
        "A zombie NPC must not break doors or get random equipment",
        !zombie.canBreakDoors() && zombie.getMainHandItem().isEmpty());
  }

  public static void assertZombifiedPiglinNPCKeepsItsWeaponOnFinalizeSpawn(
      GameTestHelper helper, EntityType<?> entityType) {
    Mob mob = mockNPC(helper, entityType);
    mob.setItemSlot(EquipmentSlot.MAINHAND, new ItemStack(Items.IRON_AXE));
    finalizeSpawn(helper, mob);

    GameTestHelpers.assertTrue(
        helper,
        "Finalize spawn must not replace the weapon of a zombified piglin NPC",
        mob.getMainHandItem().is(Items.IRON_AXE));
  }

  public static void assertSkeletonNPCGetsTheVanillaBowOnlyWithoutEquipment(
      GameTestHelper helper, EntityType<?> entityType) {
    Mob mobWithoutEquipment = mockNPC(helper, entityType);
    finalizeSpawn(helper, mobWithoutEquipment);
    GameTestHelpers.assertTrue(
        helper,
        "A skeleton NPC without equipment must get the vanilla bow",
        mobWithoutEquipment.getMainHandItem().is(Items.BOW));

    Mob mobWithEquipment = mockNPC(helper, entityType);
    mobWithEquipment.setItemSlot(EquipmentSlot.MAINHAND, new ItemStack(Items.IRON_SWORD));
    mobWithEquipment.setItemSlot(EquipmentSlot.HEAD, new ItemStack(Items.IRON_HELMET));
    finalizeSpawn(helper, mobWithEquipment);
    GameTestHelpers.assertTrue(
        helper,
        "Finalize spawn must not replace the equipment of a skeleton NPC",
        mobWithEquipment.getMainHandItem().is(Items.IRON_SWORD)
            && mobWithEquipment.getItemBySlot(EquipmentSlot.HEAD).is(Items.IRON_HELMET));
  }

  public static void assertSkeletonNPCGetsNoVanillaWeaponGoal(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    for (Item weapon : List.of(Items.BOW, Items.IRON_SWORD)) {
      easyNPC.getMob().setItemSlot(EquipmentSlot.MAINHAND, new ItemStack(weapon));
      GameTestHelpers.assertTrue(
          helper,
          "A skeleton NPC holding " + weapon + " must not get a vanilla attack goal",
          easyNPC.getEntityGoalSelector().getAvailableGoals().stream()
              .map(WrappedGoal::getGoal)
              .noneMatch(goal ->
                  goal instanceof RangedBowAttackGoal<?> || goal instanceof MeleeAttackGoal));
    }
  }

  public static void assertNPCPicksUpLootOnlyIntoItsInventory(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Mob mob = easyNPC.getMob();
    mob.setCanPickUpLoot(true);
    GameTestHelpers.assertTrue(
        helper, "The vanilla loot pickup must stay off for an NPC", !mob.canPickUpLoot());

    AttributeHandler.setInventoryAttribute(easyNPC, InventoryAttributeType.PICK_UP_ITEMS, true);
    GameTestHelpers.assertTrue(
        helper, "The inventory pickup of an NPC must enable its loot pickup", mob.canPickUpLoot());
  }

  private static Mob mockNPC(GameTestHelper helper, EntityType<?> entityType) {
    return GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION).getMob();
  }

  private static Mob mockNPCWithServerAi(GameTestHelper helper, EntityType<?> entityType) {
    Mob mob = mockNPC(helper, entityType);
    GameTestHelpers.assertTrue(helper, "NPC must run its server AI step", mob.isEffectiveAi());
    return mob;
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
