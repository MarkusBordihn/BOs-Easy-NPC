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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.api.handler.EasyNPCObjectiveHandler;
import de.markusbordihn.easynpc.config.NPCBaseConfig;
import de.markusbordihn.easynpc.data.attack.ProjectileData;
import de.markusbordihn.easynpc.data.attack.ProjectileHitEffect;
import de.markusbordihn.easynpc.data.attack.ProjectileImpactEffect;
import de.markusbordihn.easynpc.data.attack.ProjectileTrailParticle;
import de.markusbordihn.easynpc.data.attack.ProjectileType;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomMeleeAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.ProjectileAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import de.markusbordihn.easynpc.entity.projectile.EasyNPCBullet;
import de.markusbordihn.easynpc.entity.projectile.EasyNPCItemProjectile;
import de.markusbordihn.easynpc.entity.projectile.EasyNPCProjectile;
import de.markusbordihn.easynpc.handler.ProjectileHandler;
import de.markusbordihn.easynpc.item.attack.BulletItem;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import net.minecraft.core.BlockPos;
import net.minecraft.core.component.DataComponents;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.protocol.Packet;
import net.minecraft.network.protocol.game.ClientboundSoundPacket;
import net.minecraft.resources.Identifier;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.WrappedGoal;
import net.minecraft.world.entity.animal.pig.Pig;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.entity.projectile.arrow.AbstractArrow;
import net.minecraft.world.item.CrossbowItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.ItemStackTemplate;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.component.ChargedProjectiles;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.level.storage.TagValueOutput;
import net.minecraft.world.phys.Vec3;

public class ProjectileTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(2, 2, 2);
  private static final Vec3 TARGET_POSITION = new Vec3(1, 2, 1);
  private static final BlockPos GLASS_POSITION = new BlockPos(1, 2, 2);
  private static final BlockPos WALL_POSITION = new BlockPos(0, 2, 1);
  private static final Vec3 BEFORE_WALL_POSITION = new Vec3(1.5, 2.5, 1.5);
  private static final int LIFETIME_TICKS_LEFT = 5;
  private static final Vec3 BULLET_OFFSET_FROM_TARGET = new Vec3(1.5, 0.4, 0);
  private static final Vec3 BULLET_MOTION_TOWARDS_TARGET = new Vec3(-2.0, 0, 0);
  private static final float BULLET_DAMAGE = 4.0F;
  private static final ProjectileData ENABLED_PROJECTILE_DATA =
      ProjectileData.DEFAULT.withEnabled(true);
  private static final ProjectileData POISON_PROJECTILE_DATA =
      ProjectileData.DEFAULT.withInaccuracy(0.0F).withHitEffect(ProjectileHitEffect.POISON);
  private static final Vec3 SHOOTER_POSITION = new Vec3(2, 1, 2);
  private static final Vec3 ENEMY_POSITION = new Vec3(0.5, 1, 0.5);
  private static final int ENEMY_HITS_TO_SUCCEED = 2;
  private static final Identifier CUSTOM_PROJECTILE_ITEM =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, BulletItem.ID);
  private static final Vec3 LISTENER_POSITION = new Vec3(1, 2, 2);
  private static final double MAXIMUM_SOUND_DISTANCE = 0.5D;
  private static final SoundEvent CONFIGURED_RANGED_ATTACK_SOUND = SoundEvents.BLAZE_SHOOT;
  private static final SoundEvent FALLBACK_RANGED_ATTACK_SOUND = SoundEvents.FIRECHARGE_USE;

  private ProjectileTestHelper() {}

  private static int executeProjectileCommand(
      GameTestHelper helper, EasyNPC<?> easyNPC, String settingAndValue) {
    return GameTestHelpers.executeCommand(
        helper,
        GameTestHelpers.consoleCommandSource(helper, NPC_POSITION),
        "easy_npc projectile " + easyNPC.getEntityUUID() + " " + settingAndValue);
  }

  public static void assertProjectileCommandChangesSettings(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    GameTestHelpers.assertEquals(
        helper,
        "Enabled command result",
        1,
        executeProjectileCommand(helper, easyNPC, "enabled true"));
    GameTestHelpers.assertEquals(
        helper,
        "Type command result",
        1,
        executeProjectileCommand(helper, easyNPC, "type snowball"));
    GameTestHelpers.assertEquals(
        helper, "Damage command result", 1, executeProjectileCommand(helper, easyNPC, "damage 12"));
    GameTestHelpers.assertEquals(
        helper,
        "Shot count command result",
        1,
        executeProjectileCommand(helper, easyNPC, "shot_count 3"));
    GameTestHelpers.assertEquals(
        helper,
        "Hit effect command result",
        1,
        executeProjectileCommand(helper, easyNPC, "hit_effect poison 2 15"));
    GameTestHelpers.assertEquals(
        helper,
        "Item command result",
        1,
        executeProjectileCommand(helper, easyNPC, "item minecraft:diamond"));

    ProjectileData projectileData = ProjectileHandler.getProjectileData(easyNPC);
    GameTestHelpers.assertTrue(helper, "Projectile enabled", projectileData.enabled());
    GameTestHelpers.assertEquals(
        helper, "Projectile type", ProjectileType.SNOWBALL, projectileData.projectileType());
    GameTestHelpers.assertEquals(helper, "Projectile damage", 12.0F, projectileData.damage());
    GameTestHelpers.assertEquals(helper, "Shot count", 3, projectileData.shotCount());
    GameTestHelpers.assertEquals(
        helper, "Hit effect", ProjectileHitEffect.POISON, projectileData.hitEffect());
    GameTestHelpers.assertEquals(helper, "Hit effect level", 2, projectileData.hitEffectLevel());
    GameTestHelpers.assertEquals(
        helper, "Hit effect seconds", 15, projectileData.hitEffectSeconds());
    GameTestHelpers.assertEquals(
        helper,
        "Projectile item",
        Identifier.fromNamespaceAndPath("minecraft", "diamond"),
        projectileData.projectileItem());

    GameTestHelpers.assertEquals(
        helper, "Reset command result", 1, executeProjectileCommand(helper, easyNPC, "reset"));
    GameTestHelpers.assertEquals(
        helper,
        "Projectile data after reset",
        ProjectileData.DEFAULT,
        ProjectileHandler.getProjectileData(easyNPC));
  }

  public static void assertProjectileCommandRejectsInvalidValues(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    GameTestHelpers.assertEquals(
        helper,
        "Unknown item command result",
        0,
        executeProjectileCommand(helper, easyNPC, "item minecraft:unknown_item"));
    GameTestHelpers.assertEquals(
        helper,
        "Out of range damage command result",
        0,
        executeProjectileCommand(helper, easyNPC, "damage 1000"));
    GameTestHelpers.assertEquals(
        helper,
        "Unchanged default command result",
        0,
        executeProjectileCommand(helper, easyNPC, "enabled false"));
    GameTestHelpers.assertEquals(
        helper,
        "Projectile data after rejected commands",
        ProjectileData.DEFAULT,
        ProjectileHandler.getProjectileData(easyNPC));
  }

  public static void assertProjectileCommandCapsExplosionPower(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    executeProjectileCommand(
        helper, easyNPC, "explosion_power " + ProjectileData.MAXIMUM_EXPLOSION_POWER);

    GameTestHelpers.assertEquals(
        helper,
        "Explosion power capped by server config",
        Math.min(
            ProjectileData.MAXIMUM_EXPLOSION_POWER,
            NPCBaseConfig.PROJECTILE_MAXIMUM_EXPLOSION_POWER),
        ProjectileHandler.getProjectileData(easyNPC).explosionPower());
  }

  public static void assertProjectileDataIsSavedAndLoaded(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ProjectileData projectileData =
        ProjectileData.DEFAULT
            .withEnabled(true)
            .withDamage(12.0F)
            .withTrailParticle(ProjectileTrailParticle.SOUL_FIRE_FLAME)
            .withImpactEffect(ProjectileImpactEffect.PARTICLES);
    easyNPC.getEasyNPCAttackData().setProjectileData(projectileData);

    CompoundTag savedTag = saveEntity(helper, easyNPC.getEntity());
    GameTestHelpers.assertTrue(
        helper,
        "The NPC save data must contain the projectile data",
        savedTag.contains(AttackDataCapable.DATA_PROJECTILE_DATA_TAG));

    EasyNPC<?> loadedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, TARGET_POSITION);
    loadedNPC
        .getEasyNPCAttackData()
        .readAdditionalAttackData(
            TagValueInput.create(
                ProblemReporter.DISCARDING, helper.getLevel().registryAccess(), savedTag));
    GameTestHelpers.assertEquals(
        helper,
        "The loaded projectile data must match the saved one",
        projectileData,
        loadedNPC.getEasyNPCAttackData().getProjectileData());
  }

  public static void assertDefaultProjectileDataIsNotSaved(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    CompoundTag savedTag = saveEntity(helper, easyNPC.getEntity());
    GameTestHelpers.assertTrue(
        helper,
        "Default projectile data must not be written",
        !savedTag.contains(AttackDataCapable.DATA_PROJECTILE_DATA_TAG));
  }

  public static void assertCustomProjectileAttackSpawnsBullet(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Pig target = spawnTarget(helper);
    ProjectileData projectileData =
        ProjectileData.DEFAULT
            .withEnabled(true)
            .withDamage(BULLET_DAMAGE)
            .withVelocity(2.0F)
            .withInaccuracy(0.0F)
            .withTrailParticle(ProjectileTrailParticle.FLAME);

    List<Projectile> projectiles =
        AttackHandler.performCustomProjectileAttack(
            easyNPC.getLivingEntity(), target, projectileData);
    GameTestHelpers.assertEquals(helper, "A single shot must be fired", 1, projectiles.size());
    Projectile projectile = projectiles.get(0);
    GameTestHelpers.assertTrue(
        helper,
        "The default projectile type must spawn a bullet",
        projectile instanceof EasyNPCBullet);
    EasyNPCBullet bullet = (EasyNPCBullet) projectile;
    GameTestHelpers.assertTrue(
        helper,
        "The bullet must be added to the level",
        helper.getLevel().getEntity(bullet.getId()) == bullet);
    GameTestHelpers.assertTrue(
        helper,
        "The bullet must belong to the NPC",
        bullet.getOwner() == easyNPC.getLivingEntity());
    GameTestHelpers.assertTrue(
        helper,
        "The bullet must not be picked up",
        bullet.pickup == AbstractArrow.Pickup.DISALLOWED);
    GameTestHelpers.assertEquals(
        helper,
        "The bullet must carry the configured data",
        projectileData,
        bullet.getProjectileData());
    GameTestHelpers.assertEquals(
        helper,
        "The trail particle must be synched",
        ProjectileTrailParticle.FLAME,
        bullet.getTrailParticle());
    GameTestHelpers.assertTrue(
        helper,
        "The bullet must fly with the configured velocity",
        Math.abs(bullet.getDeltaMovement().length() - projectileData.velocity()) < 0.01);
    bullet.discard();
  }

  public static void assertShotCountFiresEveryProjectile(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Pig target = spawnTarget(helper);

    List<Projectile> projectiles =
        AttackHandler.performCustomProjectileAttack(
            easyNPC.getLivingEntity(),
            target,
            ProjectileData.DEFAULT
                .withEnabled(true)
                .withShotCount(ProjectileData.MAXIMUM_SHOT_COUNT));
    GameTestHelpers.assertEquals(
        helper,
        "Every configured shot must be fired",
        ProjectileData.MAXIMUM_SHOT_COUNT,
        projectiles.size());
    for (Projectile projectile : projectiles) {
      GameTestHelpers.assertTrue(
          helper,
          "Every shot must be added to the level",
          helper.getLevel().getEntity(projectile.getId()) == projectile);
      projectile.discard();
    }
  }

  public static void assertKnockbackPushesTargetFurther(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ProjectileData projectileData = ProjectileData.DEFAULT.withEnabled(true);

    double pushWithoutKnockback = getHorizontalPushAfterHit(helper, easyNPC, projectileData);
    double pushWithKnockback =
        getHorizontalPushAfterHit(
            helper, easyNPC, projectileData.withKnockback(ProjectileData.MAXIMUM_KNOCKBACK));
    GameTestHelpers.assertTrue(
        helper,
        "Knockback must push the target further, without: "
            + pushWithoutKnockback
            + ", with: "
            + pushWithKnockback,
        pushWithKnockback > pushWithoutKnockback + 1.0);
  }

  private static double getHorizontalPushAfterHit(
      GameTestHelper helper, EasyNPC<?> easyNPC, ProjectileData projectileData) {
    Pig target = spawnTarget(helper);
    createBulletBeforeTarget(easyNPC, target, projectileData).tick();
    double horizontalPush = target.getDeltaMovement().horizontalDistance();
    target.discard();
    return horizontalPush;
  }

  public static void assertHitEffectIsApplied(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Pig target = spawnTarget(helper);

    createBulletBeforeTarget(
            easyNPC,
            target,
            ProjectileData.DEFAULT
                .withEnabled(true)
                .withHitEffect(ProjectileHitEffect.POISON)
                .withHitEffectLevel(3)
                .withHitEffectSeconds(10))
        .tick();

    MobEffectInstance poisonEffect = target.getEffect(MobEffects.POISON);
    GameTestHelpers.assertNotNull(helper, "The target must be poisoned", poisonEffect);
    GameTestHelpers.assertEquals(
        helper, "The effect level must match", 2, poisonEffect.getAmplifier());
    GameTestHelpers.assertEquals(
        helper, "The effect duration must match", 200, poisonEffect.getDuration());
  }

  public static void assertBulletDealsConfiguredDamage(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Pig target = spawnTarget(helper);
    float initialHealth = target.getHealth();

    EasyNPCBullet bullet =
        createBulletBeforeTarget(
            easyNPC, target, ProjectileData.DEFAULT.withEnabled(true).withDamage(BULLET_DAMAGE));
    bullet.tick();

    GameTestHelpers.assertTrue(
        helper,
        "The target must lose exactly the configured damage, health: " + target.getHealth(),
        Math.abs(initialHealth - BULLET_DAMAGE - target.getHealth()) < 0.01F);
    GameTestHelpers.assertTrue(
        helper, "The bullet must be removed after a hit", bullet.isRemoved());
  }

  public static void assertExplosionDoesNotDestroyBlocks(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Pig target = spawnTarget(helper);
    helper.setBlock(GLASS_POSITION, Blocks.GLASS);

    EasyNPCBullet bullet =
        createBulletBeforeTarget(
            easyNPC,
            target,
            ProjectileData.DEFAULT
                .withEnabled(true)
                .withImpactEffect(ProjectileImpactEffect.EXPLOSION)
                .withExplosionPower(ProjectileData.MAXIMUM_EXPLOSION_POWER));
    bullet.tick();

    GameTestHelpers.assertTrue(helper, "The bullet must explode on impact", bullet.isRemoved());
    GameTestHelpers.assertTrue(
        helper,
        "The explosion must not destroy blocks by default",
        helper.getBlockState(GLASS_POSITION).is(Blocks.GLASS));
  }

  public static void assertEveryProjectileTypeSpawnsMatchingEntity(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    Pig target = spawnTarget(helper);

    for (ProjectileType projectileType : ProjectileType.values()) {
      ProjectileData projectileData =
          ProjectileData.DEFAULT
              .withEnabled(true)
              .withProjectileType(projectileType)
              .withProjectileItem(CUSTOM_PROJECTILE_ITEM);
      List<Projectile> projectiles =
          AttackHandler.performCustomProjectileAttack(
              easyNPC.getLivingEntity(), target, projectileData);
      GameTestHelpers.assertEquals(
          helper, "A single shot must be fired for " + projectileType, 1, projectiles.size());
      Projectile projectile = projectiles.get(0);
      GameTestHelpers.assertTrue(
          helper,
          "The projectile must be added to the level for " + projectileType,
          helper.getLevel().getEntity(projectile.getId()) == projectile);
      Class<?> expectedProjectileClass =
          EasyNPCBullet.supportsProjectileType(projectileType)
              ? EasyNPCBullet.class
              : EasyNPCItemProjectile.class;
      GameTestHelpers.assertTrue(
          helper,
          "The projectile must use the matching entity for " + projectileType,
          expectedProjectileClass.isInstance(projectile));
      GameTestHelpers.assertEquals(
          helper,
          "The projectile type must be synched",
          projectileType,
          ((EasyNPCProjectile) projectile).getProjectileType());
      if (projectile instanceof EasyNPCItemProjectile itemProjectile) {
        GameTestHelpers.assertEquals(
            helper,
            "The projectile must show the matching item for " + projectileType,
            getExpectedProjectileItem(projectileType),
            itemProjectile.getItem().getItem());
      }
      projectile.discard();
    }
  }

  public static void assertEveryProjectileTypeDealsConfiguredDamage(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    for (ProjectileType projectileType : ProjectileType.values()) {
      Pig target = spawnTarget(helper);
      float initialHealth = target.getHealth();
      Projectile projectile =
          createProjectileBeforeTarget(
              easyNPC,
              target,
              ProjectileData.DEFAULT
                  .withEnabled(true)
                  .withDamage(BULLET_DAMAGE)
                  .withProjectileType(projectileType)
                  .withProjectileItem(CUSTOM_PROJECTILE_ITEM));
      projectile.tick();

      GameTestHelpers.assertTrue(
          helper,
          "The target must lose exactly the configured damage for "
              + projectileType
              + ", health: "
              + target.getHealth(),
          Math.abs(initialHealth - BULLET_DAMAGE - target.getHealth()) < 0.01F);
      GameTestHelpers.assertTrue(
          helper,
          "The projectile must be removed after a hit for " + projectileType,
          projectile.isRemoved());
      assertTargetEffect(helper, projectileType, target);
      target.discard();
    }
  }

  public static void assertEveryProjectileTypeIsRemovedOnBlockHit(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    helper.setBlock(WALL_POSITION, Blocks.STONE);

    for (ProjectileType projectileType : ProjectileType.values()) {
      Projectile projectile =
          AttackHandler.createCustomProjectile(
              easyNPC.getLivingEntity(),
              ProjectileData.DEFAULT
                  .withEnabled(true)
                  .withProjectileType(projectileType)
                  .withProjectileItem(CUSTOM_PROJECTILE_ITEM));
      projectile.setPos(helper.absoluteVec(BEFORE_WALL_POSITION));
      projectile.setDeltaMovement(BULLET_MOTION_TOWARDS_TARGET);
      projectile.tick();

      GameTestHelpers.assertTrue(
          helper,
          "The projectile must be removed after hitting a block for " + projectileType,
          projectile.isRemoved());
    }
    helper.assertEntityNotPresent(EntityType.ITEM);
    helper.assertEntityNotPresent(EntityType.ARROW);
    helper.assertEntityNotPresent(EntityType.SPECTRAL_ARROW);
    helper.assertEntityNotPresent(EntityType.TRIDENT);
  }

  public static void assertEveryProjectileTypeIsRemovedAfterMaximumLifetime(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    List<Projectile> projectiles = new ArrayList<>();

    for (ProjectileType projectileType : ProjectileType.values()) {
      Projectile projectile =
          AttackHandler.createCustomProjectile(
              easyNPC.getLivingEntity(),
              ProjectileData.DEFAULT
                  .withEnabled(true)
                  .withProjectileType(projectileType)
                  .withProjectileItem(CUSTOM_PROJECTILE_ITEM));
      projectile.setPos(helper.absoluteVec(BEFORE_WALL_POSITION));
      projectile.setDeltaMovement(Vec3.ZERO);
      projectile.setNoGravity(true);
      projectile.tickCount = getMaximumLifetimeTicks(projectile) - LIFETIME_TICKS_LEFT;
      helper.getLevel().addFreshEntity(projectile);
      projectiles.add(projectile);
    }

    helper.runAfterDelay(
        1,
        () ->
            projectiles.forEach(
                projectile ->
                    GameTestHelpers.assertTrue(
                        helper,
                        "The projectile must stay before its lifetime ends for "
                            + ((EasyNPCProjectile) projectile).getProjectileType(),
                        !projectile.isRemoved())));
    helper.succeedWhen(
        () ->
            projectiles.forEach(
                projectile ->
                    GameTestHelpers.assertTrue(
                        helper,
                        "The projectile must be removed after its lifetime for "
                            + ((EasyNPCProjectile) projectile).getProjectileType(),
                        projectile.isRemoved())));
  }

  public static void assertNPCProjectilesCannotBePickedUp(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    for (ProjectileType projectileType : ProjectileType.values()) {
      Projectile projectile =
          AttackHandler.createCustomProjectile(
              easyNPC.getLivingEntity(),
              ProjectileData.DEFAULT.withEnabled(true).withProjectileType(projectileType));
      if (projectile instanceof AbstractArrow arrow) {
        GameTestHelpers.assertTrue(
            helper,
            "The projectile must not be picked up for " + projectileType,
            arrow.pickup == AbstractArrow.Pickup.DISALLOWED);
      }
    }
    GameTestHelpers.assertTrue(
        helper,
        "Bow arrows of an NPC must not be picked up",
        AttackHandler.getArrow(
                    easyNPC.getLivingEntity(),
                    new ItemStack(Items.BOW),
                    new ItemStack(Items.ARROW),
                    BULLET_DAMAGE)
                .pickup
            == AbstractArrow.Pickup.DISALLOWED);
    GameTestHelpers.assertTrue(
        helper,
        "Gun bullets of an NPC must not be picked up",
        AttackHandler.getBullet(
                    easyNPC.getLivingEntity(), new ItemStack(Items.CROSSBOW), BULLET_DAMAGE)
                .pickup
            == AbstractArrow.Pickup.DISALLOWED);
  }

  public static void assertProjectileItemIsResolved(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ProjectileData itemProjectileData =
        ProjectileData.DEFAULT.withEnabled(true).withProjectileType(ProjectileType.ITEM);

    GameTestHelpers.assertTrue(
        helper,
        "The custom projectile item must be registered",
        BuiltInRegistries.ITEM.getValue(CUSTOM_PROJECTILE_ITEM) != Items.AIR);
    assertItemProjectileShows(
        helper,
        easyNPC,
        itemProjectileData.withProjectileItem(CUSTOM_PROJECTILE_ITEM),
        BuiltInRegistries.ITEM.getValue(CUSTOM_PROJECTILE_ITEM));
    assertItemProjectileShows(
        helper,
        easyNPC,
        itemProjectileData.withProjectileItem(
            Identifier.fromNamespaceAndPath("minecraft", "diamond")),
        Items.DIAMOND);
    assertItemProjectileShows(
        helper,
        easyNPC,
        itemProjectileData.withProjectileItem(
            Identifier.fromNamespaceAndPath(Constants.MOD_ID, "unknown_projectile_item")),
        Items.SNOWBALL);
  }

  public static void assertBowAttackShootsCustomProjectile(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC =
        mockRangedNPC(helper, entityType, new ItemStack(Items.BOW), ENABLED_PROJECTILE_DATA);
    Pig target = spawnTarget(helper);

    AttackHandler.performDefaultRangedAttack(easyNPC.getLivingEntity(), target, 1.0F);
    List<Projectile> projectiles = getProjectilesOf(helper, easyNPC);
    GameTestHelpers.assertEquals(helper, "The bow must fire a single shot", 1, projectiles.size());
    GameTestHelpers.assertTrue(
        helper,
        "The bow must fire the custom projectile",
        projectiles.get(0) instanceof EasyNPCBullet);
    projectiles.forEach(Projectile::discard);
  }

  public static void assertBowAttackKeepsArrowWithoutCustomProjectile(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC =
        mockRangedNPC(helper, entityType, new ItemStack(Items.BOW), ProjectileData.DEFAULT);
    Pig target = spawnTarget(helper);

    AttackHandler.performDefaultRangedAttack(easyNPC.getLivingEntity(), target, 1.0F);
    List<Projectile> projectiles = getProjectilesOf(helper, easyNPC);
    GameTestHelpers.assertEquals(helper, "The bow must fire a single shot", 1, projectiles.size());
    GameTestHelpers.assertTrue(
        helper,
        "The bow must fire a vanilla arrow",
        projectiles.get(0) instanceof AbstractArrow
            && !(projectiles.get(0) instanceof EasyNPCProjectile));
    projectiles.forEach(Projectile::discard);
  }

  public static void assertCrossbowAttackShootsCustomProjectile(
      GameTestHelper helper, EntityType<?> entityType) {
    ItemStack crossbow = new ItemStack(Items.CROSSBOW);
    crossbow.set(
        DataComponents.CHARGED_PROJECTILES,
        ChargedProjectiles.of(ItemStackTemplate.fromNonEmptyStack(new ItemStack(Items.ARROW))));
    EasyNPC<?> easyNPC = mockRangedNPC(helper, entityType, crossbow, ENABLED_PROJECTILE_DATA);
    Pig target = spawnTarget(helper);

    AttackHandler.performDefaultRangedAttack(easyNPC.getLivingEntity(), target, 1.0F);
    List<Projectile> projectiles = getProjectilesOf(helper, easyNPC);
    GameTestHelpers.assertEquals(
        helper, "The crossbow must fire a single shot", 1, projectiles.size());
    GameTestHelpers.assertTrue(
        helper,
        "The crossbow must fire the custom projectile",
        projectiles.get(0) instanceof EasyNPCBullet);
    GameTestHelpers.assertTrue(
        helper,
        "The crossbow must be uncharged after the shot",
        !CrossbowItem.isCharged(easyNPC.getLivingEntity().getMainHandItem()));
    projectiles.forEach(Projectile::discard);
  }

  public static void assertProjectileAttackNeedsNoWeapon(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC =
        mockRangedNPC(
            helper,
            entityType,
            ItemStack.EMPTY,
            ProjectileData.DEFAULT.withProjectileType(ProjectileType.SMALL_FIREBALL));
    Pig target = spawnTarget(helper);

    List<Projectile> projectiles =
        AttackHandler.performProjectileAttack(easyNPC.getLivingEntity(), target);
    GameTestHelpers.assertEquals(
        helper, "The projectile attack must fire a single shot", 1, projectiles.size());
    GameTestHelpers.assertTrue(
        helper,
        "The projectile attack must fire the configured type without enabling it",
        projectiles.get(0) instanceof EasyNPCProjectile easyNPCProjectile
            && easyNPCProjectile.getProjectileType() == ProjectileType.SMALL_FIREBALL);
    projectiles.forEach(Projectile::discard);
  }

  public static void assertProjectileAttackObjectiveCreatesGoal(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ObjectiveDataEntry objectiveDataEntry =
        new ObjectiveDataEntry(ObjectiveType.PROJECTILE_ATTACK);
    GameTestHelpers.assertTrue(
        helper,
        "The projectile attack objective must be added",
        EasyNPCObjectiveHandler.addObjective(easyNPC, objectiveDataEntry));
    GameTestHelpers.assertTrue(
        helper,
        "The projectile attack objective must create a projectile attack goal",
        objectiveDataEntry.getGoal(easyNPC) instanceof ProjectileAttackGoal);
  }

  public static void assertCrossbowAttackKeepsArrowWithoutCustomProjectile(
      GameTestHelper helper, EntityType<?> entityType) {
    ItemStack crossbow = new ItemStack(Items.CROSSBOW);
    crossbow.set(
        DataComponents.CHARGED_PROJECTILES,
        ChargedProjectiles.of(ItemStackTemplate.fromNonEmptyStack(new ItemStack(Items.ARROW))));
    EasyNPC<?> easyNPC = mockRangedNPC(helper, entityType, crossbow, ProjectileData.DEFAULT);
    Pig target = spawnTarget(helper);
    easyNPC.getMob().setTarget(target);

    AttackHandler.performDefaultRangedAttack(easyNPC.getLivingEntity(), target, 1.0F);
    List<Projectile> projectiles = getProjectilesOf(helper, easyNPC);
    GameTestHelpers.assertEquals(
        helper, "The crossbow must fire a single shot", 1, projectiles.size());
    GameTestHelpers.assertTrue(
        helper,
        "The crossbow must fire a vanilla arrow",
        projectiles.get(0) instanceof AbstractArrow
            && !(projectiles.get(0) instanceof EasyNPCProjectile));
    projectiles.forEach(Projectile::discard);
  }

  public static void assertProjectileAttackObjectiveHitsEnemy(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC =
        mockRangedNPC(
            helper,
            entityType,
            ItemStack.EMPTY,
            POISON_PROJECTILE_DATA.withProjectileType(ProjectileType.SMALL_FIREBALL));
    assertObjectiveHitsEnemyRepeatedly(helper, easyNPC, ObjectiveType.PROJECTILE_ATTACK);
  }

  public static void assertBowAttackObjectiveHitsEnemyWithCustomProjectile(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC =
        mockRangedNPC(
            helper, entityType, new ItemStack(Items.BOW), POISON_PROJECTILE_DATA.withEnabled(true));
    assertObjectiveHitsEnemyRepeatedly(helper, easyNPC, ObjectiveType.BOW_ATTACK);
  }

  public static void assertMeleeAndBowAttackObjectivesShootWhenHoldingBow(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC =
        mockRangedNPC(
            helper, entityType, new ItemStack(Items.BOW), POISON_PROJECTILE_DATA.withEnabled(true));
    GameTestHelpers.assertTrue(
        helper,
        "The melee attack objective must be added",
        EasyNPCObjectiveHandler.addObjective(
            easyNPC, new ObjectiveDataEntry(ObjectiveType.MELEE_ATTACK)));
    assertObjectiveHitsEnemyRepeatedly(helper, easyNPC, ObjectiveType.BOW_ATTACK);
  }

  public static void assertMeleeAttackYieldsOnlyToMatchingRangedWeapon(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC =
        mockRangedNPC(helper, entityType, new ItemStack(Items.BOW), ProjectileData.DEFAULT);
    EasyNPCObjectiveHandler.addObjective(
        easyNPC, new ObjectiveDataEntry(ObjectiveType.MELEE_ATTACK));
    EasyNPCObjectiveHandler.addObjective(easyNPC, new ObjectiveDataEntry(ObjectiveType.BOW_ATTACK));
    Mob enemy = spawnEnemy(helper);
    enemy.setPos(easyNPC.getLivingEntity().position());
    easyNPC.getMob().setTarget(enemy);
    Goal meleeAttackGoal =
        easyNPC.getEntityGoalSelector().getAvailableGoals().stream()
            .map(WrappedGoal::getGoal)
            .filter(CustomMeleeAttackGoal.class::isInstance)
            .findFirst()
            .orElse(null);
    GameTestHelpers.assertTrue(
        helper, "The melee attack goal must be registered", meleeAttackGoal != null);

    GameTestHelpers.assertTrue(
        helper, "The melee attack yields while holding a bow", !meleeAttackGoal.canUse());

    easyNPC.getLivingEntity().setItemSlot(EquipmentSlot.MAINHAND, new ItemStack(Items.IRON_SWORD));
    GameTestHelpers.assertTrue(
        helper, "The melee attack is used while holding a sword", meleeAttackGoal.canUse());
  }

  public static void assertCrossbowAttackObjectiveHitsEnemyWithCustomProjectile(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC =
        mockRangedNPC(
            helper,
            entityType,
            new ItemStack(Items.CROSSBOW),
            POISON_PROJECTILE_DATA.withEnabled(true));
    assertObjectiveHitsEnemyRepeatedly(helper, easyNPC, ObjectiveType.CROSSBOW_ATTACK);
  }

  private static void assertObjectiveHitsEnemyRepeatedly(
      GameTestHelper helper, EasyNPC<?> easyNPC, ObjectiveType objectiveType) {
    GameTestHelpers.assertTrue(
        helper,
        "The " + objectiveType + " objective must be added",
        EasyNPCObjectiveHandler.addObjective(easyNPC, new ObjectiveDataEntry(objectiveType)));
    Mob enemy = spawnEnemy(helper);
    easyNPC.getMob().setTarget(enemy);
    Vec3 shooterPosition = helper.absoluteVec(SHOOTER_POSITION);
    Vec3 enemyPosition = helper.absoluteVec(ENEMY_POSITION);
    AtomicInteger enemyHits = new AtomicInteger();

    helper.succeedWhen(
        () -> {
          easyNPC.getLivingEntity().setPos(shooterPosition);
          enemy.setPos(enemyPosition);
          enemy.setDeltaMovement(Vec3.ZERO);
          if (enemy.hasEffect(MobEffects.POISON)
              && enemy.getLastHurtByMob() == easyNPC.getLivingEntity()) {
            enemyHits.incrementAndGet();
            enemy.removeAllEffects();
            enemy.setHealth(enemy.getMaxHealth());
          }
          GameTestHelpers.assertTrue(
              helper,
              objectiveType
                  + " must hit the enemy "
                  + ENEMY_HITS_TO_SUCCEED
                  + " times with the configured projectile, hits: "
                  + enemyHits.get(),
              enemyHits.get() >= ENEMY_HITS_TO_SUCCEED);
        });
  }

  private static Mob spawnEnemy(GameTestHelper helper) {
    Mob enemy = GameTestHelpers.spawnEntityType(helper, EntityType.VINDICATOR);
    enemy.setPos(helper.absoluteVec(ENEMY_POSITION));
    enemy.setNoAi(true);
    return enemy;
  }

  private static EasyNPC<?> mockRangedNPC(
      GameTestHelper helper,
      EntityType<?> entityType,
      ItemStack weapon,
      ProjectileData projectileData) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.getLivingEntity().setItemSlot(EquipmentSlot.MAINHAND, weapon);
    easyNPC.getEasyNPCAttackData().setProjectileData(projectileData);
    return easyNPC;
  }

  private static List<Projectile> getProjectilesOf(GameTestHelper helper, EasyNPC<?> easyNPC) {
    LivingEntity shooter = easyNPC.getLivingEntity();
    return helper
        .getLevel()
        .getEntitiesOfClass(
            Projectile.class,
            shooter.getBoundingBox().inflate(8),
            projectile -> projectile.getOwner() == shooter);
  }

  private static void assertItemProjectileShows(
      GameTestHelper helper, EasyNPC<?> easyNPC, ProjectileData projectileData, Item expectedItem) {
    EasyNPCItemProjectile itemProjectile =
        EasyNPCItemProjectile.create(easyNPC.getLivingEntity(), projectileData);
    GameTestHelpers.assertNotNull(helper, "The item projectile must be created", itemProjectile);
    GameTestHelpers.assertEquals(
        helper,
        "The item projectile must show the item for " + projectileData.projectileItem(),
        expectedItem,
        itemProjectile.getItem().getItem());
  }

  private static void assertTargetEffect(
      GameTestHelper helper, ProjectileType projectileType, Pig target) {
    switch (projectileType) {
      case FIRE_ARROW, SMALL_FIREBALL, LARGE_FIREBALL ->
          GameTestHelpers.assertTrue(
              helper, "The target must burn after a hit by " + projectileType, target.isOnFire());
      case SPECTRAL_ARROW ->
          GameTestHelpers.assertTrue(
              helper,
              "The target must glow after a spectral arrow hit",
              target.hasEffect(MobEffects.GLOWING));
      case WITHER_SKULL ->
          GameTestHelpers.assertTrue(
              helper,
              "The target must wither after a wither skull hit",
              target.hasEffect(MobEffects.WITHER));
      default ->
          GameTestHelpers.assertTrue(
              helper,
              "The target must not burn after a hit by " + projectileType,
              !target.isOnFire());
    }
  }

  private static int getMaximumLifetimeTicks(Projectile projectile) {
    return projectile instanceof EasyNPCBullet
        ? EasyNPCBullet.MAXIMUM_LIFETIME_TICKS
        : EasyNPCItemProjectile.MAXIMUM_LIFETIME_TICKS;
  }

  private static Item getExpectedProjectileItem(ProjectileType projectileType) {
    return switch (projectileType) {
      case EGG -> Items.EGG;
      case SMALL_FIREBALL, LARGE_FIREBALL -> Items.FIRE_CHARGE;
      case WITHER_SKULL -> Items.WITHER_SKELETON_SKULL;
      case ITEM -> BuiltInRegistries.ITEM.getValue(CUSTOM_PROJECTILE_ITEM);
      default -> Items.SNOWBALL;
    };
  }

  private static Projectile createProjectileBeforeTarget(
      EasyNPC<?> easyNPC, Pig target, ProjectileData projectileData) {
    Projectile projectile =
        AttackHandler.createCustomProjectile(easyNPC.getLivingEntity(), projectileData);
    projectile.setPos(target.position().add(BULLET_OFFSET_FROM_TARGET));
    projectile.setDeltaMovement(BULLET_MOTION_TOWARDS_TARGET);
    return projectile;
  }

  public static void assertRangedAttackPlaysConfiguredSound(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    int result =
        GameTestHelpers.executeCommand(
            helper,
            GameTestHelpers.consoleCommandSource(helper, NPC_POSITION),
            "easy_npc sound set "
                + easyNPC.getEntityUUID()
                + " "
                + SoundType.RANGED_ATTACK.name()
                + " "
                + BuiltInRegistries.SOUND_EVENT.getKey(CONFIGURED_RANGED_ATTACK_SOUND));
    GameTestHelpers.assertEquals(helper, "Sound command result", 1, result);

    List<SoundEvent> playedSounds = performRangedAttackAndCollectSounds(helper, easyNPC);
    GameTestHelpers.assertTrue(
        helper,
        "The configured ranged attack sound must be played, played: " + playedSounds,
        playedSounds.contains(CONFIGURED_RANGED_ATTACK_SOUND));
    GameTestHelpers.assertTrue(
        helper,
        "The fallback sound must not be played, played: " + playedSounds,
        !playedSounds.contains(FALLBACK_RANGED_ATTACK_SOUND));
  }

  public static void assertRangedAttackPlaysFallbackSoundWithoutConfiguredSound(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    List<SoundEvent> playedSounds = performRangedAttackAndCollectSounds(helper, easyNPC);
    GameTestHelpers.assertTrue(
        helper,
        "The fallback sound must be played, played: " + playedSounds,
        playedSounds.contains(FALLBACK_RANGED_ATTACK_SOUND));
  }

  private static List<SoundEvent> performRangedAttackAndCollectSounds(
      GameTestHelper helper, EasyNPC<?> easyNPC) {
    List<Packet<?>> sentPackets = new ArrayList<>();
    GameTestHelpers.mockPacketRecordingServerPlayer(
        helper, LISTENER_POSITION, "ranged_attack_sound_listener", sentPackets);
    sentPackets.clear();

    List<Projectile> projectiles =
        AttackHandler.performCustomProjectileAttack(
            easyNPC.getLivingEntity(), spawnTarget(helper), ENABLED_PROJECTILE_DATA);
    projectiles.forEach(Projectile::discard);

    Vec3 npcPosition = easyNPC.getLivingEntity().position();
    return sentPackets.stream()
        .filter(ClientboundSoundPacket.class::isInstance)
        .map(ClientboundSoundPacket.class::cast)
        .filter(
            soundPacket ->
                npcPosition.distanceTo(
                        new Vec3(soundPacket.getX(), soundPacket.getY(), soundPacket.getZ()))
                    < MAXIMUM_SOUND_DISTANCE)
        .map(soundPacket -> soundPacket.getSound().value())
        .toList();
  }

  private static Pig spawnTarget(GameTestHelper helper) {
    Pig target = GameTestHelpers.spawnEntityType(helper, EntityType.PIG);
    target.setPos(helper.absoluteVec(TARGET_POSITION));
    target.setNoAi(true);
    return target;
  }

  private static EasyNPCBullet createBulletBeforeTarget(
      EasyNPC<?> easyNPC, Pig target, ProjectileData projectileData) {
    EasyNPCBullet bullet = EasyNPCBullet.create(easyNPC.getLivingEntity(), projectileData);
    bullet.setPos(target.position().add(BULLET_OFFSET_FROM_TARGET));
    bullet.setDeltaMovement(BULLET_MOTION_TOWARDS_TARGET);
    return bullet;
  }

  private static CompoundTag saveEntity(GameTestHelper helper, Entity entity) {
    TagValueOutput valueOutput =
        TagValueOutput.createWithContext(
            ProblemReporter.DISCARDING, helper.getLevel().registryAccess());
    entity.saveWithoutId(valueOutput);
    return valueOutput.buildResult();
  }
}
