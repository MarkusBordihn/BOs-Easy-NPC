/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.data.objective;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.BowAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CrossbowAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomLookAtPlayerGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomMeleeAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomOwnerHurtByTargetGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomPanicGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.FollowLivingEntityGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.GunAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.LookAtEntityByUUIDGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.MoveBackToHomeGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.RandomStrollAroundGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.RandomStrollAroundHomeGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.ResetLookAtPlayerGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.ZombieAttackGoal;
import java.util.UUID;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.AvoidEntityGoal;
import net.minecraft.world.entity.ai.goal.FleeSunGoal;
import net.minecraft.world.entity.ai.goal.FloatGoal;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.GolemRandomStrollInVillageGoal;
import net.minecraft.world.entity.ai.goal.MoveBackToVillageGoal;
import net.minecraft.world.entity.ai.goal.MoveThroughVillageGoal;
import net.minecraft.world.entity.ai.goal.OpenDoorGoal;
import net.minecraft.world.entity.ai.goal.RandomLookAroundGoal;
import net.minecraft.world.entity.ai.goal.RandomSwimmingGoal;
import net.minecraft.world.entity.ai.goal.RestrictSunGoal;
import net.minecraft.world.entity.ai.goal.TemptGoal;
import net.minecraft.world.entity.ai.goal.WaterAvoidingRandomStrollGoal;
import net.minecraft.world.entity.ai.goal.target.HurtByTargetGoal;
import net.minecraft.world.entity.ai.goal.target.NearestAttackableTargetGoal;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.Enemy;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.crafting.Ingredient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ObjectiveUtils {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ObjectiveUtils() {}

  private static boolean requiresPathfinderMob(ObjectiveType objectiveType, EasyNPC<?> easyNPC) {
    if (easyNPC.getPathfinderMob() == null) {
      log.debug(
          "Entity {} is not a PathfinderMob, cannot use {} objective!",
          easyNPC.getEntity(),
          objectiveType);
      return false;
    }
    return true;
  }

  public static Goal createObjectiveGoal(
      ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC) {
    Entity targetOwner = objectiveDataEntry.getTargetOwner(easyNPC);
    ObjectiveType objectiveType = objectiveDataEntry.getType();

    switch (objectiveType) {
      case FOLLOW_PLAYER:
        ServerPlayer targetServerPlayer = objectiveDataEntry.getTargetPlayer();
        if (targetServerPlayer != null && !targetServerPlayer.isRemoved()) {
          return new FollowLivingEntityGoal(
              easyNPC,
              targetServerPlayer,
              objectiveDataEntry.getSpeedModifier(),
              objectiveDataEntry.getStopDistance(),
              objectiveDataEntry.getStartDistance());
        } else {
          log.debug(
              "Unable to find valid player {} for {}!",
              objectiveDataEntry.getTargetPlayer(),
              objectiveDataEntry);
        }
        break;
      case FOLLOW_OWNER:
        if (targetOwner instanceof LivingEntity livingEntity && !livingEntity.isRemoved()) {
          return new FollowLivingEntityGoal(
              easyNPC,
              livingEntity,
              objectiveDataEntry.getSpeedModifier(),
              objectiveDataEntry.getStopDistance(),
              objectiveDataEntry.getStartDistance());
        } else {
          log.debug(
              "Unable to find valid owner {} for {} with {}!",
              targetOwner,
              easyNPC.getEntity(),
              objectiveDataEntry);
        }
        break;
      case FOLLOW_ENTITY_BY_UUID:
        LivingEntity targetEntityMob = objectiveDataEntry.getTargetEntity(easyNPC);
        if (targetEntityMob != null && !targetEntityMob.isRemoved()) {
          return new FollowLivingEntityGoal(
              easyNPC,
              targetEntityMob,
              objectiveDataEntry.getSpeedModifier(),
              objectiveDataEntry.getStopDistance(),
              objectiveDataEntry.getStartDistance());
        } else {
          log.debug(
              "Unable to find living entity {} for {}!",
              objectiveDataEntry.getTargetEntityUUID(),
              objectiveDataEntry);
        }
        break;
      case FOLLOW_ITEM:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        String itemTag = objectiveDataEntry.getTargetItemTag();
        if (itemTag != null && !itemTag.isEmpty()) {
          Item item = BuiltInRegistries.ITEM.get(ResourceLocation.parse(itemTag));
          if (item != net.minecraft.world.item.Items.AIR) {
            return new TemptGoal(
                easyNPC.getPathfinderMob(),
                objectiveDataEntry.getSpeedModifier(),
                Ingredient.of(item),
                false);
          }
          log.debug("Unable to find item {} for {}!", itemTag, objectiveDataEntry);
        }
        break;
      case RANDOM_STROLL:
        return new RandomStrollAroundGoal<>(easyNPC, objectiveDataEntry.getSpeedModifier());
      case WATER_AVOIDING_RANDOM_STROLL:
        if (easyNPC.getPathfinderMob() != null) {
          return new WaterAvoidingRandomStrollGoal(
              easyNPC.getPathfinderMob(), objectiveDataEntry.getSpeedModifier());
        }
        return new RandomStrollAroundGoal<>(easyNPC, objectiveDataEntry.getSpeedModifier());
      case MOVE_THROUGH_VILLAGE:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new MoveThroughVillageGoal(
            easyNPC.getPathfinderMob(),
            objectiveDataEntry.getSpeedModifier(),
            objectiveDataEntry.getOnlyAtNight(),
            objectiveDataEntry.getDistanceToPoi(),
            objectiveDataEntry.getCanDealWithDoors());
      case MOVE_BACK_TO_HOME:
        return new MoveBackToHomeGoal<>(
            easyNPC, objectiveDataEntry.getSpeedModifier(), objectiveDataEntry.getStopDistance());
      case MOVE_BACK_TO_VILLAGE:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new MoveBackToVillageGoal(
            easyNPC.getPathfinderMob(), objectiveDataEntry.getSpeedModifier(), false);
      case RANDOM_STROLL_AROUND_HOME:
        return new RandomStrollAroundHomeGoal<>(easyNPC, objectiveDataEntry.getSpeedModifier());
      case RANDOM_STROLL_IN_VILLAGE:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new GolemRandomStrollInVillageGoal(
            easyNPC.getPathfinderMob(), objectiveDataEntry.getSpeedModifier());
      case CROSSBOW_ATTACK:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new CrossbowAttackGoal<>(
            easyNPC, objectiveDataEntry.getSpeedModifier(), objectiveDataEntry.getAttackRadius());
      case BOW_ATTACK:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new BowAttackGoal<>(
            easyNPC,
            objectiveDataEntry.getSpeedModifier(),
            objectiveDataEntry.getAttackInterval(),
            objectiveDataEntry.getAttackRadius());
      case MELEE_ATTACK:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new CustomMeleeAttackGoal<>(
            easyNPC, objectiveDataEntry.getSpeedModifier(), objectiveDataEntry.isMustSeeTarget());
      case ZOMBIE_ATTACK:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new ZombieAttackGoal<>(
            easyNPC, objectiveDataEntry.getSpeedModifier(), objectiveDataEntry.isMustSeeTarget());
      case GUN_ATTACK:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new GunAttackGoal<>(
            easyNPC,
            objectiveDataEntry.getSpeedModifier(),
            objectiveDataEntry.getAttackInterval(),
            objectiveDataEntry.getAttackRadius());
      case RANDOM_SWIMMING:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new RandomSwimmingGoal(
            easyNPC.getPathfinderMob(),
            objectiveDataEntry.getSpeedModifier(),
            objectiveDataEntry.getInterval());
      case FLOAT:
        return new FloatGoal(easyNPC.getMob());
      case OPEN_DOOR:
        return new OpenDoorGoal(easyNPC.getMob(), false);
      case CLOSE_DOOR:
        return new OpenDoorGoal(easyNPC.getMob(), true);
      case LOOK_AT_RESET:
        return new ResetLookAtPlayerGoal<>(easyNPC);
      case LOOK_AT_PLAYER:
        return new CustomLookAtPlayerGoal<>(
            easyNPC,
            Player.class,
            objectiveDataEntry.getLookDistance(),
            objectiveDataEntry.getProbability());
      case LOOK_AT_MOB:
        return new CustomLookAtPlayerGoal<>(
            easyNPC,
            Mob.class,
            objectiveDataEntry.getLookDistance(),
            objectiveDataEntry.getProbability());
      case LOOK_AT_ANIMAL:
        return new CustomLookAtPlayerGoal<>(
            easyNPC,
            Animal.class,
            objectiveDataEntry.getLookDistance(),
            objectiveDataEntry.getProbability());
      case LOOK_AT_ENTITY_BY_UUID:
        UUID lookAtEntityUUID = objectiveDataEntry.getTargetEntityUUID();
        if (lookAtEntityUUID != null) {
          return new LookAtEntityByUUIDGoal<>(
              easyNPC, lookAtEntityUUID, objectiveDataEntry.getLookDistance());
        }
        log.debug("No valid target entity UUID for LOOK_AT_ENTITY_BY_UUID objective!");
        break;
      case LOOK_AT_OWNER:
        Entity lookAtOwner = objectiveDataEntry.getTargetOwner(easyNPC);
        if (lookAtOwner instanceof LivingEntity ownerEntity && !ownerEntity.isRemoved()) {
          return new LookAtEntityByUUIDGoal<>(
              easyNPC, ownerEntity.getUUID(), objectiveDataEntry.getLookDistance());
        }
        log.debug("No valid owner for LOOK_AT_OWNER objective!");
        break;
      case LOOK_RANDOM_AROUND:
        return new RandomLookAroundGoal(easyNPC.getMob());
      case PANIC:
        return new CustomPanicGoal<>(easyNPC, objectiveDataEntry.getSpeedModifier());
      case AVOID_SUN:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new RestrictSunGoal(easyNPC.getPathfinderMob());
      case FLEE_SUN:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new FleeSunGoal(easyNPC.getPathfinderMob(), objectiveDataEntry.getSpeedModifier());
      case FLEE_CREEPER:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new AvoidEntityGoal<>(
            easyNPC.getPathfinderMob(),
            Creeper.class,
            objectiveDataEntry.getLookDistance(),
            objectiveDataEntry.getSpeedModifier(),
            objectiveDataEntry.getSpeedModifier() * 1.2D);
      case FLEE_MOB:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new AvoidEntityGoal<>(
            easyNPC.getPathfinderMob(),
            Mob.class,
            objectiveDataEntry.getLookDistance(),
            objectiveDataEntry.getSpeedModifier(),
            objectiveDataEntry.getSpeedModifier() * 1.2D);
      case FLEE_MONSTER:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new AvoidEntityGoal<>(
            easyNPC.getPathfinderMob(),
            Monster.class,
            objectiveDataEntry.getLookDistance(),
            objectiveDataEntry.getSpeedModifier(),
            objectiveDataEntry.getSpeedModifier() * 1.2D);
      case FLEE_PLAYER:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new AvoidEntityGoal<>(
            easyNPC.getPathfinderMob(),
            Player.class,
            objectiveDataEntry.getLookDistance(),
            objectiveDataEntry.getSpeedModifier(),
            objectiveDataEntry.getSpeedModifier() * 1.2D);
      case FLEE_VILLAGER:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        return new AvoidEntityGoal<>(
            easyNPC.getPathfinderMob(),
            AbstractVillager.class,
            objectiveDataEntry.getLookDistance(),
            objectiveDataEntry.getSpeedModifier(),
            objectiveDataEntry.getSpeedModifier() * 1.2D);
      default:
        return null;
    }
    return null;
  }

  public static Goal createObjectiveTarget(
      ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC) {
    Mob mob = easyNPC.getMob();
    ObjectiveType objectiveType = objectiveDataEntry.getType();

    switch (objectiveType) {
      case ATTACK_ANIMAL:
        return new NearestAttackableTargetGoal<>(
            mob, Animal.class, objectiveDataEntry.isMustSeeTarget());
      case ATTACK_PLAYER:
        return new NearestAttackableTargetGoal<>(
            mob, Player.class, objectiveDataEntry.isMustSeeTarget());
      case ATTACK_PLAYER_WITHOUT_OWNER:
        return new NearestAttackableTargetGoal<>(
            mob,
            Player.class,
            objectiveDataEntry.getInterval(),
            objectiveDataEntry.isMustSeeTarget(),
            objectiveDataEntry.isMustReachTarget(),
            entity ->
                easyNPC.getEasyNPCOwnerData() != null
                    && entity != easyNPC.getEasyNPCOwnerData().getOwner());
      case ATTACK_MONSTER:
        return new NearestAttackableTargetGoal<>(
            mob, Monster.class, objectiveDataEntry.isMustSeeTarget());
      case ATTACK_MOB_WITHOUT_CREEPER:
        return new NearestAttackableTargetGoal<>(
            mob,
            Mob.class,
            objectiveDataEntry.getInterval(),
            objectiveDataEntry.isMustSeeTarget(),
            objectiveDataEntry.isMustReachTarget(),
            entity -> entity instanceof Enemy && !(entity instanceof Creeper));
      case ATTACK_MOB:
        return new NearestAttackableTargetGoal<>(
            mob,
            Mob.class,
            objectiveDataEntry.getInterval(),
            objectiveDataEntry.isMustSeeTarget(),
            objectiveDataEntry.isMustReachTarget(),
            Enemy.class::isInstance);
      case ATTACK_VILLAGER:
        return new NearestAttackableTargetGoal<>(
            mob, AbstractVillager.class, objectiveDataEntry.isMustSeeTarget());
      case OWNER_HURT_BY_TARGET:
        return new CustomOwnerHurtByTargetGoal<>(easyNPC);
      case HURT_BY_TARGET:
        if (!requiresPathfinderMob(objectiveType, easyNPC)) {
          return null;
        }
        HurtByTargetGoal hurtByTargetGoal = new HurtByTargetGoal(easyNPC.getPathfinderMob());
        hurtByTargetGoal.setAlertOthers();
        return hurtByTargetGoal;
      default:
        return null;
    }
  }
}
