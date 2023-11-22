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
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.ai.goal.CrossbowAttackGoal;
import de.markusbordihn.easynpc.entity.ai.goal.CustomLookAtPlayerGoal;
import de.markusbordihn.easynpc.entity.ai.goal.CustomMeleeAttackGoal;
import de.markusbordihn.easynpc.entity.ai.goal.FollowLivingEntityGoal;
import de.markusbordihn.easynpc.entity.ai.goal.ResetLookAtPlayerGoal;
import de.markusbordihn.easynpc.entity.ai.goal.ZombieAttackGoal;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.FleeSunGoal;
import net.minecraft.world.entity.ai.goal.FloatGoal;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.GolemRandomStrollInVillageGoal;
import net.minecraft.world.entity.ai.goal.MoveBackToVillageGoal;
import net.minecraft.world.entity.ai.goal.MoveThroughVillageGoal;
import net.minecraft.world.entity.ai.goal.OpenDoorGoal;
import net.minecraft.world.entity.ai.goal.PanicGoal;
import net.minecraft.world.entity.ai.goal.RandomLookAroundGoal;
import net.minecraft.world.entity.ai.goal.RandomStrollGoal;
import net.minecraft.world.entity.ai.goal.RandomSwimmingGoal;
import net.minecraft.world.entity.ai.goal.RangedBowAttackGoal;
import net.minecraft.world.entity.ai.goal.RestrictSunGoal;
import net.minecraft.world.entity.ai.goal.WaterAvoidingRandomStrollGoal;
import net.minecraft.world.entity.ai.goal.target.NearestAttackableTargetGoal;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.Enemy;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.entity.player.Player;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ObjectiveUtils {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ObjectiveUtils() {}

  public static Goal createObjectiveGoal(ObjectiveData objectiveData, EasyNPCEntity easyNPCEntity) {
    Entity targetOwner = objectiveData.getTargetOwner(easyNPCEntity);

    switch (objectiveData.getType()) {
      case FOLLOW_PLAYER:
        ServerPlayer targetServerPlayer = objectiveData.getTargetPlayer();
        if (targetServerPlayer != null && !targetServerPlayer.isRemoved()) {
          return new FollowLivingEntityGoal(
              easyNPCEntity,
              targetServerPlayer,
              objectiveData.getSpeedModifier(),
              objectiveData.getStopDistance(),
              objectiveData.getStartDistance(),
              easyNPCEntity instanceof FlyingAnimal);
        } else {
          log.error(
              "Unable to find player {} for {}!", objectiveData.getTargetPlayer(), objectiveData);
        }
        break;
      case FOLLOW_OWNER:
        if (targetOwner instanceof LivingEntity livingEntity && !livingEntity.isRemoved()) {
          return new FollowLivingEntityGoal(
              easyNPCEntity,
              livingEntity,
              objectiveData.getSpeedModifier(),
              objectiveData.getStopDistance(),
              objectiveData.getStartDistance(),
              easyNPCEntity instanceof FlyingAnimal);
        } else {
          log.error(
              "Unable to find valid owner {} for {} with {}!",
              targetOwner,
              easyNPCEntity,
              objectiveData);
        }
        break;
      case FOLLOW_ENTITY_BY_UUID:
        LivingEntity targetEntityMob = objectiveData.getTargetEntity(easyNPCEntity);
        if (targetEntityMob != null && !targetEntityMob.isRemoved()) {
          return new FollowLivingEntityGoal(
              easyNPCEntity,
              targetEntityMob,
              objectiveData.getSpeedModifier(),
              objectiveData.getStopDistance(),
              objectiveData.getStartDistance(),
              easyNPCEntity instanceof FlyingAnimal);
        } else {
          log.error(
              "Unable to find living entity {} for {}!",
              objectiveData.getTargetEntityUUID(),
              objectiveData);
        }
        break;
      case RANDOM_STROLL:
        return new RandomStrollGoal(easyNPCEntity, objectiveData.getSpeedModifier());
      case WATER_AVOIDING_RANDOM_STROLL:
        return new WaterAvoidingRandomStrollGoal(easyNPCEntity, objectiveData.getSpeedModifier());
      case MOVE_THROUGH_VILLAGE:
        return new MoveThroughVillageGoal(
            easyNPCEntity,
            objectiveData.getSpeedModifier(),
            objectiveData.getOnlyAtNight(),
            objectiveData.getDistanceToPoi(),
            objectiveData.getCanDealWithDoors());
      case MOVE_BACK_TO_VILLAGE:
        return new MoveBackToVillageGoal(easyNPCEntity, objectiveData.getSpeedModifier(), false);
      case RANDOM_STROLL_IN_VILLAGE:
        return new GolemRandomStrollInVillageGoal(easyNPCEntity, objectiveData.getSpeedModifier());
      case CROSSBOW_ATTACK:
        return new CrossbowAttackGoal(
            easyNPCEntity, objectiveData.getSpeedModifier(), objectiveData.getAttackRadius());
      case BOW_ATTACK:
        return new RangedBowAttackGoal<>(
            easyNPCEntity,
            objectiveData.getSpeedModifier(),
            objectiveData.getAttackInterval(),
            objectiveData.getAttackRadius());
      case MELEE_ATTACK:
        return new CustomMeleeAttackGoal(
            easyNPCEntity, objectiveData.getSpeedModifier(), objectiveData.isMustSeeTarget());
      case ZOMBIE_ATTACK:
        return new ZombieAttackGoal(
            easyNPCEntity, objectiveData.getSpeedModifier(), objectiveData.isMustSeeTarget());
      case RANDOM_SWIMMING:
        return new RandomSwimmingGoal(
            easyNPCEntity, objectiveData.getSpeedModifier(), objectiveData.getInterval());
      case FLOAT:
        return new FloatGoal(easyNPCEntity);
      case OPEN_DOOR:
        return new OpenDoorGoal(easyNPCEntity, false);
      case CLOSE_DOOR:
        return new OpenDoorGoal(easyNPCEntity, true);
      case LOOK_AT_RESET:
        return new ResetLookAtPlayerGoal(easyNPCEntity);
      case LOOK_AT_PLAYER:
        return new CustomLookAtPlayerGoal(
            easyNPCEntity,
            Player.class,
            objectiveData.getLookDistance(),
            objectiveData.getProbability());
      case LOOK_AT_MOB:
        return new CustomLookAtPlayerGoal(
            easyNPCEntity,
            Mob.class,
            objectiveData.getLookDistance(),
            objectiveData.getProbability());
      case LOOK_AT_ANIMAL:
        return new CustomLookAtPlayerGoal(
            easyNPCEntity,
            Animal.class,
            objectiveData.getLookDistance(),
            objectiveData.getProbability());
      case LOOK_RANDOM_AROUND:
        return new RandomLookAroundGoal(easyNPCEntity);
      case PANIC:
        return new PanicGoal(easyNPCEntity, objectiveData.getSpeedModifier());
      case AVOID_SUN:
        return new RestrictSunGoal(easyNPCEntity);
      case FLEE_SUN:
        return new FleeSunGoal(easyNPCEntity, objectiveData.getSpeedModifier());
      default:
        return null;
    }
    return null;
  }

  public static Goal createObjectiveTarget(
      ObjectiveData objectiveData, EasyNPCEntity easyNPCEntity) {
    return switch (objectiveData.getType()) {
      case ATTACK_ANIMAL -> new NearestAttackableTargetGoal<>(
          easyNPCEntity, Animal.class, objectiveData.isMustSeeTarget());
      case ATTACK_PLAYER -> new NearestAttackableTargetGoal<>(
          easyNPCEntity, Player.class, objectiveData.isMustSeeTarget());
      case ATTACK_MONSTER -> new NearestAttackableTargetGoal<>(
          easyNPCEntity, Monster.class, objectiveData.isMustSeeTarget());
      case ATTACK_MOB_WITHOUT_CREEPER -> new NearestAttackableTargetGoal<>(
          easyNPCEntity,
          Mob.class,
          objectiveData.getInterval(),
          false,
          false,
          entity -> entity instanceof Enemy && !(entity instanceof Creeper));
      case ATTACK_MOB -> new NearestAttackableTargetGoal<>(
          easyNPCEntity,
          Mob.class,
          objectiveData.getInterval(),
          false,
          false,
          Enemy.class::isInstance);
      case ATTACK_VILLAGER -> new NearestAttackableTargetGoal<>(
          easyNPCEntity, AbstractVillager.class, objectiveData.isMustSeeTarget());
      default -> null;
    };
  }
}
