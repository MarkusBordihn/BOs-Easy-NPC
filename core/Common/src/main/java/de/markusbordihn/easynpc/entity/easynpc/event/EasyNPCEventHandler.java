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

package de.markusbordihn.easynpc.entity.easynpc.event;

import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.level.portal.DimensionTransition;

public final class EasyNPCEventHandler {

  public static <E extends PathfinderMob> void handlePlayerJoinEvent(
      EasyNPC<E> easyNPC, ServerPlayer serverPlayer) {
    ObjectiveDataCapable<E> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.onPlayerJoinUpdateObjective(serverPlayer);
    }
  }

  public static <E extends PathfinderMob> void handlePlayerLeaveEvent(
      EasyNPC<E> easyNPC, ServerPlayer serverPlayer) {
    ObjectiveDataCapable<E> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.onPlayerLeaveUpdateObjective(serverPlayer);
    }
  }

  public static <E extends PathfinderMob> void handleLivingEntityJoinEvent(
      EasyNPC<E> easyNPC, LivingEntity livingEntity) {
    ObjectiveDataCapable<E> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.onLivingEntityJoinUpdateObjective(livingEntity);
    }
  }

  public static <E extends PathfinderMob> void handleLivingEntityLeaveEvent(
      EasyNPC<E> easyNPC, LivingEntity livingEntity) {
    ObjectiveDataCapable<E> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.onLivingEntityLeaveUpdateObjective(livingEntity);
    }
  }

  public static <E extends PathfinderMob> void handleEasyNPCJoinEvent(
      EasyNPC<E> easyNPC, EasyNPC<?> entity) {
    ObjectiveDataCapable<E> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData != null && entity != null) {
      objectiveData.onEasyNPCJoinUpdateObjective(entity);
    }
  }

  public static <E extends PathfinderMob> void handleEasyNPCLeaveEvent(
      EasyNPC<E> easyNPC, EasyNPC<?> entity) {
    ObjectiveDataCapable<E> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData != null && entity != null) {
      objectiveData.onEasyNPCLeaveUpdateObjective(entity);
    }
  }

  public static <E extends PathfinderMob> void handleDieEvent(
      EasyNPC<E> easyNPC, DamageSource damageSource) {
    TradingDataCapable<E> tradingData = easyNPC.getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.stopMerchantTrading();
    }

    ActionEventDataCapable<E> actionEventData = easyNPC.getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.handleActionEvent(
          ActionEventType.ON_DEATH, getServerPlayerFromDamageSource(damageSource));
    }
  }

  public static <E extends PathfinderMob> void handleKillEvent(EasyNPC<E> easyNPC) {
    TradingDataCapable<E> tradingData = easyNPC.getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.stopMerchantTrading();
    }

    ActionEventDataCapable<E> actionEventData = easyNPC.getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.handleActionEvent(ActionEventType.ON_KILL);
    }
  }

  public static <E extends PathfinderMob> void handleChangeDimensionEvent(
      EasyNPC<E> easyNPC, DimensionTransition dimensionTransition) {
    TradingDataCapable<E> tradingData = easyNPC.getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.stopMerchantTrading();
    }
  }

  public static <E extends PathfinderMob> void handleHurtEvent(
      EasyNPC<E> easyNPC, DamageSource damageSource, float damage) {
    ActionEventDataCapable<E> actionEventData = easyNPC.getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.handleActionEvent(
          ActionEventType.ON_HURT, getServerPlayerFromDamageSource(damageSource));
    }
  }

  private static ServerPlayer getServerPlayerFromDamageSource(DamageSource damageSource) {
    if (damageSource.getEntity() instanceof ServerPlayer serverPlayer) {
      return serverPlayer;
    }
    if (damageSource.getDirectEntity() instanceof Projectile projectile
        && projectile.getOwner() instanceof ServerPlayer serverPlayerOfProjectile) {
      return serverPlayerOfProjectile;
    }
    return null;
  }
}
