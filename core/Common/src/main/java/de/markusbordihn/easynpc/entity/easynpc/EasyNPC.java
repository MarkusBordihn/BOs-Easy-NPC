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

package de.markusbordihn.easynpc.entity.easynpc;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.npc.NPCType;
import de.markusbordihn.easynpc.data.npc.RawNPCType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.event.EasyNPCEventHandler;
import de.markusbordihn.easynpc.server.player.FakePlayer;
import java.util.Random;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.profiling.ProfilerFiller;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.control.LookControl;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.monster.RangedAttackMob;
import net.minecraft.world.entity.npc.Npc;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.portal.TeleportTransition;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface EasyNPC<E extends Mob> extends EasyNPCDataAccessors<E>, Npc {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  Identifier DEFAULT_CUSTOM_NPC_IDENTIFIER =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "general");

  Random randomNumber = new Random();

  int getNPCDataVersion();

  void setNPCDataVersion(int version);

  default NPCType getNPCType() {
    return RawNPCType.GENERIC;
  }

  default void registerEasyNPCDefaultData() {
    ObjectiveDataCapable<E> objectiveData = this.getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.registerStandardObjectives();
    }

    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.registerDefaultActionInteractionEvents();
    }
  }

  FakePlayer getFakePlayer(ServerLevel level, BlockPos blockPos);

  default LookControl getEntityLookControl() {
    return EasyNPCEntityAccess.getLookControl(this);
  }

  default PathfinderMob getPathfinderMob() {
    return EasyNPCEntityAccess.getPathfinderMob(this);
  }

  default Level getEntityLevel() {
    return EasyNPCEntityAccess.getLevel(this);
  }

  default ServerLevel getEntityServerLevel() {
    return EasyNPCEntityAccess.getServerLevel(this);
  }

  default boolean isClientSideInstance() {
    return EasyNPCEntityAccess.isClientSide(this);
  }

  default boolean isServerSideInstance() {
    return EasyNPCEntityAccess.isServerSide(this);
  }

  default LivingEntity getLivingEntity() {
    return EasyNPCEntityAccess.getLivingEntity(this);
  }

  default Merchant getMerchant() {
    return EasyNPCEntityAccess.getMerchant(this);
  }

  default RangedAttackMob getRangedAttackMob() {
    return EasyNPCEntityAccess.getRangedAttackMob(this);
  }

  default ProfilerFiller getProfiler() {
    return EasyNPCEntityAccess.getProfiler(this);
  }

  default Entity getEntity() {
    return EasyNPCEntityAccess.getEntity(this);
  }

  default Mob getMob() {
    return EasyNPCEntityAccess.getMob(this);
  }

  default UUID getEntityUUID() {
    return EasyNPCEntityAccess.getEntityUUID(this);
  }

  default Component getEntityTypeName() {
    return EasyNPCEntityAccess.getEntityTypeName(this);
  }

  default String getEntityTypeId() {
    return EasyNPCEntityAccess.getEntityTypeId(this);
  }

  default Identifier getCustomNPCIdentifier() {
    if (this.getEasyNPCPresetData() != null) {
      Identifier customIdentifier = this.getEasyNPCPresetData().getCustomIdentifier();
      if (customIdentifier != null) {
        return customIdentifier;
      }
    }

    return DEFAULT_CUSTOM_NPC_IDENTIFIER;
  }

  default CrossbowAttackMob getCrossbowAttackMob() {
    return EasyNPCEntityAccess.getCrossbowAttackMob(this);
  }

  default void handlePlayerJoinEvent(ServerPlayer serverPlayer) {
    EasyNPCEventHandler.handlePlayerJoinEvent(this, serverPlayer);
  }

  default void handlePlayerLeaveEvent(ServerPlayer serverPlayer) {
    EasyNPCEventHandler.handlePlayerLeaveEvent(this, serverPlayer);
  }

  default void handleLivingEntityJoinEvent(LivingEntity livingEntity) {
    EasyNPCEventHandler.handleLivingEntityJoinEvent(this, livingEntity);
  }

  default void handleLivingEntityLeaveEvent(LivingEntity livingEntity) {
    EasyNPCEventHandler.handleLivingEntityLeaveEvent(this, livingEntity);
  }

  default void handleEasyNPCJoinEvent(EasyNPC<?> entity) {
    EasyNPCEventHandler.handleEasyNPCJoinEvent(this, entity);
  }

  default void handleEasyNPCLeaveEvent(EasyNPC<?> entity) {
    EasyNPCEventHandler.handleEasyNPCLeaveEvent(this, entity);
  }

  default void handleDieEvent(DamageSource damageSource) {
    EasyNPCEventHandler.handleDieEvent(this, damageSource);
  }

  default void handleKillEvent() {
    EasyNPCEventHandler.handleKillEvent(this);
  }

  /**
   * Handle the event when the EasyNPC is changing dimension.
   *
   * @param TeleportTransition The dimension transition event.
   */
  default void handleChangeDimensionEvent(TeleportTransition TeleportTransition) {
    EasyNPCEventHandler.handleChangeDimensionEvent(this, TeleportTransition);
  }

  default void handleHurtEvent(DamageSource damageSource, float damage) {
    EasyNPCEventHandler.handleHurtEvent(this, damageSource, damage);
  }

  /**
   * Define the synched entity data for the EasyNPC.
   *
   * @param builder The synched entity data builder.
   * @param synchedDataIndex The index of the synched data.
   * @param defaultData The default data to set.
   */
  <T> void defineSynchedEntityData(
      SynchedEntityData.Builder builder, SynchedDataIndex synchedDataIndex, T defaultData);

  default <T> void setSynchedEntityData(SynchedDataIndex synchedDataIndex, T data) {
    if (synchedDataIndex.persistent) {
      StatusDataCapable<?> statusData = getEasyNPCStatusData();
      if (statusData != null) {
        statusData.markNPCDataUpdated();
      }
    }
    setSynchedEntityData(synchedDataIndex, data, false);
  }

  <T> void setSynchedEntityData(SynchedDataIndex synchedDataIndex, T data, boolean forceUpdate);

  <T> T getSynchedEntityData(SynchedDataIndex synchedDataIndex);

  GoalSelector getEntityGoalSelector();

  GoalSelector getEntityTargetSelector();
}
