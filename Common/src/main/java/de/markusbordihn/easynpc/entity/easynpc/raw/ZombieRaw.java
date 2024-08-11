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

package de.markusbordihn.easynpc.entity.easynpc.raw;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.data.synched.SynchedEntityData;
import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.entity.EasyNPCBaseEntity;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPCBase;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.InteractionHandler;
import de.markusbordihn.easynpc.server.player.FakePlayer;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.EnumMap;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.util.TimeUtil;
import net.minecraft.util.valueproviders.UniformInt;
import net.minecraft.world.DifficultyInstance;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.MobType;
import net.minecraft.world.entity.SpawnGroupData;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.monster.Zombie;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ProjectileWeaponItem;
import net.minecraft.world.item.trading.MerchantOffers;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.ServerLevelAccessor;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ZombieRaw extends Zombie implements EasyNPCBase<Zombie> {

  public static final String ID = "zombie_raw";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final EnumMap<SynchedDataIndex, EntityDataAccessor<?>> entityDataAccessorMap =
      new EnumMap<>(SynchedDataIndex.class);
  private static final UniformInt PERSISTENT_ANGER_TIME = TimeUtil.rangeOfSeconds(20, 39);

  static {
    EasyNPCBase.registerEasyNPCDataSerializers();
    EasyNPCBase.registerEasyNPCSyncedData(entityDataAccessorMap, ZombieRaw.class);
  }

  private final EnumMap<TickerType, Integer> tickerMap = new EnumMap<>(TickerType.class);
  protected MerchantOffers merchantTradingOffers;
  private ServerEntityData serverEntityData;
  private int attackAnimationTick;
  private int npcDataVersion = -1;
  private UUID persistentAngerTarget;
  private int remainingPersistentAngerTime;
  private SynchedEntityData synchedEntityData;
  private Player tradingPlayer;
  private FakePlayer fakePlayer;

  public ZombieRaw(EntityType<? extends Zombie> entityType, Level level) {
    super(entityType, level);
  }

  @Override
  public FakePlayer getFakePlayer(ServerLevel level, BlockPos blockPos) {
    if (FakePlayer.isInvalidFakePlayer(this.fakePlayer)) {
      this.fakePlayer = new FakePlayer(level, blockPos);
      return this.fakePlayer;
    }
    return this.fakePlayer.updatePosition(level, blockPos);
  }

  @Override
  public int getTicker(TickerType tickerType) {
    return this.tickerMap.getOrDefault(tickerType, 0);
  }

  @Override
  public void setTicker(TickerType tickerType, int ticker) {
    this.tickerMap.put(tickerType, ticker);
  }

  @Override
  public void onCrossbowAttackPerformed() {
    this.noActionTime = 0;
  }

  @Override
  public int getAttackAnimationTick() {
    return this.attackAnimationTick;
  }

  @Override
  public boolean doHurtTarget(Entity entity) {
    this.attackAnimationTick = 10;
    this.level.broadcastEntityEvent(this, (byte) 4);
    return super.doHurtTarget(entity);
  }

  @Override
  public void handleEntityEvent(byte flag) {
    super.handleEntityEvent(flag);
    if (flag == 4) {
      this.attackAnimationTick = 10;
    }
  }

  @Override
  public void aiStep() {
    super.aiStep();

    if (this.isClientSide()) {
      this.updateSwingTime();
      if (this.attackAnimationTick > 0) {
        --this.attackAnimationTick;
      }
    } else {
      this.updatePersistentAnger((ServerLevel) this.level, true);
    }
  }

  @Override
  public Player getTradingPlayer() {
    return this.tradingPlayer;
  }

  @Override
  public void setTradingPlayer(Player player) {
    this.tradingPlayer = player;
  }

  @Override
  public Component getName() {
    Component component = this.getCustomName();
    return component != null ? TextUtils.removeAction(component) : this.getTypeName();
  }

  @Override
  public boolean hurt(DamageSource damageSource, float damage) {
    this.handleHurtEvent(damageSource, damage);
    return super.hurt(damageSource, damage);
  }

  @Override
  public void die(DamageSource damageSource) {
    this.handleDieEvent(damageSource);
    super.die(damageSource);
  }

  @Override
  public Entity changeDimension(ServerLevel serverLevel) {
    this.handleChangeDimensionEvent(serverLevel);
    return super.changeDimension(serverLevel);
  }

  @Override
  public InteractionResult mobInteract(Player player, InteractionHand hand) {
    return InteractionHandler.handleMobInteraction(this, player, hand);
  }

  @Override
  public MerchantOffers getMerchantTradingOffers() {
    return this.merchantTradingOffers;
  }

  @Override
  public void setMerchantTradingOffers(MerchantOffers merchantOffers) {
    this.merchantTradingOffers = merchantOffers;
  }

  @Override
  public int getNPCDataVersion() {
    return this.npcDataVersion;
  }

  @Override
  public void setNPCDataVersion(int version) {
    this.npcDataVersion = version;
  }

  @Override
  public SpawnGroupData finalizeSpawn(
      ServerLevelAccessor serverLevelAccessor,
      DifficultyInstance difficulty,
      MobSpawnType mobSpawnType,
      SpawnGroupData spawnGroupData,
      CompoundTag compoundTag) {
    return finalizeEasyNPCSpawn(
        super.finalizeSpawn(
            serverLevelAccessor, difficulty, mobSpawnType, spawnGroupData, compoundTag));
  }

  @Override
  public void handleEasyNPCJoin(EasyNPC<?> easyNPC) {
    this.onEasyNPCJoinUpdateObjective(easyNPC);
  }

  @Override
  public void handleEasyNPCLeave(EasyNPC<?> easyNPC) {
    this.onEasyNPCLeaveUpdateObjective(easyNPC);
  }

  @Override
  public void handlePlayerJoin(ServerPlayer serverPlayer) {
    this.onPlayerJoinUpdateObjective(serverPlayer);
  }

  @Override
  public void handlePlayerLeave(ServerPlayer serverPlayer) {
    this.onPlayerLeaveUpdateObjective(serverPlayer);
  }

  @Override
  public void handleLivingEntityJoin(LivingEntity livingEntity) {
    this.onLivingEntityJoinUpdateObjective(livingEntity);
  }

  @Override
  public void handleLivingEntityLeave(LivingEntity livingEntity) {
    this.onLivingEntityLeaveUpdateObjective(livingEntity);
  }

  @Override
  public boolean canFireProjectileWeapon(ProjectileWeaponItem projectileWeaponItem) {
    return AttackHandler.canFireProjectileWeapon(projectileWeaponItem);
  }

  @Override
  public int getRemainingPersistentAngerTime() {
    return this.remainingPersistentAngerTime;
  }

  @Override
  public void setRemainingPersistentAngerTime(int remainingPersistentAngerTime) {
    this.remainingPersistentAngerTime = remainingPersistentAngerTime;
  }

  @Override
  public UUID getPersistentAngerTarget() {
    return this.persistentAngerTarget;
  }

  @Override
  public void setPersistentAngerTarget(UUID targetUUID) {
    this.persistentAngerTarget = targetUUID;
  }

  @Override
  public void startPersistentAngerTimer() {
    this.setRemainingPersistentAngerTime(PERSISTENT_ANGER_TIME.sample(this.random));
  }

  @Override
  public GoalSelector getEntityGoalSelector() {
    return this.goalSelector;
  }

  @Override
  public GoalSelector getEntityTargetSelector() {
    return this.targetSelector;
  }

  @Override
  public <T> void defineSynchedEntityData(SynchedDataIndex synchedDataIndex, T defaultData) {
    if (this.synchedEntityData == null) {
      this.synchedEntityData = new SynchedEntityData(this, entityDataAccessorMap);
    }
    this.synchedEntityData.define(synchedDataIndex, defaultData);
  }

  @Override
  public <T> void setSynchedEntityData(SynchedDataIndex synchedDataIndex, T data) {
    this.synchedEntityData.set(synchedDataIndex, data);
  }

  @Override
  public <T> T getSynchedEntityData(SynchedDataIndex synchedDataIndex) {
    return this.synchedEntityData.get(synchedDataIndex);
  }

  @Override
  public void defineServerEntityData() {
    this.serverEntityData = new ServerEntityData(this);
  }

  @Override
  public ServerEntityData getServerEntityData() {
    if (this.serverEntityData == null) {
      this.defineServerEntityData();
    }
    return this.serverEntityData;
  }

  @Override
  public boolean canBeLeashed(Player player) {
    if (!this.isLeashed()
        && player instanceof ServerPlayer serverPlayer
        && (serverPlayer.isCreative() || isOwner(serverPlayer))) {
      return true;
    }
    return !this.isLeashed() && getAttributeDataLoaded() && getAttributeCanBeLeashed();
  }

  @Override
  public boolean isAttackable() {
    return getAttributeDataLoaded() && getAttributeIsAttackable();
  }

  @Override
  public boolean isPushable() {
    return getAttributeDataLoaded() && getAttributeIsPushable();
  }

  @Override
  protected void pushEntities() {
    if (getAttributeDataLoaded() && getAttributePushEntities()) {
      super.pushEntities();
    }
  }

  @Override
  public boolean isInvulnerable() {
    return getAttributeDataLoaded() ? !getAttributeIsAttackable() : super.isInvulnerable();
  }

  @Override
  public boolean isInvulnerableTo(DamageSource damageSource) {
    return isInvulnerable() || super.isInvulnerableTo(damageSource);
  }

  @Override
  public void playAmbientSound() {
    this.playDefaultAmbientSound();
  }

  @Override
  public int getAmbientSoundInterval() {
    return 240;
  }

  @Override
  public void playHurtSound(DamageSource damageSource) {
    this.playDefaultHurtSound(damageSource);
  }

  @Override
  protected void playStepSound(BlockPos blockPos, BlockState blockState) {
    this.playDefaultStepSound(blockPos, blockState);
  }

  @Override
  public SoundEvent getDeathSound() {
    return this.getDefaultDeathSound();
  }

  @Override
  protected void handleNetherPortal() {
    if (getAttributeDataLoaded() && getAttributeCanUseNetherPortal()) {
      super.handleNetherPortal();
    }
  }

  @Override
  public boolean removeWhenFarAway(double distance) {
    return false;
  }

  @Override
  public void baseTick() {
    super.baseTick();

    // Early exit for client side and dead entities.
    if (this.isClientSide() || !this.isAlive()) {
      return;
    }

    // Handle custom objective base tick.
    this.handleCustomObjectiveBaseTick();

    // Handle base tick for specific conditions.
    this.handleBaseTick();
  }

  @Override
  public void travel(Vec3 vec3) {
    this.handleNavigationTravelEvent(vec3);
    super.travel(vec3);
  }

  @Override
  public boolean isSaddleable() {
    return false;
  }

  @Override
  public void equipSaddle(SoundSource soundSource) {
    if (soundSource != null) {
      this.level.playSound(null, this, SoundEvents.PIG_SADDLE, soundSource, 0.5F, 1.0F);
    }
  }

  @Override
  public boolean isSaddled() {
    return false;
  }

  @Override
  public MobType getMobType() {
    return MobType.UNDEFINED;
  }

  @Override
  public boolean supportsPoseConfiguration() {
    return false;
  }

  @Override
  public boolean supportsScalingConfiguration() {
    return false;
  }

  @Override
  public boolean supportsDefaultRotationConfiguration() {
    return false;
  }

  @Override
  public boolean supportsSkinConfiguration() {
    return false;
  }

  @Override
  public boolean supportsChangeModelConfiguration() {
    return false;
  }

  @Override
  protected void defineSynchedData() {
    super.defineSynchedData();
    this.defineEasyNPCBaseSyncedData();
    this.defineEasyNPCBaseServerSideData();
  }

  @Override
  public void addAdditionalSaveData(CompoundTag compoundTag) {
    super.addAdditionalSaveData(compoundTag);
    this.addPersistentAngerSaveData(compoundTag);
    this.addEasyNPCBaseAdditionalSaveData(compoundTag);
  }

  @Override
  public void readAdditionalSaveData(CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);
    this.readPersistentAngerSaveData(this.level, compoundTag);
    this.readEasyNPCBaseAdditionalSaveData(compoundTag);
  }

  @Override
  public boolean equals(Object object) {
    if (this == object) {
      return true;
    }
    if (!(object instanceof EasyNPCBaseEntity<?> easyNPCBaseEntity)) {
      return false;
    }
    return java.util.Objects.equals(this.getUUID(), easyNPCBaseEntity.getUUID());
  }

  @Override
  public int hashCode() {
    return java.util.Objects.hash(this.getUUID());
  }
}
