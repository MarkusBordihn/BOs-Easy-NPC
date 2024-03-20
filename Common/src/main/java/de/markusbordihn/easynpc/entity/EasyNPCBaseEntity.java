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

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.custom.CustomDataAccessor;
import de.markusbordihn.easynpc.data.entity.CustomEntityData;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationData;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.data.GuiData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.NPCData;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionData;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.SpawnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import java.util.UUID;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.util.TimeUtil;
import net.minecraft.util.valueproviders.UniformInt;
import net.minecraft.world.DifficultyInstance;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.AgeableMob;
import net.minecraft.world.entity.EntityDimensions;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.NeutralMob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.SpawnGroupData;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.ProjectileWeaponItem;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.ServerLevelAccessor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCBaseEntity extends AgeableMob
    implements NeutralMob,
        Merchant,
        EasyNPC<AgeableMob>,
        ActionHandler<AgeableMob>,
        ActionEventData<AgeableMob>,
        AttackData<AgeableMob>,
        AttributeData<AgeableMob>,
        ConfigurationData<AgeableMob>,
        DialogData<AgeableMob>,
        GuiData<AgeableMob>,
        ModelData<AgeableMob>,
        NavigationData<AgeableMob>,
        NPCData<AgeableMob>,
        ObjectiveData<AgeableMob>,
        OwnerData<AgeableMob>,
        PresetData<AgeableMob>,
        ProfessionData<AgeableMob>,
        ScaleData<AgeableMob>,
        SkinData<AgeableMob>,
        SpawnerData<AgeableMob>,
        TradingData<AgeableMob>,
        VariantData<AgeableMob> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final UniformInt PERSISTENT_ANGER_TIME = TimeUtil.rangeOfSeconds(20, 39);

  static {
    // Register custom data serializers
    ActionEventData.registerActionEventDataSerializer();
    DialogData.registerDialogDataSerializer();
    ModelData.registerModelDataSerializer();
    ObjectiveData.registerObjectiveDataSerializer();
    ProfessionData.registerProfessionDataSerializer();
    SkinData.registerSkinDataSerializer();
    SpawnerData.registerSpawnerDataSerializer();
    TradingData.registerTradingDataSerializer();
  }

  private final CustomEntityData customEntityData = new CustomEntityData(this);
  protected MerchantOffers offers;
  private int remainingPersistentAngerTime;
  private UUID persistentAngerTarget;
  private int npcDataVersion = -1;
  private Player tradingPlayer;

  public EasyNPCBaseEntity(EntityType<? extends AgeableMob> entityType, Level world) {
    super(entityType, world);
    this.defineCustomData();
  }

  @Override
  public boolean isClientSide() {
    return this.getLevel().isClientSide();
  }

  public Player getTradingPlayer() {
    return this.tradingPlayer;
  }

  public void setTradingPlayer(@Nullable Player player) {
    this.tradingPlayer = player;
  }

  public boolean showProgressBar() {
    return true;
  }

  public int getVillagerXp() {
    return 0;
  }

  public void overrideOffers(@Nullable MerchantOffers merchantOffers) {
    /* Method is not used */
  }

  public void overrideXp(int experience) {
    /* Method is not used */
  }

  public void notifyTrade(MerchantOffer merchantOffer) {
    merchantOffer.increaseUses();
    this.ambientSoundTime = -this.getAmbientSoundInterval();
    this.rewardTradeXp(merchantOffer);
    if (this.tradingPlayer instanceof ServerPlayer serverPlayer) {
      log.debug("Trade {} with {} for {}", merchantOffer, serverPlayer, this);
    }
  }

  public void notifyTradeUpdated(ItemStack itemStack) {
    if (!this.isClientSide() && this.ambientSoundTime > -this.getAmbientSoundInterval() + 20) {
      this.ambientSoundTime = -this.getAmbientSoundInterval();
      this.playSound(
          this.getTradeUpdatedSound(!itemStack.isEmpty()),
          this.getSoundVolume(),
          this.getVoicePitch());
    }
  }

  protected SoundEvent getTradeUpdatedSound(boolean yesSound) {
    return yesSound ? SoundEvents.VILLAGER_YES : SoundEvents.VILLAGER_NO;
  }

  public SoundEvent getNotifyTradeSound() {
    return SoundEvents.VILLAGER_YES;
  }

  protected void rewardTradeXp(MerchantOffer merchantOffer) {
    if (merchantOffer.shouldRewardExp() && merchantOffer.getXp() > 0) {
      int tradeExperience = 3 + this.random.nextInt(merchantOffer.getXp());
      this.level.addFreshEntity(
          new ExperienceOrb(
              this.level, this.getX(), this.getY() + 0.5D, this.getZ(), tradeExperience));
    }
  }

  @Nullable
  public MerchantOffers getOffers() {
    if (this.offers == null) {
      this.updateTradesData();
    }
    return this.offers;
  }

  public int getNPCDataVersion() {
    return this.npcDataVersion;
  }

  public void setNPCDataVersion(int version) {
    this.npcDataVersion = version;
  }

  protected void updateTrades() {}

  @Override
  public void updateTradesData() {
    MerchantOffers merchantOffers = null;
    if (this.getTradingType() == TradingType.BASIC
        || this.getTradingType() == TradingType.ADVANCED) {
      // Create a copy of the offers to avoid side effects.
      merchantOffers = new MerchantOffers(this.getTradingOffers().createTag());
    }
    if (merchantOffers != null && !merchantOffers.isEmpty()) {
      // Filter out offers which are missing item a, item b or result item.
      merchantOffers.removeIf(
          merchantOffer ->
              (merchantOffer.getBaseCostA().isEmpty() && merchantOffer.getCostB().isEmpty())
                  || merchantOffer.getResult().isEmpty());
      this.offers = merchantOffers;
    }
    updateTrades();
  }

  @Override
  @Nullable
  public SpawnGroupData finalizeSpawn(
      @Nonnull ServerLevelAccessor serverLevelAccessor,
      @Nonnull DifficultyInstance difficulty,
      @Nonnull MobSpawnType mobSpawnType,
      @Nullable SpawnGroupData spawnGroupData,
      @Nullable CompoundTag compoundTag) {
    spawnGroupData =
        super.finalizeSpawn(
            serverLevelAccessor, difficulty, mobSpawnType, spawnGroupData, compoundTag);
    log.debug(
        "FinalizeSpawn for {} with spawnGroupData {} at {}",
        this,
        spawnGroupData,
        this.blockPosition());
    this.setHomePosition(this.blockPosition());
    return spawnGroupData;
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
    return projectileWeaponItem == Items.CROSSBOW || projectileWeaponItem == Items.BOW;
  }

  @Nullable
  @Override
  public AgeableMob getBreedOffspring(ServerLevel serverLevel, AgeableMob ageableMob) {
    return null;
  }

  @Override
  public int getRemainingPersistentAngerTime() {
    return this.remainingPersistentAngerTime;
  }

  @Override
  public void setRemainingPersistentAngerTime(int remainingPersistentAngerTime) {
    this.remainingPersistentAngerTime = remainingPersistentAngerTime;
  }

  @Nullable
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
  public AgeableMob getEasyNPCEntity() {
    return this;
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
  public <T> void setEasyNPCData(EntityDataAccessor<T> entityDataAccessor, T entityData) {
    this.entityData.set(entityDataAccessor, entityData);
  }

  @Override
  public <T> T getEasyNPCData(EntityDataAccessor<T> entityDataAccessor) {
    return this.entityData.get(entityDataAccessor);
  }

  @Override
  public <T> void defineEasyNPCData(EntityDataAccessor<T> entityDataAccessor, T entityData) {
    this.entityData.define(entityDataAccessor, entityData);
  }

  @Override
  public <T> void setEasyNPCCustomData(CustomDataAccessor<T> entityDataAccessor, T entityData) {
    this.customEntityData.set(entityDataAccessor, entityData);
  }

  @Override
  public <T> T getEasyNPCCustomData(CustomDataAccessor<T> entityDataAccessor) {
    return this.customEntityData.get(entityDataAccessor);
  }

  @Override
  public <T> void defineEasyNPCCustomData(CustomDataAccessor<T> entityDataAccessor, T entityData) {
    this.customEntityData.define(entityDataAccessor, entityData);
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
  public boolean isInvulnerable() {
    return getAttributeDataLoaded() ? !getAttributeIsAttackable() : super.isInvulnerable();
  }

  @Override
  public boolean isInvulnerableTo(DamageSource damageSource) {
    return isInvulnerable() || super.isInvulnerableTo(damageSource);
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
  @Nonnull
  public EntityDimensions getDimensions(@Nonnull Pose pose) {
    float scaleXZ = getScaleX() > getScaleZ() ? getScaleX() : getScaleZ();
    return super.getDimensions(pose).scale(scaleXZ, getScaleY());
  }

  @Override
  public void defineCustomData() {
    if (this.isServerSide()) {
      log.info("Define custom server-side data for {}", this);
      this.defineCustomActionData();
      this.defineCustomDialogData();
      this.defineCustomObjectiveData();
      this.defineCustomSpawnerData();
    }
  }

  @Override
  protected void defineSynchedData() {
    super.defineSynchedData();

    this.defineSynchedActionData();
    this.defineSynchedAttackData();
    this.defineSynchedAttributeData();
    this.defineSynchedDialogData();
    this.defineSynchedModelData();
    this.defineSynchedNavigationData();
    this.defineSynchedObjectiveData();
    this.defineSynchedOwnerData();
    this.defineSynchedProfessionData();
    this.defineSynchedScaleData();
    this.defineSynchedSkinData();
    this.defineSynchedTradingData();
    this.defineSynchedVariantData();
  }

  @Override
  public void addAdditionalSaveData(@Nonnull CompoundTag compoundTag) {
    super.addAdditionalSaveData(compoundTag);

    this.addAdditionalActionData(compoundTag);
    this.addAdditionalAttackData(compoundTag);
    this.addAdditionalAttributeData(compoundTag);
    this.addAdditionalDialogData(compoundTag);
    this.addAdditionalModelData(compoundTag);
    this.addAdditionalNavigationData(compoundTag);
    this.addAdditionalNPCData(compoundTag);
    this.addAdditionalObjectiveData(compoundTag);
    this.addAdditionalOwnerData(compoundTag);
    this.addAdditionalProfessionData(compoundTag);
    this.addAdditionalScaleData(compoundTag);
    this.addAdditionalSkinData(compoundTag);
    this.addAdditionalTradingData(compoundTag);
    this.addAdditionalVariantData(compoundTag);
    this.addAdditionalSpawnerData(compoundTag);

    this.addPersistentAngerSaveData(compoundTag);
  }

  @Override
  public void readAdditionalSaveData(@Nonnull CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);

    this.readAdditionalNPCData(compoundTag);

    this.readAdditionalActionData(compoundTag);
    this.readAdditionalAttackData(compoundTag);
    this.readAdditionalAttributeData(compoundTag);
    this.readAdditionalDialogData(compoundTag);
    this.readAdditionalModelData(compoundTag);
    this.readAdditionalNavigationData(compoundTag);
    this.readAdditionalObjectiveData(compoundTag);
    this.readAdditionalOwnerData(compoundTag);
    this.readAdditionalProfessionData(compoundTag);
    this.readAdditionalScaleData(compoundTag);
    this.readAdditionalSkinData(compoundTag);
    this.readAdditionalTradingData(compoundTag);
    this.readAdditionalVariantData(compoundTag);
    this.readAdditionalSpawnerData(compoundTag);

    this.readPersistentAngerSaveData(this.level, compoundTag);

    // Register attribute based objectives
    this.registerAttributeBasedObjectives();
  }
}
