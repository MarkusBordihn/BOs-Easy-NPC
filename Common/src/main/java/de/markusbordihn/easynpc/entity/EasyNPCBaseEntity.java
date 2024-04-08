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
import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationData;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.data.GuiData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionData;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundData;
import de.markusbordihn.easynpc.entity.easynpc.data.SpawnData;
import de.markusbordihn.easynpc.entity.easynpc.data.SpawnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.BaseTickHandler;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.EnumMap;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.resources.ResourceLocation;
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
import net.minecraft.world.entity.EntityDimensions;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.NeutralMob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.Saddleable;
import net.minecraft.world.entity.SpawnGroupData;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.monster.Enemy;
import net.minecraft.world.entity.monster.RangedAttackMob;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.ProjectileWeaponItem;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.ServerLevelAccessor;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCBaseEntity<E extends PathfinderMob> extends PathfinderMob
    implements NeutralMob,
        Merchant,
        RangedAttackMob,
        CrossbowAttackMob,
        Saddleable,
        EasyNPC<E>,
        ActionHandler<E>,
        ActionEventData<E>,
        AttackData<E>,
        AttributeData<E>,
        BaseTickHandler<E>,
        ConfigurationData<E>,
        DialogData<E>,
        GuiData<E>,
        ModelData<E>,
        NavigationData<E>,
        ConfigData<E>,
        ObjectiveData<E>,
        OwnerData<E>,
        PresetData<E>,
        ProfessionData<E>,
        ScaleData<E>,
        SkinData<E>,
        SoundData<E>,
        SpawnData<E>,
        SpawnerData<E>,
        TickerData<E>,
        TradingData<E>,
        VariantData<E> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final UniformInt PERSISTENT_ANGER_TIME = TimeUtil.rangeOfSeconds(20, 39);

  static {
    // Register custom data serializers
    ActionEventData.registerActionEventDataSerializer();
    DialogData.registerDialogDataSerializer();
    ModelData.registerModelDataSerializer();
    ObjectiveData.registerObjectiveDataSerializer();
    ProfessionData.registerProfessionDataSerializer();
    SoundData.registerSoundDataSerializer();
    SkinData.registerSkinDataSerializer();
    SpawnerData.registerSpawnerDataSerializer();
    TradingData.registerTradingDataSerializer();
  }

  private final CustomEntityData customEntityData = new CustomEntityData(this);
  private final EnumMap<TickerType, Integer> tickerMap = new EnumMap<>(TickerType.class);
  protected MerchantOffers offers;
  private int remainingPersistentAngerTime;
  private UUID persistentAngerTarget;
  private int npcDataVersion = -1;
  private Player tradingPlayer;
  private int attackAnimationTick;

  public EasyNPCBaseEntity(
      EntityType<? extends PathfinderMob> entityType, Level level, Enum<?> variant) {
    super(entityType, level);
    this.registerEasyNPCDefaultHandler(variant);
    this.setInvulnerable(true);
    this.refreshGroundNavigation();
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
  public void performRangedAttack(LivingEntity livingEntity, float damage) {
    AttackHandler.performDefaultRangedAttack(this, livingEntity, damage);
  }

  @Override
  public void onCrossbowAttackPerformed() {
    this.noActionTime = 0;
  }

  @Override
  public void shootCrossbowProjectile(
      LivingEntity livingEntity, ItemStack itemStack, Projectile projectile, float rangeFactor) {
    this.shootCrossbowProjectile(this, livingEntity, projectile, rangeFactor, 1.6F);
  }

  @Override
  public void setChargingCrossbow(boolean isCharging) {
    this.entityData.set(IS_CHARGING_CROSSBOW, isCharging);
  }

  @Override
  public int getAttackAnimationTick() {
    return this.attackAnimationTick;
  }

  @Override
  public boolean doHurtTarget(Entity entity) {
    this.attackAnimationTick = 10;
    this.level().broadcastEntityEvent(this, (byte) 4);
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
      this.updatePersistentAnger((ServerLevel) this.level(), true);
    }
  }

  @Override
  public boolean isClientSide() {
    return this.getLevel().isClientSide();
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
  public boolean showProgressBar() {
    return true;
  }

  @Override
  public int getVillagerXp() {
    return 0;
  }

  @Override
  public void overrideOffers(MerchantOffers merchantOffers) {
    /* Method is not used */
  }

  @Override
  public void overrideXp(int experience) {
    /* Method is not used */
  }

  @Override
  public void notifyTrade(MerchantOffer merchantOffer) {
    merchantOffer.increaseUses();
    this.ambientSoundTime = -this.getAmbientSoundInterval();
    this.rewardTradeXp(merchantOffer);
    if (this.tradingPlayer instanceof ServerPlayer serverPlayer) {
      log.debug("Trade {} with {} for {}", merchantOffer, serverPlayer, this);
    }
  }

  @Override
  public void notifyTradeUpdated(ItemStack itemStack) {
    if (!this.isClientSide() && this.ambientSoundTime > -this.getAmbientSoundInterval() + 20) {
      this.ambientSoundTime = -this.getAmbientSoundInterval();
      this.playDefaultTradeUpdatedSound(!itemStack.isEmpty());
    }
  }

  @Override
  public SoundEvent getNotifyTradeSound() {
    return SoundEvents.VILLAGER_YES;
  }

  protected void rewardTradeXp(MerchantOffer merchantOffer) {
    if (merchantOffer.shouldRewardExp() && merchantOffer.getXp() > 0) {
      int tradeExperience = 3 + this.random.nextInt(merchantOffer.getXp());
      this.level()
          .addFreshEntity(
              new ExperienceOrb(
                  this.level(), this.getX(), this.getY() + 0.5D, this.getZ(), tradeExperience));
    }
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
    if (!(player instanceof ServerPlayer serverPlayer) || hand != InteractionHand.MAIN_HAND) {
      return InteractionResult.PASS;
    }

    // Item based actions.
    ItemStack handItemStack = player.getItemInHand(hand);
    if (!handItemStack.isEmpty()) {
      Item handItem = handItemStack.getItem();

      // Handle Easy NPC Wand
      Item easyNPCWand =
          BuiltInRegistries.ITEM
              .getOptional(new ResourceLocation(Constants.MOD_ID, "easy_npc_wand"))
              .orElse(null);
      if (handItem.equals(easyNPCWand)) {
        this.openMainConfigurationMenu(serverPlayer);
        return InteractionResult.PASS;
      }

      // Handle Armourer's Workshop items like the NPC wand.
      if (Constants.MOD_ARMOURERS_WORKSHOP_ID.equals(
          BuiltInRegistries.ITEM.getKey(handItem).getNamespace())) {
        if (this.getSkinModel().hasArmourersWorkshopSupport()) {
          log.debug("Ignore event for Armourer's Workshop Item for {} ...", this);
          return InteractionResult.PASS;
        } else {
          serverPlayer.sendSystemMessage(
              Component.translatable(
                  Constants.TEXT_PREFIX + "armourers_workshop.no_support",
                  this.getSkinModel().name(),
                  this));
        }
      }
    }

    // Open configuration menu for owner and creative mode if the player is crouching.
    if (player.isCreative() && player.isCrouching()) {
      this.openMainConfigurationMenu(serverPlayer);
      return InteractionResult.PASS;
    }

    this.handleActionInteractionEvent(serverPlayer);

    // Open dialog menu, if we have a simple dialog.
    if (this.hasDialog()) {
      this.openDialog(serverPlayer);
      return InteractionResult.CONSUME;
    }

    // Open trading screen, if we have a trading inventory.
    if (this.hasTrading()) {
      return this.openTradingScreen(serverPlayer);
    }

    return InteractionResult.PASS;
  }

  @Override
  public MerchantOffers getOffers() {
    if (this.offers == null) {
      this.updateTradesData();
    }
    return this.offers;
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
  }

  @Override
  public SpawnGroupData finalizeSpawn(
      ServerLevelAccessor serverLevelAccessor,
      DifficultyInstance difficulty,
      MobSpawnType mobSpawnType,
      SpawnGroupData spawnGroupData,
      CompoundTag compoundTag) {
    spawnGroupData =
        super.finalizeSpawn(
            serverLevelAccessor, difficulty, mobSpawnType, spawnGroupData, compoundTag);
    log.debug(
        "FinalizeSpawn for {} with spawnGroupData {} at {}",
        this,
        spawnGroupData,
        this.blockPosition());
    this.setHomePosition(this.blockPosition());
    if (!this.hasObjectives()) {
      this.registerStandardObjectives();
    }
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
  public E getEasyNPCEntity() {
    return (E) this;
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
  public boolean canBeLeashed(Player player) {
    if (!this.isLeashed()
        && player instanceof ServerPlayer serverPlayer
        && (serverPlayer.isCreative() || isOwner(serverPlayer))) {
      return true;
    }
    return !this.isLeashed()
        && getAttributeDataLoaded()
        && getAttributeCanBeLeashed()
        && !(this instanceof Enemy);
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
  public EntityDimensions getDimensions(Pose pose) {
    float scaleXZ = getScaleX() > getScaleZ() ? getScaleX() : getScaleZ();
    return super.getDimensions(pose).scale(scaleXZ, getScaleY());
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

    // Handle movement for NPC for specific conditions.
    if (this.hasTravelTargetObjectives()) {
      // Allow travel for NPC, if travel objectives are used.
      super.travel(vec3);
    } else {
      // Make sure we only calculate animations for be as much as possible server-friendly.
      this.calculateEntityAnimation(this instanceof FlyingAnimal);
    }
  }

  @Override
  public boolean isSaddleable() {
    return false;
  }

  @Override
  public void equipSaddle(SoundSource soundSource) {
    if (soundSource != null) {
      this.level().playSound(null, this, SoundEvents.PIG_SADDLE, soundSource, 0.5F, 1.0F);
    }
  }

  @Override
  public boolean isSaddled() {
    return false;
  }

  @Override
  protected void defineSynchedData() {
    super.defineSynchedData();
    this.defineEasyNPCSyncedData();
  }

  @Override
  public void addAdditionalSaveData(CompoundTag compoundTag) {
    super.addAdditionalSaveData(compoundTag);
    this.addPersistentAngerSaveData(compoundTag);
    this.addEasyNPCAdditionalSaveData(compoundTag);
  }

  @Override
  public void readAdditionalSaveData(CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);
    this.readPersistentAngerSaveData(this.level(), compoundTag);
    this.readEasyNPCAdditionalSaveData(compoundTag);
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
