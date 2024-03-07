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
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.data.EntityObjectiveData;
import de.markusbordihn.easynpc.entity.data.EntityTradingData;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.List;
import java.util.UUID;
import javax.annotation.Nullable;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.util.TimeUtil;
import net.minecraft.util.valueproviders.UniformInt;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.AgeableMob;
import net.minecraft.world.entity.EntitySelector;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.ai.navigation.GroundPathNavigation;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.monster.RangedAttackMob;
import net.minecraft.world.entity.npc.InventoryCarrier;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.item.BowItem;
import net.minecraft.world.item.CrossbowItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.ProjectileWeaponItem;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCEntityData extends EasyNPCBaseEntity
    implements EntityObjectiveData,
    EntityTradingData,
    RangedAttackMob,
    CrossbowAttackMob,
    Merchant,
    InventoryCarrier {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  // Synced Data
  private static final EntityDataAccessor<Boolean> DATA_TAME =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  private static final String DATA_INVENTORY_TAG = "Inventory";
  private static final String DATA_POSE_TAG = "Pose";
  private static final String DATA_TAME_TAG = "Tame";
  private static final String DATA_EASY_NPC_DATA_VERSION_TAG = "EasyNPCVersion";
  private static final int NPC_DATA_VERSION = 1;
  private static final UniformInt PERSISTENT_ANGER_TIME = TimeUtil.rangeOfSeconds(20, 39);
  private final CustomEntityData customEntityDataOld = new CustomEntityData(this);
  private final SimpleContainer inventory = new SimpleContainer(8);
  protected MerchantOffers offers;
  protected int attackAnimationTick;
  private boolean syncedDataLoaded = false;
  private boolean isPreview = false;
  private Player tradingPlayer;
  private int remainingPersistentAngerTime;
  private UUID persistentAngerTarget;
  private int npcDataVersion = -1;

  public EasyNPCEntityData(EntityType<? extends EasyNPCEntity> entityType, Level level) {
    super(entityType, level);
    this.defineCustomDataOld();
  }

  public Pose getPose(String pose) {
    return Pose.valueOf(pose);
  }

  public boolean isTame() {
    return this.entityData.get(DATA_TAME);
  }

  public void setTame(boolean tamed) {
    this.entityData.set(DATA_TAME, tamed);
  }

  @Nullable
  public MerchantOffers getOffers() {
    if (this.offers == null) {
      this.updateTradesData();
    }
    return this.offers;
  }

  public Player getTradingPlayer() {
    return this.tradingPlayer;
  }

  // Trading related methods
  public void setTradingPlayer(@Nullable Player player) {
    this.tradingPlayer = player;
  }

  public boolean showProgressBar() {
    return true;
  }

  public int getVillagerXp() {
    return 0;
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
    updateTrades();
  }

  protected void updateTrades() {
  }

  public void overrideOffers(@Nullable MerchantOffers merchantOffers) {
  }

  public void overrideXp(int experience) {
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

  public int getAttackAnimationTick() {
    return this.attackAnimationTick;
  }

  // Experience related methods
  protected void rewardTradeXp(MerchantOffer merchantOffer) {
    if (merchantOffer.shouldRewardExp() && merchantOffer.getXp() > 0) {
      int tradeExperience = 3 + this.random.nextInt(merchantOffer.getXp());
      this.level.addFreshEntity(
          new ExperienceOrb(
              this.level, this.getX(), this.getY() + 0.5D, this.getZ(), tradeExperience));
    }
  }

  public boolean isClientSide() {
    return this.level.isClientSide;
  }

  public boolean isPreview() {
    return this.isPreview;
  }

  public void setPreview(boolean isPreview) {
    this.isPreview = isPreview;
  }

  public CompoundTag exportPreset() {
    return this.serializeNBT();
  }

  public void importPreset(CompoundTag compoundTag) {
    // Reset action data and pose to default, to avoid side effects.
    this.setPose(Pose.STANDING);
    this.setModelPose(ModelPose.DEFAULT);
    this.clearActionEventSet();
    this.clearDialogDataSet();
    this.clearObjectiveDataSet();

    // If preset contains id and pos then we can import it directly, otherwise we
    // need to merge it with existing data.
    if (!compoundTag.contains("UUID") && !compoundTag.contains("Pos")) {
      CompoundTag existingCompoundTag = this.serializeNBT();

      // Remove existing dialog data to allow legacy presets to be imported.
      if (existingCompoundTag.contains(DialogData.DATA_DIALOG_DATA_TAG)) {
        existingCompoundTag.remove(DialogData.DATA_DIALOG_DATA_TAG);
      }

      // Remove existing model data to allow legacy presets to be imported.
      if (existingCompoundTag.contains(ModelData.EASY_NPC_DATA_MODEL_DATA_TAG)) {
        existingCompoundTag.remove(ModelData.EASY_NPC_DATA_MODEL_DATA_TAG);
      }

      // Remove existing skin data to allow legacy presets to be imported.
      if (existingCompoundTag.contains(SkinData.EASY_NPC_DATA_SKIN_DATA_TAG)) {
        existingCompoundTag.remove(SkinData.EASY_NPC_DATA_SKIN_DATA_TAG);
      }

      // Remove existing action data to allow legacy presets to be imported.
      if (existingCompoundTag.contains(ActionEventData.DATA_ACTION_DATA_TAG)) {
        existingCompoundTag.remove(ActionEventData.DATA_ACTION_DATA_TAG);
      }

      log.debug(
          "Merging preset {} with existing data {} for {}", compoundTag, existingCompoundTag, this);
      compoundTag = existingCompoundTag.merge(compoundTag);
    } else {
      log.debug("Importing full preset {} for {}", compoundTag, this);
    }

    // Remove motion tag to avoid side effects.
    if (compoundTag.contains("Motion")) {
      compoundTag.remove("Motion");
    }

    this.deserializeNBT(compoundTag);
  }

  public List<? extends Player> getPlayersInRange(Double range) {
    return this.level.players().stream()
        .filter(EntitySelector.NO_SPECTATORS)
        .filter(entity -> this.closerThan(entity, range))
        .toList();
  }

  protected void defineCustomDataOld() {
    this.defineCustomObjectiveData();
  }

  public boolean synchedDataLoaded() {
    return this.syncedDataLoaded;
  }

  public int getNPCDataVersion() {
    return this.npcDataVersion;
  }

  @Override
  public Component getName() {
    Component component = this.getCustomName();
    return component != null ? TextUtils.removeAction(component) : this.getTypeName();
  }

  @Override
  public EasyNPCEntity getEntity() {
    return (EasyNPCEntity) this;
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
  public Level getEntityLevel() {
    return this.level;
  }

  @Override
  public ServerLevel getEntityServerLevel() {
    if (this.level instanceof ServerLevel serverLevel) {
      return serverLevel;
    }
    return null;
  }

  @Override
  public GroundPathNavigation getEntityGroundPathNavigation() {
    if (this.getNavigation() instanceof GroundPathNavigation groundPathNavigation) {
      return groundPathNavigation;
    }
    return null;
  }

  @Override
  public <T> void setEntityData(EntityDataAccessor<T> entityDataAccessor, T entityData) {
    this.entityData.set(entityDataAccessor, entityData);
  }

  @Override
  public <T> T getEntityData(EntityDataAccessor<T> entityDataAccessor) {
    return this.entityData.get(entityDataAccessor);
  }

  @Override
  public <T> void defineEntityData(EntityDataAccessor<T> entityDataAccessor, T entityData) {
    this.entityData.define(entityDataAccessor, entityData);
  }

  @Override
  public <T> void setCustomEntityData(CustomDataAccessor<T> entityDataAccessor, T entityData) {
    this.customEntityDataOld.set(entityDataAccessor, entityData);
  }

  @Override
  public <T> T getCustomEntityData(CustomDataAccessor<T> entityDataAccessor) {
    return this.customEntityDataOld.get(entityDataAccessor);
  }

  @Override
  public <T> void defineCustomEntityData(CustomDataAccessor<T> entityDataAccessor, T entityData) {
    this.customEntityDataOld.define(entityDataAccessor, entityData);
  }

  @Override
  protected void defineSynchedData() {
    super.defineSynchedData();
    this.defineSynchedObjectiveData();
    this.defineSynchedTradingData();

    // Handle pose
    this.entityData.define(DATA_TAME, false);
  }

  @Override
  public void addAdditionalSaveData(CompoundTag compoundTag) {
    super.addAdditionalSaveData(compoundTag);

    // Add version tag.
    compoundTag.putInt(DATA_EASY_NPC_DATA_VERSION_TAG, NPC_DATA_VERSION);

    // Add additional data.
    this.addAdditionalObjectiveData(compoundTag);
    this.addAdditionalTradingData(compoundTag);

    // Handle pose
    if (this.getModelPose() == ModelPose.DEFAULT && this.getPose() != null) {
      compoundTag.putString(DATA_POSE_TAG, this.getPose().name());
    } else {
      compoundTag.putString(DATA_POSE_TAG, Pose.STANDING.name());
    }

    // Inventory
    if (!this.inventory.isEmpty()) {
      compoundTag.put(DATA_INVENTORY_TAG, this.inventory.createTag());
    }

    // Handle tame state.
    compoundTag.putBoolean(DATA_TAME_TAG, this.isTame());
  }

  @Override
  public void readAdditionalSaveData(CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);

    // Read version tag.
    if (compoundTag.contains(DATA_EASY_NPC_DATA_VERSION_TAG)) {
      this.npcDataVersion = compoundTag.getInt(DATA_EASY_NPC_DATA_VERSION_TAG);
      if (this.npcDataVersion > NPC_DATA_VERSION) {
        log.warn("Incompatible Easy NPC Data with version {} for {}!", this.npcDataVersion, this);
      } else if (this.npcDataVersion < NPC_DATA_VERSION) {
        log.warn(
            "Outdated Easy NPC Data with version {} for {}!Will try to convert data to new format.",
            this.npcDataVersion,
            this);
      }
    } else {
      log.warn("Legacy Easy NPC Data for {}! Will try to convert data to new format.", this);
    }

    // Read additional data.
    this.readAdditionalObjectiveData(compoundTag);
    this.readAdditionalTradingData(compoundTag);

    // Handle pose
    if (this.getModelPose() == ModelPose.DEFAULT && compoundTag.contains(DATA_POSE_TAG)) {
      String pose = compoundTag.getString(DATA_POSE_TAG);
      if (pose != null && !pose.isEmpty()) {
        this.setPose(this.getPose(pose));
      }
    }

    // Handle tame state.
    if (compoundTag.contains(DATA_TAME_TAG)) {
      this.setTame(compoundTag.getBoolean(DATA_TAME_TAG));
    }

    // Inventory
    if (compoundTag.contains(DATA_INVENTORY_TAG, 9)) {
      this.inventory.fromTag(compoundTag.getList(DATA_INVENTORY_TAG, 10));
    }

    // Register attribute based objectives
    this.registerAttributeBasedObjectives();

    // Set synced data loaded state.
    this.syncedDataLoaded = true;
  }

  @Override
  public AgeableMob getBreedOffspring(ServerLevel serverLevel, AgeableMob ageableMob) {
    return null;
  }

  public int getEntityGuiScaling() {
    return 45;
  }

  public int getEntityGuiTop() {
    return 0;
  }

  public int getEntityGuiLeft() {
    return 0;
  }

  public int getEntityDialogTop() {
    return 0;
  }

  public int getEntityDialogLeft() {
    return 0;
  }

  public int getEntityDialogScaling() {
    return 50;
  }

  public int getEntitySkinScaling() {
    return 30;
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
  public void setChargingCrossbow(boolean isCharging) {
    this.entityData.set(IS_CHARGING_CROSSBOW, isCharging);
  }

  @Override
  public void performRangedAttack(LivingEntity livingEntity, float damage) {
    if (this.getMainHandItem().getItem() instanceof BowItem) {
      this.performBowAttack(this, livingEntity, damage);
    } else if (this.getMainHandItem().getItem() instanceof CrossbowItem) {
      AttackData.addChargedProjectile(this.getMainHandItem(), new ItemStack(Items.ARROW, 1));
      this.performCrossbowAttack(this, 1.6F);
    }
  }

  @Override
  public void shootCrossbowProjectile(
      LivingEntity livingEntity, ItemStack itemStack, Projectile projectile, float rangeFactor) {
    this.shootCrossbowProjectile(this, livingEntity, projectile, rangeFactor, 1.6F);
  }

  @Override
  public void onCrossbowAttackPerformed() {
    this.noActionTime = 0;
  }

  @Override
  public boolean canFireProjectileWeapon(ProjectileWeaponItem projectileWeaponItem) {
    return projectileWeaponItem == Items.CROSSBOW || projectileWeaponItem == Items.BOW;
  }

  @Override
  public SimpleContainer getInventory() {
    return this.inventory;
  }

  protected ItemStack addToInventory(ItemStack itemStack) {
    return this.inventory.addItem(itemStack);
  }

  protected boolean canAddToInventory(ItemStack itemStack) {
    return this.inventory.canAddItem(itemStack);
  }
}
