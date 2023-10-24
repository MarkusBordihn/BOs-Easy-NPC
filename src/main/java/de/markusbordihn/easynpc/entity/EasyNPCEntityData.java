/**
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

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.Nullable;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.entity.AgeableMob;
import net.minecraft.world.entity.EntitySelector;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.npc.Npc;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import net.minecraft.world.level.Level;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.data.EntityActionData;
import de.markusbordihn.easynpc.entity.data.EntityAttackData;
import de.markusbordihn.easynpc.entity.data.CustomDataSerializers;
import de.markusbordihn.easynpc.entity.data.EntityDialogData;
import de.markusbordihn.easynpc.entity.data.EntityModelData;
import de.markusbordihn.easynpc.entity.data.EntityOwnerData;
import de.markusbordihn.easynpc.entity.data.EntityAttributeData;
import de.markusbordihn.easynpc.entity.data.EntityScaleData;
import de.markusbordihn.easynpc.entity.data.EntitySkinData;
import de.markusbordihn.easynpc.entity.data.EntityTradingData;
import de.markusbordihn.easynpc.utils.TextUtils;

public class EasyNPCEntityData extends AgeableMob implements EntityActionData, EntityAttackData,
    EntityDialogData, EntityModelData, EntityOwnerData, EntityAttributeData, EntityScaleData,
    EntitySkinData, EntityTradingData, Npc, Merchant {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Default Variants
  private enum Variant {
    STEVE
  }

  // Synced Data
  private static final EntityDataAccessor<Boolean> DATA_TAME =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  private static final EntityDataAccessor<Profession> DATA_PROFESSION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.PROFESSION);
  private static final EntityDataAccessor<String> DATA_VARIANT =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);

  // Stored Entity Data Tags
  private static final String DATA_POSE_TAG = "Pose";
  private static final String DATA_PROFESSION_TAG = "Profession";
  private static final String DATA_TAME_TAG = "Tame";
  private static final String DATA_VARIANT_TAG = "Variant";

  // Cache
  private boolean isPreview = false;
  private Player tradingPlayer;
  protected MerchantOffers offers;

  public EasyNPCEntityData(EntityType<? extends EasyNPCEntity> entityType, Level level) {
    super(entityType, level);
  }

  public Pose getPose(String pose) {
    return Pose.valueOf(pose);
  }

  public Profession getDefaultProfession() {
    return Profession.NONE;
  }

  public Profession getProfession() {
    return this.entityData.get(DATA_PROFESSION);
  }

  public Profession getProfession(String name) {
    return Profession.valueOf(name);
  }

  public void setProfession(Profession profession) {
    this.entityData.set(DATA_PROFESSION, profession);
  }

  public void setProfession(String name) {
    Profession profession = getProfession(name);
    if (profession != null) {
      setProfession(profession);
    } else {
      log.error("Unknown profession {} for {}", name, this);
    }
  }

  public boolean hasProfessions() {
    return false;
  }

  public Profession[] getProfessions() {
    return Profession.values();
  }

  public boolean hasProfession() {
    return false;
  }

  public Component getProfessionName() {
    Enum<?> profession = getProfession();
    return profession != null ? TextUtils.normalizeName(profession.name()) : Component.literal("");
  }

  public boolean isTame() {
    return this.entityData.get(DATA_TAME);
  }

  public void setTame(boolean tamed) {
    this.entityData.set(DATA_TAME, tamed);
  }

  // Trading related methods
  public void setTradingPlayer(@Nullable Player player) {
    this.tradingPlayer = player;
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
      merchantOffers.removeIf(merchantOffer -> {
        return (merchantOffer.getBaseCostA().isEmpty() && merchantOffer.getCostB().isEmpty())
            || merchantOffer.getResult().isEmpty();
      });
      this.offers = merchantOffers;
    }
    updateTrades();
  }

  protected void updateTrades() {}

  public void overrideOffers(@Nullable MerchantOffers merchantOffers) {}

  public void overrideXp(int experience) {}

  public void notifyTrade(MerchantOffer merchantOffer) {
    merchantOffer.increaseUses();
    this.ambientSoundTime = -this.getAmbientSoundInterval();
    this.rewardTradeXp(merchantOffer);
    if (this.tradingPlayer instanceof ServerPlayer serverPlayer) {
      log.debug("Trade {} with {} for {}", merchantOffer, serverPlayer, this);
    }
  }

  public void notifyTradeUpdated(ItemStack itemStack) {
    if (!this.level.isClientSide && this.ambientSoundTime > -this.getAmbientSoundInterval() + 20) {
      this.ambientSoundTime = -this.getAmbientSoundInterval();
      this.playSound(this.getTradeUpdatedSound(!itemStack.isEmpty()), this.getSoundVolume(),
          this.getVoicePitch());
    }
  }

  protected SoundEvent getTradeUpdatedSound(boolean yesSound) {
    return yesSound ? SoundEvents.VILLAGER_YES : SoundEvents.VILLAGER_NO;
  }

  public SoundEvent getNotifyTradeSound() {
    return SoundEvents.VILLAGER_YES;
  }

  // Experience related methods
  protected void rewardTradeXp(MerchantOffer merchantOffer) {
    if (merchantOffer.shouldRewardExp()) {
      int tradeExperience = 3 + this.random.nextInt(4);
      this.level.addFreshEntity(new ExperienceOrb(this.level, this.getX(), this.getY() + 0.5D,
          this.getZ(), tradeExperience));
    }
  }

  // Variant related methods
  public Enum<?> getDefaultVariant() {
    return Variant.STEVE;
  }

  public Enum<?> getVariant() {
    return getVariant(this.entityData.get(DATA_VARIANT));
  }

  public Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

  public void setVariant(Enum<?> variant) {
    this.entityData.set(DATA_VARIANT, variant != null ? variant.name() : "");
  }

  public void setVariant(String name) {
    Enum<?> variant = getVariant(name);
    if (variant != null) {
      setVariant(variant);
    } else {
      log.error("Unknown variant {} for {}", name, this);
    }
  }

  public Enum<?>[] getVariants() {
    return Variant.values();
  }

  public Component getVariantName() {
    Enum<?> variant = getVariant();
    return variant != null ? TextUtils.normalizeName(variant.name()) : this.getTypeName();
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
    this.clearActionData();

    // If preset contains id and pos then we can import it directly, otherwise we
    // need to merge it with existing data.
    if (!compoundTag.contains("UUID") && !compoundTag.contains("pos")) {
      CompoundTag existingCompoundTag = this.serializeNBT();

      // Remove existing dialog data to allow legacy presets to be imported.
      if (existingCompoundTag.contains(EntityDialogData.DATA_DIALOG_DATA_TAG)) {
        existingCompoundTag.remove(EntityDialogData.DATA_DIALOG_DATA_TAG);
      }

      // Remove existing model data to allow legacy presets to be imported.
      if (existingCompoundTag.contains(EntityModelData.DATA_MODEL_DATA_TAG)) {
        existingCompoundTag.remove(EntityModelData.DATA_MODEL_DATA_TAG);
      }

      // Remove existing skin data to allow legacy presets to be imported.
      if (existingCompoundTag.contains(EntitySkinData.DATA_SKIN_DATA_TAG)) {
        existingCompoundTag.remove(EntitySkinData.DATA_SKIN_DATA_TAG);
      }

      log.info("Merging preset {} with existing data for {}", compoundTag, this);
      compoundTag = existingCompoundTag.merge(compoundTag);
    }

    this.deserializeNBT(compoundTag);
  }

  public List<Player> getPlayersInRange(Double range) {
    return this.level.players().stream().filter(EntitySelector.NO_SPECTATORS).filter(entity -> {
      return this.closerThan(entity, range);
    }).sorted().collect(Collectors.toList());
  }

  @Override
  public Component getName() {
    Component component = this.getCustomName();
    return component != null ? TextUtils.removeAction(component) : this.getTypeName();
  }

  @Override
  public Level getEntityLevel() {
    return this.level;
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
  protected void defineSynchedData() {
    super.defineSynchedData();
    this.defineSynchedActionData();
    this.defineSynchedAttackData();
    this.defineSynchedAttributeData();
    this.defineSynchedDialogData();
    this.defineSynchedModelData();
    this.defineSynchedOwnerData();
    this.defineSynchedScaleData();
    this.defineSynchedSkinData();
    this.defineSynchedTradingData();

    // Handle pose, profession and variant.
    this.entityData.define(DATA_TAME, false);
    this.entityData.define(DATA_PROFESSION, this.getDefaultProfession());
    this.entityData.define(DATA_VARIANT, this.getDefaultVariant().name());
  }

  @Override
  public void addAdditionalSaveData(CompoundTag compoundTag) {
    super.addAdditionalSaveData(compoundTag);
    this.addAdditionalActionData(compoundTag);
    this.addAdditionalAttackData(compoundTag);
    this.addAdditionalAttributeData(compoundTag);
    this.addAdditionalDialogData(compoundTag);
    this.addAdditionalModelData(compoundTag);
    this.addAdditionalOwnerData(compoundTag);
    this.addAdditionalScaleData(compoundTag);
    this.addAdditionalSkinData(compoundTag);
    this.addAdditionalTradingData(compoundTag);

    // Handle pose, profession and variant.
    if (this.getModelPose() == ModelPose.DEFAULT && this.getPose() != null) {
      compoundTag.putString(DATA_POSE_TAG, this.getPose().name());
    } else {
      compoundTag.putString(DATA_POSE_TAG, Pose.STANDING.name());
    }
    if (this.getProfession() != null) {
      compoundTag.putString(DATA_PROFESSION_TAG, this.getProfession().name());
    }
    if (this.getVariant() != null) {
      compoundTag.putString(DATA_VARIANT_TAG, this.getVariant().name());
    }

    // Handle tame state.
    compoundTag.putBoolean(DATA_TAME_TAG, this.isTame());
  }

  @Override
  public void readAdditionalSaveData(CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);
    this.readAdditionalActionData(compoundTag);
    this.readAdditionalAttackData(compoundTag);
    this.readAdditionalAttributeData(compoundTag);
    this.readAdditionalDialogData(compoundTag);
    this.readAdditionalModelData(compoundTag);
    this.readAdditionalOwnerData(compoundTag);
    this.readAdditionalScaleData(compoundTag);
    this.readAdditionalSkinData(compoundTag);
    this.readAdditionalTradingData(compoundTag);

    // Handle pose, profession and variant data.
    if (this.getModelPose() == ModelPose.DEFAULT && compoundTag.contains(DATA_POSE_TAG)) {
      String pose = compoundTag.getString(DATA_POSE_TAG);
      if (pose != null && !pose.isEmpty()) {
        this.setPose(this.getPose(pose));
      }
    }

    if (compoundTag.contains(DATA_PROFESSION_TAG)) {
      String profession = compoundTag.getString(DATA_PROFESSION_TAG);
      if (profession != null && !profession.isEmpty()) {
        this.setProfession(this.getProfession(profession));
      }
    }
    if (compoundTag.contains(DATA_VARIANT_TAG)) {
      String variant = compoundTag.getString(DATA_VARIANT_TAG);
      if (variant != null && !variant.isEmpty()) {
        this.setVariant(this.getVariant(variant));
      }
    }

    // Handle tame state.
    if (compoundTag.contains(DATA_TAME_TAG)) {
      this.setTame(compoundTag.getBoolean(DATA_TAME_TAG));
    }
  }

  @Override
  public AgeableMob getBreedOffspring(ServerLevel serverLevel, AgeableMob ageableMob) {
    return null;
  }

  @SuppressWarnings("java:S3400")
  public int getEntityGuiScaling() {
    return 30;
  }

  @SuppressWarnings("java:S3400")
  public int getEntityGuiTop() {
    return 0;
  }

  @SuppressWarnings("java:S3400")
  public int getEntityGuiLeft() {
    return 0;
  }

  @SuppressWarnings("java:S3400")
  public int getEntityDialogTop() {
    return 0;
  }

  @SuppressWarnings("java:S3400")
  public int getEntityDialogLeft() {
    return 0;
  }

  @SuppressWarnings("java:S3400")
  public int getEntityDialogScaling() {
    return 50;
  }

}
