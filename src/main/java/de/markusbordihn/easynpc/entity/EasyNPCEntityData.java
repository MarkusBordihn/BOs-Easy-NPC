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
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.AgeableMob;
import net.minecraft.world.entity.EntitySelector;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.npc.Npc;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.data.EntityActionData;
import de.markusbordihn.easynpc.entity.data.EntityAttackData;
import de.markusbordihn.easynpc.entity.data.CustomDataSerializers;
import de.markusbordihn.easynpc.entity.data.EntityDialogData;
import de.markusbordihn.easynpc.entity.data.EntityModelData;
import de.markusbordihn.easynpc.entity.data.EntityOwnerData;
import de.markusbordihn.easynpc.entity.data.EntityScaleData;
import de.markusbordihn.easynpc.entity.data.EntitySkinData;
import de.markusbordihn.easynpc.utils.TextUtils;

public class EasyNPCEntityData extends AgeableMob
    implements Npc, EntityActionData, EntityAttackData, EntityDialogData, EntityModelData,
    EntityOwnerData, EntityScaleData, EntitySkinData {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Default Variants
  private enum Variant {
    STEVE
  }

  // Synced Data
  private static final EntityDataAccessor<Profession> DATA_PROFESSION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.PROFESSION);
  private static final EntityDataAccessor<String> DATA_VARIANT =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);

  // Stored Entity Data Tags
  private static final String DATA_POSE_TAG = "Pose";
  private static final String DATA_PROFESSION_TAG = "Profession";
  private static final String DATA_VARIANT_TAG = "Variant";

  // Cache
  private boolean isPreview = false;

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
    return profession != null ? TextUtils.normalizeName(profession.name()) : new TextComponent("");
  }

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

      // Remove existing model data to allow legacy presets to be imported.
      if (existingCompoundTag.contains(EntityModelData.DATA_MODEL_DATA_TAG)) {
        existingCompoundTag.remove(EntityModelData.DATA_MODEL_DATA_TAG);
      }
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
    this.defineSynchedDialogData();
    this.defineSynchedModelData();
    this.defineSynchedOwnerData();
    this.defineSynchedScaleData();
    this.defineSynchedSkinData();

    // Handle pose, profession and variant.
    this.entityData.define(DATA_PROFESSION, this.getDefaultProfession());
    this.entityData.define(DATA_VARIANT, this.getDefaultVariant().name());
  }

  @Override
  public void addAdditionalSaveData(CompoundTag compoundTag) {
    super.addAdditionalSaveData(compoundTag);
    this.addAdditionalActionData(compoundTag);
    this.addAdditionalAttackData(compoundTag);
    this.addAdditionalDialogData(compoundTag);
    this.addAdditionalModelData(compoundTag);
    this.addAdditionalOwnerData(compoundTag);
    this.addAdditionalScaleData(compoundTag);
    this.addAdditionalSkinData(compoundTag);

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
  }

  @Override
  public void readAdditionalSaveData(CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);
    this.readAdditionalActionData(compoundTag);
    this.readAdditionalAttackData(compoundTag);
    this.readAdditionalDialogData(compoundTag);
    this.readAdditionalModelData(compoundTag);
    this.readAdditionalOwnerData(compoundTag);
    this.readAdditionalScaleData(compoundTag);
    this.readAdditionalSkinData(compoundTag);

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
  public int getEntityDialogTop() {
    return 0;
  }

  @SuppressWarnings("java:S3400")
  public int getEntityDialogScaling() {
    return 50;
  }

}
