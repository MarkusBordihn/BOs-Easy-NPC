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

import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import javax.annotation.Nullable;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.AgeableMob;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.npc.Npc;
import net.minecraft.world.level.Level;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.action.ActionDataHelper;
import de.markusbordihn.easynpc.action.ActionType;
import de.markusbordihn.easynpc.dialog.DialogType;
import de.markusbordihn.easynpc.skin.SkinModel;
import de.markusbordihn.easynpc.skin.SkinType;
import de.markusbordihn.easynpc.utils.TextUtils;

public class EasyNPCEntityData extends AgeableMob implements Npc {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Constants values
  private static final float DEFAULT_SCALE_X = 1.0f;
  private static final float DEFAULT_SCALE_Y = 1.0f;
  private static final float DEFAULT_SCALE_Z = 1.0f;

  // Default values
  private ResourceLocation baseTextureLocation;
  private ResourceLocation professionTextureLocation;
  private ResourceLocation textureLocation;
  private boolean hasBaseTextureLocation = false;
  private boolean hasProfessionTextureLocation = false;
  private boolean hasTextureLocation = false;

  // Default Variants
  private enum Variant {
    STEVE
  }

  // Cache
  private int actionPermissionLevel = 0;

  // Synced Data
  private static final EntityDataAccessor<CompoundTag> DATA_ACTION_DATA =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.COMPOUND_TAG);
  private static final EntityDataAccessor<Boolean> DATA_ACTION_DEBUG =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  private static final EntityDataAccessor<Optional<UUID>> DATA_OWNER_UUID_ID =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.OPTIONAL_UUID);
  private static final EntityDataAccessor<String> DATA_DIALOG =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<DialogType> DATA_DIALOG_TYPE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, DataSerializers.DIALOG_TYPE);
  private static final EntityDataAccessor<ModelPose> DATA_MODEL_POSE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, DataSerializers.MODEL_POSE);
  private static final EntityDataAccessor<String> DATA_NO_DIALOG =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_NO_DIALOG_BUTTON =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<Profession> DATA_PROFESSION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, DataSerializers.PROFESSION);
  private static final EntityDataAccessor<Float> DATA_SCALE_X =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.FLOAT);
  private static final EntityDataAccessor<Float> DATA_SCALE_Y =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.FLOAT);
  private static final EntityDataAccessor<Float> DATA_SCALE_Z =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.FLOAT);
  private static final EntityDataAccessor<String> DATA_SKIN =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_SKIN_URL =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<Optional<UUID>> DATA_SKIN_UUID =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.OPTIONAL_UUID);
  private static final EntityDataAccessor<SkinType> DATA_SKIN_TYPE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, DataSerializers.SKIN_TYPE);
  private static final EntityDataAccessor<String> DATA_VARIANT =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_YES_DIALOG =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_YES_DIALOG_BUTTON =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);

  // Stored Entity Data Tags
  private static final String DATA_ACTION_DATA_TAG = "ActionData";
  private static final String DATA_ACTION_DEBUG_TAG = "ActionDebug";
  private static final String DATA_ACTION_PERMISSION_LEVEL_TAG = "ActionPermissionLevel";
  private static final String DATA_DIALOG_TAG = "Dialog";
  private static final String DATA_DIALOG_TYPE_TAG = "DialogType";
  private static final String DATA_MODEL_POSE_TAG = "ModelPose";
  private static final String DATA_NO_DIALOG_BUTTON_TAG = "NoDialogButton";
  private static final String DATA_NO_DIALOG_TAG = "NoDialog";
  private static final String DATA_OWNER_TAG = "Owner";
  private static final String DATA_POSE_TAG = "Pose";
  private static final String DATA_PROFESSION_TAG = "Profession";
  private static final String DATA_SCALE_X_TAG = "ScaleX";
  private static final String DATA_SCALE_Y_TAG = "ScaleY";
  private static final String DATA_SCALE_Z_TAG = "ScaleZ";
  private static final String DATA_SKIN_TAG = "Skin";
  private static final String DATA_SKIN_TYPE_TAG = "SkinType";
  private static final String DATA_SKIN_URL_TAG = "SkinURL";
  private static final String DATA_SKIN_UUID_TAG = "SkinUUID";
  private static final String DATA_VARIANT_TAG = "Variant";
  private static final String DATA_YES_DIALOG_BUTTON_TAG = "YesDialogButton";
  private static final String DATA_YES_DIALOG_TAG = "YesDialog";

  public EasyNPCEntityData(EntityType<? extends EasyNPCEntity> entityType, Level level) {
    super(entityType, level);
  }

  public boolean hasBaseTextureLocation() {
    return this.hasBaseTextureLocation;
  }

  public ResourceLocation getBaseTextureLocation() {
    return this.baseTextureLocation;
  }

  public void setBaseTextureLocation(ResourceLocation textureLocation) {
    this.baseTextureLocation = textureLocation;
    this.hasBaseTextureLocation = this.baseTextureLocation != null;
  }

  public boolean hasTextureLocation() {
    return this.hasTextureLocation;
  }

  public ResourceLocation getTextureLocation() {
    return this.textureLocation;
  }

  public void setTextureLocation(ResourceLocation textureLocation) {
    this.textureLocation = textureLocation;
    this.hasTextureLocation = textureLocation != null;
  }

  public boolean hasProfessionTextureLocation() {
    return this.hasProfessionTextureLocation;
  }

  public ResourceLocation getProfessionTextureLocation() {
    return this.professionTextureLocation;
  }

  public void setProfessionTextureLocation(ResourceLocation textureLocation) {
    this.professionTextureLocation = textureLocation;
    this.hasProfessionTextureLocation = textureLocation != null;
  }

  public void setAction(ActionType actionType, String action) {
    CompoundTag compoundTag =
        ActionDataHelper.setAction(this.entityData.get(DATA_ACTION_DATA), actionType, action);
    this.entityData.set(DATA_ACTION_DATA, compoundTag);
  }

  public String getAction(ActionType actionType) {
    return ActionDataHelper.getAction(this.entityData.get(DATA_ACTION_DATA), actionType);
  }

  public boolean hasAction(ActionType actionType) {
    return ActionDataHelper.hasAction(this.entityData.get(DATA_ACTION_DATA), actionType);
  }

  public Map<ActionType, String> getActions() {
    return ActionDataHelper.readActionData(this.entityData.get(DATA_ACTION_DATA));
  }

  public CompoundTag getActionData() {
    return this.entityData.get(DATA_ACTION_DATA);
  }

  public void setActionData(CompoundTag compoundTag) {
    this.entityData.set(DATA_ACTION_DATA, compoundTag);
  }

  public boolean getActionDebug() {
    return this.entityData.get(DATA_ACTION_DEBUG);
  }

  public void setActionDebug(boolean enableDebug) {
    this.entityData.set(DATA_ACTION_DEBUG, enableDebug);
  }

  public int getActionPermissionLevel() {
    return this.actionPermissionLevel;
  }

  public void setActionPermissionLevel(int actionPermissionLevel) {
    this.actionPermissionLevel = actionPermissionLevel;
  }

  public DialogType getDialogType() {
    return this.entityData.get(DATA_DIALOG_TYPE);
  }

  public void setDialogType(DialogType dialogType) {
    this.entityData.set(DATA_DIALOG_TYPE, dialogType);
  }

  public ModelPose getModelPose() {
    return this.entityData.get(DATA_MODEL_POSE);
  }

  public void setModelPose(ModelPose modelPose) {
    this.entityData.set(DATA_MODEL_POSE, modelPose);
  }

  public boolean hasDialog() {
    return !this.entityData.get(DATA_DIALOG).isEmpty();
  }

  public String getDialog() {
    return this.entityData.get(DATA_DIALOG);
  }

  public void setDialog(String dialog) {
    this.entityData.set(DATA_DIALOG, dialog);
  }

  public String getNoDialog() {
    return this.entityData.get(DATA_NO_DIALOG);
  }

  public void setNoDialog(String dialog) {
    this.entityData.set(DATA_NO_DIALOG, dialog);
  }

  public String getNoDialogButton() {
    return this.entityData.get(DATA_NO_DIALOG_BUTTON);
  }

  public void setNoDialogButton(String dialogButton) {
    this.entityData.set(DATA_NO_DIALOG_BUTTON, dialogButton);
  }

  public String getYesDialog() {
    return this.entityData.get(DATA_YES_DIALOG);
  }

  public void setYesDialog(String dialog) {
    this.entityData.set(DATA_YES_DIALOG, dialog);
  }

  public String getYesDialogButton() {
    return this.entityData.get(DATA_YES_DIALOG_BUTTON);
  }

  public void setYesDialogButton(String dialogButton) {
    this.entityData.set(DATA_YES_DIALOG_BUTTON, dialogButton);
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

  public Float getDefaultScaleX() {
    return EasyNPCEntityData.DEFAULT_SCALE_X;
  }

  public Float getDefaultScaleY() {
    return EasyNPCEntityData.DEFAULT_SCALE_Y;
  }

  public Float getDefaultScaleZ() {
    return EasyNPCEntityData.DEFAULT_SCALE_Z;
  }

  public Float getScaleX() {
    return this.entityData.get(DATA_SCALE_X);
  }

  public void setScaleX(Float scale) {
    this.entityData.set(DATA_SCALE_X, scale);
  }

  public Float getScaleY() {
    return this.entityData.get(DATA_SCALE_Y);
  }

  public void setScaleY(Float scale) {
    this.entityData.set(DATA_SCALE_Y, scale);
  }

  public Float getScaleZ() {
    return this.entityData.get(DATA_SCALE_Z);
  }

  public void setScaleZ(Float scale) {
    this.entityData.set(DATA_SCALE_Z, scale);
  }

  public String getSkin() {
    return this.entityData.get(DATA_SKIN);
  }

  public void setSkin(String skin) {
    this.entityData.set(DATA_SKIN, skin != null ? skin : "");
  }

  public String getSkinURL() {
    return this.entityData.get(DATA_SKIN_URL);
  }

  public void setSkinURL(String skinURL) {
    this.entityData.set(DATA_SKIN_URL, skinURL != null ? skinURL : "");
  }

  public Optional<UUID> getSkinUUID() {
    return this.entityData.get(DATA_SKIN_UUID);
  }

  public void setSkinUUID(UUID uuid) {
    this.entityData.set(DATA_SKIN_UUID, Optional.of(uuid));
  }

  public void setSkinUUID(Optional<UUID> uuid) {
    this.entityData.set(DATA_SKIN_UUID, uuid);
  }

  public SkinType getSkinType() {
    return this.entityData.get(DATA_SKIN_TYPE);
  }

  public SkinType getSkinType(String name) {
    return SkinType.get(name);
  }

  public void setSkinType(SkinType skinType) {
    this.entityData.set(DATA_SKIN_TYPE, skinType);
  }

  public void setSkinType(String name) {
    SkinType skinType = getSkinType(name);
    if (skinType != null) {
      setSkinType(skinType);
    } else {
      log.error("Unknown skin type {} for {}", name, this);
    }
  }

  public SkinModel getSkinModel() {
    return SkinModel.HUMANOID;
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

  @Override
  public Component getName() {
    Component component = this.getCustomName();
    return component != null ? TextUtils.removeAction(component) : this.getTypeName();
  }

  @Nullable
  public UUID getOwnerUUID() {
    return this.entityData.get(DATA_OWNER_UUID_ID).orElse((UUID) null);
  }

  public void setOwnerUUID(@Nullable UUID uuid) {
    this.entityData.set(DATA_OWNER_UUID_ID, Optional.ofNullable(uuid));
  }

  public boolean hasOwner() {
    return this.getOwnerUUID() != null;
  }

  @Override
  protected void defineSynchedData() {
    super.defineSynchedData();
    this.entityData.define(DATA_ACTION_DATA, new CompoundTag());
    this.entityData.define(DATA_ACTION_DEBUG, false);
    this.entityData.define(DATA_DIALOG, "");
    this.entityData.define(DATA_DIALOG_TYPE, DialogType.NONE);
    this.entityData.define(DATA_MODEL_POSE, ModelPose.DEFAULT);
    this.entityData.define(DATA_NO_DIALOG, "");
    this.entityData.define(DATA_NO_DIALOG_BUTTON, "No");
    this.entityData.define(DATA_OWNER_UUID_ID, Optional.empty());
    this.entityData.define(DATA_PROFESSION, this.getDefaultProfession());
    this.entityData.define(DATA_SCALE_X, this.getDefaultScaleX());
    this.entityData.define(DATA_SCALE_Y, this.getDefaultScaleY());
    this.entityData.define(DATA_SCALE_Z, this.getDefaultScaleZ());
    this.entityData.define(DATA_SKIN, "");
    this.entityData.define(DATA_SKIN_URL, "");
    this.entityData.define(DATA_SKIN_UUID, Optional.empty());
    this.entityData.define(DATA_SKIN_TYPE, SkinType.DEFAULT);
    this.entityData.define(DATA_VARIANT, this.getDefaultVariant().name());
    this.entityData.define(DATA_YES_DIALOG, "");
    this.entityData.define(DATA_YES_DIALOG_BUTTON, "Yes");
  }

  @Override
  public void addAdditionalSaveData(CompoundTag compoundTag) {
    super.addAdditionalSaveData(compoundTag);
    if (ActionDataHelper.hasActionData(getActionData())) {
      compoundTag.put(DATA_ACTION_DATA_TAG, getActionData());
    }
    compoundTag.putInt(DATA_ACTION_PERMISSION_LEVEL_TAG, this.getActionPermissionLevel());
    compoundTag.putBoolean(DATA_ACTION_DEBUG_TAG, this.getActionDebug());
    if (this.getDialog() != null) {
      compoundTag.putString(DATA_DIALOG_TAG, this.getDialog());
    }
    if (this.getDialogType() != null) {
      compoundTag.putString(DATA_DIALOG_TYPE_TAG, this.getDialogType().name());
    }
    if (this.getModelPose() != null) {
      compoundTag.putString(DATA_MODEL_POSE_TAG, this.getModelPose().name());
    }
    if (this.getNoDialog() != null) {
      compoundTag.putString(DATA_NO_DIALOG_TAG, this.getNoDialog());
    }
    if (this.getNoDialogButton() != null) {
      compoundTag.putString(DATA_NO_DIALOG_BUTTON_TAG, this.getNoDialogButton());
    }
    if (this.getYesDialog() != null) {
      compoundTag.putString(DATA_YES_DIALOG_TAG, this.getYesDialog());
    }
    if (this.getYesDialogButton() != null) {
      compoundTag.putString(DATA_YES_DIALOG_BUTTON_TAG, this.getYesDialogButton());
    }
    if (this.getOwnerUUID() != null) {
      compoundTag.putUUID(DATA_OWNER_TAG, this.getOwnerUUID());
    }
    if (this.getPose() != null) {
      compoundTag.putString(DATA_POSE_TAG, this.getPose().name());
    }
    if (this.getProfession() != null) {
      compoundTag.putString(DATA_PROFESSION_TAG, this.getProfession().name());
    }
    if (this.getScaleX() != null && this.getScaleX() > 0.0f) {
      compoundTag.putFloat(DATA_SCALE_X_TAG, this.getScaleX());
    }
    if (this.getScaleY() != null && this.getScaleY() > 0.0f) {
      compoundTag.putFloat(DATA_SCALE_Y_TAG, this.getScaleY());
    }
    if (this.getScaleZ() != null && this.getScaleZ() > 0.0f) {
      compoundTag.putFloat(DATA_SCALE_Z_TAG, this.getScaleZ());
    }
    if (this.getSkin() != null) {
      compoundTag.putString(DATA_SKIN_TAG, this.getSkin());
    }
    if (this.getSkinURL() != null) {
      compoundTag.putString(DATA_SKIN_URL_TAG, this.getSkinURL());
    }
    Optional<UUID> skinUUID = this.getSkinUUID();
    if (skinUUID.isPresent()) {
      compoundTag.putUUID(DATA_SKIN_UUID_TAG, skinUUID.get());
    }
    if (this.getSkinType() != null) {
      compoundTag.putString(DATA_SKIN_TYPE_TAG, this.getSkinType().name());
    }
    if (this.getVariant() != null) {
      compoundTag.putString(DATA_VARIANT_TAG, this.getVariant().name());
    }
  }

  @Override
  public void readAdditionalSaveData(CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);
    if (compoundTag.contains(DATA_ACTION_DATA_TAG)) {
      this.setActionData(compoundTag.getCompound(DATA_ACTION_DATA_TAG));
    }
    if (compoundTag.contains(DATA_ACTION_DEBUG_TAG)) {
      this.setActionDebug(compoundTag.getBoolean(DATA_ACTION_DEBUG_TAG));
    }
    if (compoundTag.contains(DATA_ACTION_PERMISSION_LEVEL_TAG)) {
      this.setActionPermissionLevel(compoundTag.getInt(DATA_ACTION_PERMISSION_LEVEL_TAG));
    }
    if (compoundTag.contains(DATA_DIALOG_TAG)) {
      String dialog = compoundTag.getString(DATA_DIALOG_TAG);
      if (dialog != null) {
        this.setDialog(dialog);
      }
    }
    if (compoundTag.contains(DATA_DIALOG_TYPE_TAG)) {
      String dialogType = compoundTag.getString(DATA_DIALOG_TYPE_TAG);
      if (dialogType != null && !dialogType.isEmpty()) {
        this.setDialogType(DialogType.get(dialogType));
      }
    }
    if (compoundTag.contains(DATA_MODEL_POSE_TAG)) {
      String modelPose = compoundTag.getString(DATA_MODEL_POSE_TAG);
      if (modelPose != null && !modelPose.isEmpty()) {
        this.setModelPose(ModelPose.get(modelPose));
      }
    }
    if (compoundTag.contains(DATA_NO_DIALOG_TAG)) {
      String dialog = compoundTag.getString(DATA_NO_DIALOG_TAG);
      if (dialog != null) {
        this.setNoDialog(dialog);
      }
    }
    if (compoundTag.contains(DATA_NO_DIALOG_BUTTON_TAG)) {
      String dialogButton = compoundTag.getString(DATA_NO_DIALOG_BUTTON_TAG);
      if (dialogButton != null) {
        this.setNoDialogButton(dialogButton);
      }
    }
    if (compoundTag.contains(DATA_YES_DIALOG_TAG)) {
      String dialog = compoundTag.getString(DATA_YES_DIALOG_TAG);
      if (dialog != null) {
        this.setYesDialog(dialog);
      }
    }
    if (compoundTag.contains(DATA_YES_DIALOG_BUTTON_TAG)) {
      String dialogButton = compoundTag.getString(DATA_YES_DIALOG_BUTTON_TAG);
      if (dialogButton != null) {
        this.setYesDialogButton(dialogButton);
      }
    }
    if (compoundTag.hasUUID(DATA_OWNER_TAG)) {
      UUID uuid = compoundTag.getUUID(DATA_OWNER_TAG);
      if (uuid != null) {
        this.setOwnerUUID(uuid);
      }
    }
    if (compoundTag.contains(DATA_POSE_TAG)) {
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
    if (compoundTag.contains(DATA_SCALE_X_TAG)) {
      Float scale = compoundTag.getFloat(DATA_SCALE_X_TAG);
      if (scale != null && scale > 0.0f) {
        this.setScaleX(scale);
      }
    }
    if (compoundTag.contains(DATA_SCALE_Y_TAG)) {
      Float scale = compoundTag.getFloat(DATA_SCALE_Y_TAG);
      if (scale != null && scale > 0.0f) {
        this.setScaleY(scale);
      }
    }
    if (compoundTag.contains(DATA_SCALE_Z_TAG)) {
      Float scale = compoundTag.getFloat(DATA_SCALE_Z_TAG);
      if (scale != null && scale > 0.0f) {
        this.setScaleZ(scale);
      }
    }
    if (compoundTag.contains(DATA_SKIN_TAG)) {
      String skin = compoundTag.getString(DATA_SKIN_TAG);
      if (skin != null && !skin.isEmpty()) {
        this.setSkin(skin);
      }
    }
    if (compoundTag.contains(DATA_SKIN_URL_TAG)) {
      String url = compoundTag.getString(DATA_SKIN_URL_TAG);
      if (url != null && !url.isEmpty()) {
        this.setSkinURL(url);
      }
    }
    if (compoundTag.contains(DATA_SKIN_UUID_TAG)) {
      UUID skinUUID = compoundTag.getUUID(DATA_SKIN_UUID_TAG);
      if (skinUUID != null) {
        this.setSkinUUID(skinUUID);
      }
    }
    if (compoundTag.contains(DATA_SKIN_TYPE_TAG)) {
      String skinType = compoundTag.getString(DATA_SKIN_TYPE_TAG);
      if (skinType != null && !skinType.isEmpty()) {
        this.setSkinType(this.getSkinType(skinType));
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
