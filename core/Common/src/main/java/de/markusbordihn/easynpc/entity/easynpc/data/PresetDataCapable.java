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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.data.attribute.LegacyAttributeConverter;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.resources.Identifier;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.level.storage.TagValueOutput;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface PresetDataCapable<T extends Mob> extends EasyNPC<T> {

  ServerDataAccessor<UUID> CUSTOM_DATA_PRESET_UUID =
      ServerEntityData.defineId(ServerDataIndex.PRESET_UUID, EntityDataSerializersManager.UUID);
  ServerDataAccessor<String> CUSTOM_DATA_CUSTOM_IDENTIFIER =
      ServerEntityData.defineId(ServerDataIndex.CUSTOM_IDENTIFIER, EntityDataSerializers.STRING);
  ServerDataAccessor<Boolean> CUSTOM_DATA_RESTORE_ON_OWNER_LOGIN =
      ServerEntityData.defineId(
          ServerDataIndex.RESTORE_ON_OWNER_LOGIN, EntityDataSerializers.BOOLEAN);
  String PRESET_UUID_TAG = "PresetUUID";
  String CUSTOM_IDENTIFIER_TAG = "CustomIdentifier";
  String RESTORE_ON_OWNER_LOGIN_TAG = "RestoreOnOwnerLogin";
  String PRESET_METADATA_TAG = "PresetMetadata";
  String ENTITY_UUID_TAG = "UUID";
  String ID_TAG = "id";

  List<String> ENTITY_DATA_VOLATILE_FIELDS =
      List.of(
          "AbsorptionAmount",
          "Air",
          "AngerTime",
          "DeathTime",
          "FallDistance",
          "FallFlying",
          "Fire",
          "forge:spawn_type",
          "Health",
          "HurtByTimestamp",
          "HurtTime",
          "Motion",
          "PortalCooldown");

  default void importPresetData(CompoundTag compoundTag) {

    if (compoundTag == null || compoundTag.isEmpty() || this.getEntity() == null) {
      return;
    }

    // Convert legacy (pre-1.21) attribute NBT before any merge, so the imported values are not
    // dropped in favor of the existing current-format attributes during the merge below.
    LegacyAttributeConverter.convertLegacyAttributes(compoundTag);

    // Reset specific data to avoid side effects
    if (this.getEntity() != null) {
      this.getEntity().setPose(Pose.STANDING);
    }
    if (this.getEasyNPCModelData() != null) {
      this.getEasyNPCModelData().setModelPose(ModelPose.VANILLA);
    }
    if (this.getEasyNPCActionEventData() != null
        && compoundTag.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG)) {
      this.getEasyNPCActionEventData().clearActionEventSet();
    }
    if (this.getEasyNPCDialogData() != null) {
      this.getEasyNPCDialogData().clearDialogDataSet();
    }
    if (this.getEasyNPCStateData() != null) {
      this.getEasyNPCStateData().clearStateDataSet();
    }
    if (this.getEasyNPCFactionData() != null) {
      // Remove scoreboard membership before the import may change the entity UUID.
      this.getEasyNPCFactionData().setFactionName("");
      this.getEasyNPCFactionData().applyFactionToScoreboard();
    }

    // Presets without id and pos have to be merged with the existing data instead of imported.
    if (!compoundTag.contains(ENTITY_UUID_TAG) || !compoundTag.contains("Pos")) {
      CompoundTag existingCompoundTag = this.serializePresetData();

      if (existingCompoundTag.contains(DialogDataCapable.DATA_DIALOG_DATA_TAG)) {
        existingCompoundTag.remove(DialogDataCapable.DATA_DIALOG_DATA_TAG);
      }

      if (existingCompoundTag.contains(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG)) {
        existingCompoundTag.remove(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG);
      }

      if (existingCompoundTag.contains(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG)) {
        existingCompoundTag.remove(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG);
      }

      if (existingCompoundTag.contains(RenderDataCapable.DATA_RENDER_DATA_TAG)) {
        existingCompoundTag.remove(RenderDataCapable.DATA_RENDER_DATA_TAG);
      }

      if (compoundTag.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG)) {
        existingCompoundTag.remove(ActionEventDataCapable.DATA_ACTION_DATA_TAG);
      }

      compoundTag = existingCompoundTag.merge(compoundTag);
    }

    for (String volatileField : ENTITY_DATA_VOLATILE_FIELDS) {
      compoundTag.remove(volatileField);
    }

    // Import preset data to entity.
    ValueInput valueInput =
        TagValueInput.create(
            ProblemReporter.DISCARDING, getEntityLevel().registryAccess(), compoundTag);
    this.getEntity().load(valueInput);

    // Fix possible legacy custom name.
    CompoundTagUtils.fixLegacyCustomName(this.getEntity(), compoundTag);

    if (this.getEntity() instanceof LivingEntity livingEntity) {
      float maxHealth =
          livingEntity.getAttribute(Attributes.MAX_HEALTH) != null
              ? (float) livingEntity.getAttribute(Attributes.MAX_HEALTH).getValue()
              : 20.0f;
      livingEntity.setHealth(maxHealth);
      livingEntity.setAbsorptionAmount(0.0f);
      livingEntity.deathTime = 0;
      livingEntity.hurtTime = 0;
    }
  }

  default CompoundTag serializePresetData() {
    if (this.getEntity() == null) {
      return new CompoundTag();
    }

    // Entity saved data
    TagValueOutput tagValueOutput =
        TagValueOutput.createWithContext(
            ProblemReporter.DISCARDING, getEntityLevel().registryAccess());
    this.getEntity().saveWithoutId(tagValueOutput);
    CompoundTag entityData = tagValueOutput.buildResult();

    // Add Entity type id to the preset data
    String entityTypeId = this.getEntityTypeId();
    if (entityTypeId != null) {
      entityData.putString(ID_TAG, entityTypeId);
    }

    // Add Preset UUID for unique identification (after saveWithoutId to prevent overwriting)
    if (!entityData.contains(PRESET_UUID_TAG)) {
      CompoundTagUtils.writeUUID(entityData, PRESET_UUID_TAG, UUID.randomUUID());
    }

    // Add Entity UUID for spawner tracking (single/boss spawner)
    CompoundTagUtils.writeUUID(entityData, ENTITY_UUID_TAG, this.getEntity().getUUID());

    for (String entityDataFieldName : ENTITY_DATA_VOLATILE_FIELDS) {
      entityData.remove(entityDataFieldName);
    }

    if (!entityData.contains(PRESET_METADATA_TAG)) {
      String presetName = PresetMetadata.DEFAULT_NAME;
      if (this.getEntity().hasCustomName() && this.getEntity().getCustomName() != null) {
        presetName = this.getEntity().getCustomName().getString();
      }
      String presetAuthor =
          this.getEasyNPCOwnerData() != null && this.getEasyNPCOwnerData().hasNPCOwner()
              ? this.getEasyNPCOwnerData().getNPCOwnerName()
              : PresetMetadata.DEFAULT_AUTHOR;
      PresetMetadata metadata = PresetMetadata.createDefault(presetName, presetAuthor);
      entityData.put(PRESET_METADATA_TAG, metadata.toCompoundTag());
    }

    return entityData;
  }

  default boolean hasPresetUUID() {
    return this.getPresetUUID() != null;
  }

  default UUID getPresetUUID() {
    return getEasyNPCServerData().getServerEntityData(CUSTOM_DATA_PRESET_UUID);
  }

  default void setPresetUUID(UUID uuid) {
    getEasyNPCServerData().setServerEntityData(CUSTOM_DATA_PRESET_UUID, uuid);
  }

  default Identifier getCustomIdentifier() {
    String customIdentifier =
        getEasyNPCServerData().getServerEntityData(CUSTOM_DATA_CUSTOM_IDENTIFIER);
    if (customIdentifier == null || customIdentifier.isEmpty()) {
      return null;
    }

    return Identifier.tryParse(customIdentifier);
  }

  default void setCustomIdentifier(Identifier customIdentifier) {
    getEasyNPCServerData()
        .setServerEntityData(
            CUSTOM_DATA_CUSTOM_IDENTIFIER,
            customIdentifier != null ? customIdentifier.toString() : "");
  }

  default boolean getRestoreOnOwnerLogin() {
    return Boolean.TRUE.equals(
        getEasyNPCServerData().getServerEntityData(CUSTOM_DATA_RESTORE_ON_OWNER_LOGIN));
  }

  default void setRestoreOnOwnerLogin(boolean restoreOnOwnerLogin) {
    getEasyNPCServerData()
        .setServerEntityData(CUSTOM_DATA_RESTORE_ON_OWNER_LOGIN, restoreOnOwnerLogin);
  }

  default void defineCustomPresetData() {
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_PRESET_UUID, null);
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_CUSTOM_IDENTIFIER, "");
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_RESTORE_ON_OWNER_LOGIN, false);
  }

  default void addAdditionalPresetData(ValueOutput valueOutput) {
    if (!this.isServerSideInstance()) {
      return;
    }

    if (this.getPresetUUID() != null) {
      valueOutput.store(PRESET_UUID_TAG, UUIDUtil.CODEC, this.getPresetUUID());
    }
    Identifier customIdentifier = this.getCustomIdentifier();
    if (customIdentifier != null) {
      valueOutput.putString(CUSTOM_IDENTIFIER_TAG, customIdentifier.toString());
    }
    if (this.getRestoreOnOwnerLogin()) {
      valueOutput.putBoolean(RESTORE_ON_OWNER_LOGIN_TAG, true);
    }
  }

  default void readAdditionalPresetData(ValueInput valueInput) {
    Optional<UUID> presetUUID = valueInput.read(PRESET_UUID_TAG, UUIDUtil.CODEC);
    presetUUID.ifPresent(this::setPresetUUID);

    Optional<String> customIdentifier = valueInput.getString(CUSTOM_IDENTIFIER_TAG);
    customIdentifier.ifPresent(
        identifier -> this.setCustomIdentifier(Identifier.tryParse(identifier)));

    this.setRestoreOnOwnerLogin(valueInput.getBooleanOr(RESTORE_ON_OWNER_LOGIN_TAG, false));
  }
}
