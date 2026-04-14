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

import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.List;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.ai.attributes.Attributes;

public interface PresetDataCapable<T extends Mob> extends EasyNPC<T> {

  ServerDataAccessor<UUID> CUSTOM_DATA_PRESET_UUID =
      ServerEntityData.defineId(ServerDataIndex.PRESET_UUID, EntityDataSerializersManager.UUID);
  String PRESET_UUID_TAG = "PresetUUID";
  String PRESET_METADATA_TAG = "PresetMetadata";
  String ENTITY_UUID_TAG = "UUID";

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

    // Skip import if no data is or no entity is available.
    if (compoundTag == null || compoundTag.isEmpty() || this.getEntity() == null) {
      return;
    }

    // Reset specific data to avoid side effects
    if (this.getEntity() != null) {
      this.getEntity().setPose(Pose.STANDING);
    }
    if (this.getEasyNPCModelData() != null) {
      this.getEasyNPCModelData().setModelPose(ModelPose.VANILLA);
    }
    if (this.getEasyNPCActionEventData() != null) {
      this.getEasyNPCActionEventData().clearActionEventSet();
    }
    if (this.getEasyNPCDialogData() != null) {
      this.getEasyNPCDialogData().clearDialogDataSet();
    }

    // If preset contains id and pos then we can import it directly, otherwise we
    // need to merge it with existing data.
    if (!compoundTag.contains(Entity.UUID_TAG) || !compoundTag.contains("Pos")) {
      CompoundTag existingCompoundTag = this.serializePresetData();

      // Remove existing dialog data.
      if (existingCompoundTag.contains(DialogDataCapable.DATA_DIALOG_DATA_TAG)) {
        existingCompoundTag.remove(DialogDataCapable.DATA_DIALOG_DATA_TAG);
      }

      // Remove existing model data.
      if (existingCompoundTag.contains(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG)) {
        existingCompoundTag.remove(ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG);
      }

      // Remove existing skin data.
      if (existingCompoundTag.contains(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG)) {
        existingCompoundTag.remove(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG);
      }

      // Remove existing render data.
      if (existingCompoundTag.contains(RenderDataCapable.DATA_RENDER_DATA_TAG)) {
        existingCompoundTag.remove(RenderDataCapable.DATA_RENDER_DATA_TAG);
      }

      // Remove existing action data.
      if (existingCompoundTag.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG)) {
        existingCompoundTag.remove(ActionEventDataCapable.DATA_ACTION_DATA_TAG);
      }

      compoundTag = existingCompoundTag.merge(compoundTag);
    }

    // Remove volatile fields that could cause issues (e.g. dead state)
    for (String volatileField : ENTITY_DATA_VOLATILE_FIELDS) {
      compoundTag.remove(volatileField);
    }

    // Import preset data to entity.
    this.getEntity().load(compoundTag);

    // Ensure entity is alive with full health after import
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
    CompoundTag compoundTag = new CompoundTag();
    if (this.getEntity() == null) {
      return compoundTag;
    }

    // Add Entity type id to the preset data.
    String entityTypeId = this.getEntityTypeId();
    if (entityTypeId != null) {
      compoundTag.putString(Entity.ID_TAG, entityTypeId);
    }

    // Entity saved data
    CompoundTag entityData = this.getEntity().saveWithoutId(compoundTag);

    // Add Preset UUID for unique identification (after saveWithoutId to prevent overwriting)
    if (!entityData.contains(PRESET_UUID_TAG)) {
      UUID presetUUID = this.hasPresetUUID() ? this.getPresetUUID() : UUID.randomUUID();
      entityData.putUUID(PRESET_UUID_TAG, presetUUID);
    }

    // Add Entity UUID for spawner tracking (single/boss spawner)
    entityData.putUUID(ENTITY_UUID_TAG, this.getEntity().getUUID());

    // Clean up and optimize entity data for smaller memory footprint
    for (String entityDataFieldName : ENTITY_DATA_VOLATILE_FIELDS) {
      entityData.remove(entityDataFieldName);
    }

    // Add preset metadata if not already present
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

  default void defineCustomPresetData() {
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_PRESET_UUID, null);
  }

  default void addAdditionalPresetData(CompoundTag compoundTag) {
    if (this.isServerSideInstance() && this.getPresetUUID() != null) {
      compoundTag.putUUID(PRESET_UUID_TAG, this.getPresetUUID());
    }
  }

  default void readAdditionalPresetData(CompoundTag compoundTag) {
    if (compoundTag.hasUUID(PRESET_UUID_TAG)) {
      this.setPresetUUID(compoundTag.getUUID(PRESET_UUID_TAG));
    }
  }
}
