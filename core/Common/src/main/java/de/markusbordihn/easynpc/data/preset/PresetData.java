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

package de.markusbordihn.easynpc.data.preset;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.DoubleTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record PresetData(
    String name,
    EntityType<?> entityType,
    CompoundTag data,
    ResourceLocation location,
    PresetType presetType,
    PresetMetadata metadata) {
  public static final String ID = "preset_data";
  public static final String EMPTY_NAME = "Empty";
  public static final String DATA_TAG = "data";
  public static final String ENTITY_TYPE_TAG = "EntityType";
  public static final String PARENT_TAG = "Parent";
  public static final String PRESET_TAG = "Preset";
  public static final String PRESET_UUID_TAG = "PresetUUID";
  public static final String POSITION_TAG = "Pos";
  public static final String ROTATION_TAG = "Rotation";
  public static final PresetData EMPTY =
      new PresetData(
          EMPTY_NAME,
          EntityType.ARMOR_STAND,
          new CompoundTag(),
          null,
          null,
          PresetMetadata.createDefault());
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public PresetData(EntityType<?> entityType, CompoundTag data) {
    this(
        entityType.getDescriptionId(),
        entityType,
        ensurePresetUUID(data),
        null,
        null,
        PresetMetadata.createDefault());
  }

  public PresetData(
      ResourceLocation location,
      PresetType presetType,
      PresetMetadata metadata,
      CompoundTag data,
      EntityType<?> entityType) {
    this(
        metadata != null && metadata.name() != null && !metadata.name().isEmpty()
            ? metadata.name()
            : (location != null ? location.getPath() : EMPTY_NAME),
        entityType,
        data,
        location,
        presetType,
        metadata);
  }

  public static boolean usesEntityDataWrapper(CompoundTag compoundTag) {
    return compoundTag != null
        && compoundTag.contains(DATA_TAG)
        && !compoundTag.contains(Entity.ID_TAG);
  }

  private static CompoundTag ensurePresetUUID(CompoundTag data) {
    if (data == null) {
      return data;
    }

    if (!data.hasUUID(PRESET_UUID_TAG)) {
      CompoundTag updated = data.copy();
      updated.putUUID(PRESET_UUID_TAG, UUID.randomUUID());
      return updated;
    }

    return data;
  }

  public static PresetData fromCompoundTag(
      ResourceLocation location, PresetType presetType, CompoundTag compoundTag) {
    if (compoundTag == null) {
      return null;
    }

    if (compoundTag.contains(PARENT_TAG)) {
      log.error(
          "Preset {} still references the parent preset {}, "
              + "it was loaded without resolving its parent presets",
          location,
          compoundTag.getString(PARENT_TAG));
      return null;
    }

    CompoundTag entityData =
        usesEntityDataWrapper(compoundTag) ? compoundTag.getCompound(DATA_TAG) : compoundTag;
    if (!entityData.contains(Entity.ID_TAG)) {
      log.error("Missing entity ID tag in preset data: {}", compoundTag);
      return null;
    }

    EntityType<?> entityType =
        EntityType.byString(entityData.getString(Entity.ID_TAG)).orElse(null);
    if (entityType == null) {
      log.error("Unknown entity type in preset data: {}", compoundTag);
      return null;
    }

    return new PresetData(
        location,
        presetType,
        PresetMetadata.fromPresetData(compoundTag),
        ensurePresetUUID(entityData),
        entityType);
  }

  public static PresetData create(
      String name,
      EntityType<?> entityType,
      CompoundTag data,
      ResourceLocation location,
      PresetType presetType,
      PresetMetadata metadata) {
    return new PresetData(name, entityType, ensurePresetUUID(data), location, presetType, metadata);
  }

  public boolean isEmpty() {
    return this.equals(EMPTY);
  }

  public boolean hasData() {
    return this.data != null && !this.data.isEmpty();
  }

  public boolean hasEntityType() {
    return this.entityType != null;
  }

  public boolean hasValidData() {
    return hasEntityType() && hasData();
  }

  public String getDisplayName() {
    if (location != null && metadata != null) {
      return CustomPresetDataFiles.getPresetDisplayName(location, metadata);
    }
    return name;
  }

  public PresetData withPosition(Vec3 position) {
    if (position == null || data == null) {
      return this;
    }
    CompoundTag updatedData = data.copy();
    ListTag posTag = new ListTag();
    posTag.add(DoubleTag.valueOf(position.x));
    posTag.add(DoubleTag.valueOf(position.y));
    posTag.add(DoubleTag.valueOf(position.z));
    updatedData.put(POSITION_TAG, posTag);
    return new PresetData(name, entityType, updatedData, location, presetType, metadata);
  }

  public PresetData withoutPosition() {
    if (this.data == null || !this.data.contains(POSITION_TAG)) {
      return this;
    }

    CompoundTag updatedData = this.data.copy();
    updatedData.remove(POSITION_TAG);
    updatedData.remove(ROTATION_TAG);
    return new PresetData(
        this.name, this.entityType, updatedData, this.location, this.presetType, this.metadata);
  }

  public PresetData withUUID(UUID uuid) {
    if (uuid == null || data == null) {
      return this;
    }
    CompoundTag updatedData = data.copy();
    updatedData.putUUID(Entity.UUID_TAG, uuid);
    return new PresetData(name, entityType, updatedData, location, presetType, metadata);
  }

  public UUID getPresetUUID() {
    if (data == null || !data.hasUUID(PRESET_UUID_TAG)) {
      return null;
    }
    return data.getUUID(PRESET_UUID_TAG);
  }

  public UUID getEntityUUID() {
    if (data == null || !data.hasUUID(Entity.UUID_TAG)) {
      return null;
    }
    return data.getUUID(Entity.UUID_TAG);
  }
}
