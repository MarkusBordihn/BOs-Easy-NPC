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

import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.DoubleTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public record PresetData(
    String name,
    EntityType<?> entityType,
    CompoundTag data,
    ResourceLocation location,
    PresetType presetType,
    PresetMetadata metadata) {

  public static final String ID = "preset_data";
  public static final String EMPTY_NAME = "Empty";
  public static final PresetData EMPTY =
      new PresetData(
          EMPTY_NAME,
          EntityType.ARMOR_STAND,
          new CompoundTag(),
          null,
          null,
          PresetMetadata.createDefault());

  public PresetData(EntityType<?> entityType, CompoundTag data) {
    this(
        entityType.getDescriptionId(),
        entityType,
        data,
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

  public static PresetData fromCompoundTag(
      ResourceLocation location, PresetType presetType, CompoundTag compoundTag) {
    if (compoundTag == null) {
      return null;
    }

    CompoundTag entityData =
        compoundTag.contains("data") && !compoundTag.contains(Entity.ID_TAG)
            ? compoundTag.getCompound("data")
            : compoundTag;
    if (!entityData.contains(Entity.ID_TAG)) {
      return null;
    }

    EntityType<?> entityType =
        EntityType.byString(entityData.getString(Entity.ID_TAG)).orElse(null);
    if (entityType == null) {
      return null;
    }

    return new PresetData(
        location, presetType, PresetMetadata.fromPresetData(compoundTag), entityData, entityType);
  }

  public static PresetData fromNBT(
      ResourceLocation location,
      PresetType presetType,
      PresetMetadata metadata,
      CompoundTag compoundTag) {
    if (compoundTag == null || !compoundTag.contains(Entity.ID_TAG)) {
      return null;
    }

    String entityTypeId = compoundTag.getString(Entity.ID_TAG);
    EntityType<?> entityType = EntityType.byString(entityTypeId).orElse(null);

    if (entityType == null) {
      return null;
    }

    return new PresetData(location, presetType, metadata, compoundTag, entityType);
  }

  public static PresetData of(EntityType<?> entityType, CompoundTag compoundTag) {
    return new PresetData(entityType, compoundTag);
  }

  public static PresetData create(
      String name,
      EntityType<?> entityType,
      CompoundTag data,
      ResourceLocation location,
      PresetType presetType,
      PresetMetadata metadata) {
    return new PresetData(name, entityType, data, location, presetType, metadata);
  }

  public static CompoundTag cleanupEntityData(CompoundTag entityData) {
    return PresetDataUtils.cleanupEntityData(entityData, PresetDataUtils.CleanupMode.RUNTIME_ONLY);
  }

  public static CompoundTag cleanupEntityData(CompoundTag entityData, CleanupMode mode) {
    PresetDataUtils.CleanupMode utilMode =
        mode == CleanupMode.FULL
            ? PresetDataUtils.CleanupMode.FULL
            : PresetDataUtils.CleanupMode.RUNTIME_ONLY;
    return PresetDataUtils.cleanupEntityData(entityData, utilMode);
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
    updatedData.put("Pos", posTag);
    return new PresetData(name, entityType, updatedData, location, presetType, metadata);
  }

  public PresetData withUUID(UUID uuid) {
    if (uuid == null || data == null) {
      return this;
    }
    CompoundTag updatedData = data.copy();
    updatedData.putUUID(Entity.UUID_TAG, uuid);
    return new PresetData(name, entityType, updatedData, location, presetType, metadata);
  }

  public enum CleanupMode {
    RUNTIME_ONLY,
    FULL
  }
}
