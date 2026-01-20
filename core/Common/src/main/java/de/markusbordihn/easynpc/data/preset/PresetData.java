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

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.component.DataComponents;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.DoubleTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
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
  public static final String ENTITY_TYPE_TAG = "EntityType";
  public static final String PRESET_TAG = "Preset";
  public static final String PRESET_UUID_TAG = "PresetUUID";
  public static final PresetData EMPTY =
      new PresetData(
          EMPTY_NAME,
          EntityType.ARMOR_STAND,
          new CompoundTag(),
          null,
          null,
          PresetMetadata.createDefault());
  public static final Codec<PresetData> CODEC =
      RecordCodecBuilder.create(
          instance ->
              instance
                  .group(
                      Codec.STRING.fieldOf("name").forGetter(PresetData::name),
                      BuiltInRegistries.ENTITY_TYPE
                          .byNameCodec()
                          .fieldOf("entityType")
                          .forGetter(PresetData::entityType),
                      CompoundTag.CODEC.fieldOf("data").forGetter(PresetData::data),
                      ResourceLocation.CODEC
                          .optionalFieldOf("location")
                          .forGetter(
                              presetData ->
                                  presetData.location() != null
                                      ? java.util.Optional.of(presetData.location())
                                      : java.util.Optional.empty()),
                      Codec.STRING
                          .xmap(PresetType::valueOf, PresetType::name)
                          .optionalFieldOf("presetType")
                          .forGetter(
                              presetData ->
                                  presetData.presetType() != null
                                      ? java.util.Optional.of(presetData.presetType())
                                      : java.util.Optional.empty()),
                      PresetMetadata.CODEC
                          .optionalFieldOf("metadata", PresetMetadata.createDefault())
                          .forGetter(PresetData::metadata))
                  .apply(
                      instance,
                      (name, entityType, data, location, presetType, metadata) ->
                          new PresetData(
                              name,
                              entityType,
                              data,
                              location.orElse(null),
                              presetType.orElse(null),
                              metadata)));
  public static final StreamCodec<RegistryFriendlyByteBuf, PresetData> STREAM_CODEC =
      StreamCodec.composite(
          ByteBufCodecs.STRING_UTF8,
          PresetData::name,
          ByteBufCodecs.registry(Registries.ENTITY_TYPE),
          PresetData::entityType,
          ByteBufCodecs.COMPOUND_TAG,
          PresetData::data,
          ByteBufCodecs.optional(ResourceLocation.STREAM_CODEC),
          presetData ->
              presetData.location() != null ? Optional.of(presetData.location()) : Optional.empty(),
          ByteBufCodecs.optional(
              ByteBufCodecs.STRING_UTF8.map(PresetType::valueOf, PresetType::name)),
          presetData ->
              presetData.presetType() != null
                  ? Optional.of(presetData.presetType())
                  : Optional.empty(),
          PresetMetadata.STREAM_CODEC,
          PresetData::metadata,
          (name, entityType, data, location, presetType, metadata) ->
              new PresetData(
                  name,
                  entityType,
                  data,
                  location.orElse(null),
                  presetType.orElse(null),
                  metadata));
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

    CompoundTag entityData =
        compoundTag.contains("data") && !compoundTag.contains(Entity.ID_TAG)
            ? compoundTag.getCompound("data")
            : compoundTag;
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

  public static boolean has(ItemStack itemStack) {
    return itemStack != null
        && !itemStack.isEmpty()
        && itemStack.has(DataComponents.PRESET_DATA)
        && !itemStack.getOrDefault(DataComponents.PRESET_DATA, PresetData.EMPTY).isEmpty();
  }

  public static PresetData get(ItemStack itemStack) {
    if (!has(itemStack)) {
      return null;
    }
    return itemStack.get(DataComponents.PRESET_DATA);
  }

  public static ItemStack set(Item item, PresetData presetData) {
    return set(new ItemStack(item), presetData);
  }

  public static ItemStack set(ItemStack itemStack, PresetData presetData) {
    if (itemStack == null || itemStack.isEmpty() || presetData == null) {
      return null;
    }
    itemStack.set(DataComponents.PRESET_DATA, presetData);
    return itemStack;
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
