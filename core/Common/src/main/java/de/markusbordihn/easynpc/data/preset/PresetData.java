/*
 * Copyright 2025 Markus Bordihn
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
import de.markusbordihn.easynpc.component.DataComponents;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;

public record PresetData(String name, EntityType<?> entityType, CompoundTag data) {

  public static final String ID = "preset_data";
  public static final String EMPTY_NAME = "Empty";

  public static final PresetData EMPTY =
      new PresetData(EMPTY_NAME, EntityType.ARMOR_STAND, new CompoundTag());
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
                      CompoundTag.CODEC.fieldOf("data").forGetter(PresetData::data))
                  .apply(instance, PresetData::new));
  public static final StreamCodec<RegistryFriendlyByteBuf, PresetData> STREAM_CODEC =
      StreamCodec.composite(
          ByteBufCodecs.STRING_UTF8,
          PresetData::name,
          ByteBufCodecs.registry(Registries.ENTITY_TYPE),
          PresetData::entityType,
          ByteBufCodecs.COMPOUND_TAG,
          PresetData::data,
          PresetData::new);

  public PresetData(final EntityType<?> entityType, final CompoundTag data) {
    this(entityType.getDescriptionId(), entityType, data);
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

  public boolean isEmpty() {
    return this.equals(EMPTY);
  }

  public boolean hasData() {
    return this.data != null && !this.data.isEmpty();
  }

  public boolean hasEntityType() {
    return this.entityType != null;
  }
}
