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

import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.EnumMap;
import java.util.stream.Stream;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;

public interface VariantData<T extends PathfinderMob> extends EasyNPC<T> {

  String EASY_NPC_DATA_VARIANT_TAG = "Variant";

  static void registerSyncedVariantData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Variant Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.VARIANT,
        SynchedEntityData.defineId(
            EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.STRING));
  }

  default Enum<?> getDefaultVariant() {
    return Variant.STEVE;
  }

  default Enum<?> getVariant() {
    return getVariant(getSynchedEntityData(SynchedDataIndex.VARIANT));
  }

  default void setVariant(Enum<?> variant) {
    setSynchedEntityData(SynchedDataIndex.VARIANT, variant != null ? variant.name() : "");
  }

  default void setVariant(String name) {
    Enum<?> variant = getVariant(name);
    if (variant != null) {
      setVariant(variant);
    } else {
      log.error("Unknown variant {} for {}", name, this);
    }
  }

  default Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

  default Enum<?>[] getVariants() {
    return Variant.values();
  }

  default Stream<String> getVariantNames() {
    return Stream.of(getVariants()).map(Enum::name);
  }

  default Component getVariantName() {
    Enum<?> variant = getVariant();
    return variant != null ? TextUtils.normalizeName(variant.name()) : getEasyNPCTypeName();
  }

  default boolean hasVariantCrossedArms() {
    return this.hasVariantCrossedArms(getVariant());
  }

  default boolean hasVariantCrossedArms(Enum<?> variant) {
    return variant != null && variant.name().endsWith("_CROSSED_ARMS");
  }

  default boolean hasVariantSaddled() {
    return this.hasVariantSaddled(getVariant());
  }

  default boolean hasVariantSaddled(Enum<?> variant) {
    return variant != null && variant.name().endsWith("_SADDLED");
  }

  default void defineSynchedVariantData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.VARIANT, getDefaultVariant().name());
  }

  default void addAdditionalVariantData(CompoundTag compoundTag) {
    if (this.getVariant() != null) {
      compoundTag.putString(EASY_NPC_DATA_VARIANT_TAG, this.getVariant().name());
    }
  }

  default void readAdditionalVariantData(CompoundTag compoundTag) {
    if (compoundTag.contains(EASY_NPC_DATA_VARIANT_TAG)) {
      String variant = compoundTag.getString(EASY_NPC_DATA_VARIANT_TAG);
      if (!variant.isEmpty()) {
        this.setVariant(this.getVariant(variant));
      }
    }
  }

  enum Variant {
    STEVE,
    ALEX
  }
}
