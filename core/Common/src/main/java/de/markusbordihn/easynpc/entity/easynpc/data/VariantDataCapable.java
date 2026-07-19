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

import de.markusbordihn.easynpc.api.skin.CrossedArmsVariant;
import de.markusbordihn.easynpc.api.skin.SaddleableVariant;
import de.markusbordihn.easynpc.data.skin.variant.HumanoidSkinVariant;
import de.markusbordihn.easynpc.data.skin.variant.VillagerVariantData;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.stream.Stream;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.npc.VillagerProfession;
import net.minecraft.world.entity.npc.VillagerType;

public interface VariantDataCapable<T extends Mob> extends EasyNPC<T> {

  String EASY_NPC_DATA_VARIANT_TYPE_TAG = "VariantType";

  default Enum<?> getDefaultSkinVariantType() {
    return HumanoidSkinVariant.STEVE;
  }

  default Enum<?> getSkinVariantType() {
    return getSkinVariantType(getSynchedEntityData(SynchedDataIndex.VARIANT_TYPE));
  }

  default void setSkinVariantType(Enum<?> variant) {
    if (getSkinVariantType() != variant) {
      setSynchedEntityData(SynchedDataIndex.VARIANT_TYPE, variant != null ? variant.name() : "");
      handleSkinVariantTypeChange(variant);
    }
  }

  default void setSkinVariantType(String name) {
    Enum<?> variantType = getSkinVariantType(name);
    if (variantType != null) {
      setSkinVariantType(variantType);
    } else {
      log.error("Unknown variant {} for {}", name, this);
    }
  }

  default void handleSkinVariantTypeChange(Enum<?> variant) {
    // Handle variant change if needed.
  }

  default Enum<?> getSkinVariantType(String name) {
    return HumanoidSkinVariant.valueOf(name);
  }

  default Enum<?>[] getSkinVariantTypes() {
    return HumanoidSkinVariant.values();
  }

  default Stream<String> getSkinVariantTypeNames() {
    return Stream.of(getSkinVariantTypes()).map(Enum::name);
  }

  default Component getSkinVariantTypeName() {
    Enum<?> variant = getSkinVariantType();
    return variant != null ? TextUtils.normalizeName(variant.name()) : getEntityTypeName();
  }

  default boolean hasVariantTypeCrossedArms() {
    return this.hasVariantTypeCrossedArms(getSkinVariantType());
  }

  default boolean hasVariantTypeCrossedArms(Enum<?> variant) {
    return variant instanceof CrossedArmsVariant crossedArmsVariant
        && crossedArmsVariant.hasCrossedArms();
  }

  default boolean hasVariantTypeSaddled() {
    return this.hasVariantTypeSaddled(getSkinVariantType());
  }

  default boolean hasVariantTypeSaddled(Enum<?> variant) {
    return variant instanceof SaddleableVariant saddleableVariant && saddleableVariant.isSaddled();
  }

  default VillagerProfession getVillagerProfession(Enum<?> variantType) {
    if (!(variantType instanceof VillagerVariantData villagerVariant)) {
      return null;
    }

    return BuiltInRegistries.VILLAGER_PROFESSION.get(
        villagerVariant.getProfession().getRegistryKey());
  }

  default VillagerType getVillagerType(Enum<?> variantType) {
    if (!(variantType instanceof VillagerVariantData villagerVariant)) {
      return null;
    }

    return BuiltInRegistries.VILLAGER_TYPE.get(villagerVariant.getVillagerBiome().getRegistryKey());
  }

  default void defineSynchedVariantData() {
    defineSynchedEntityData(SynchedDataIndex.VARIANT_TYPE, getDefaultSkinVariantType().name());
  }

  default void addAdditionalVariantData(CompoundTag compoundTag) {
    if (this.getSkinVariantType() != null) {
      compoundTag.putString(EASY_NPC_DATA_VARIANT_TYPE_TAG, this.getSkinVariantType().name());
    }
  }

  default void readAdditionalVariantData(CompoundTag compoundTag) {
    if (compoundTag.contains(EASY_NPC_DATA_VARIANT_TYPE_TAG)) {
      String variantType = compoundTag.getString(EASY_NPC_DATA_VARIANT_TYPE_TAG);
      if (!variantType.isEmpty()) {
        this.setSkinVariantType(this.getSkinVariantType(variantType));
      }
    }
  }
}
