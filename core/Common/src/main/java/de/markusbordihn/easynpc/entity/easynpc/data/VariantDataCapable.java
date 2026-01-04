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

import de.markusbordihn.easynpc.data.skin.variant.HumanoidSkinVariant;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.Locale;
import java.util.stream.Stream;
import net.minecraft.core.Holder;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.npc.villager.VillagerProfession;
import net.minecraft.world.entity.npc.villager.VillagerType;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface VariantDataCapable<T extends PathfinderMob> extends EasyNPC<T> {

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
    return variant != null && variant.name().endsWith("_CROSSED_ARMS");
  }

  default boolean hasVariantTypeSaddled() {
    return this.hasVariantTypeSaddled(getSkinVariantType());
  }

  default boolean hasVariantTypeSaddled(Enum<?> variant) {
    return variant != null && variant.name().endsWith("_SADDLED");
  }

  default Holder<VillagerProfession> getVillagerProfession(Enum<?> variantType) {
    String name = variantType.name().toLowerCase(Locale.ROOT);
    for (VillagerProfession profession : BuiltInRegistries.VILLAGER_PROFESSION) {
      if (name.endsWith(
          BuiltInRegistries.VILLAGER_PROFESSION
              .getKey(profession)
              .getPath()
              .toLowerCase(Locale.ROOT))) {
        return BuiltInRegistries.VILLAGER_PROFESSION.wrapAsHolder(profession);
      }
    }
    return null;
  }

  default Holder<VillagerType> getVillagerType(Enum<?> variantType) {
    String name = variantType.name().toLowerCase(Locale.ROOT);
    for (VillagerType villagerType : BuiltInRegistries.VILLAGER_TYPE) {
      Holder<VillagerType> holder = BuiltInRegistries.VILLAGER_TYPE.wrapAsHolder(villagerType);
      String typeName = BuiltInRegistries.VILLAGER_TYPE.getKey(villagerType).getPath();
      if (name.startsWith(typeName.toLowerCase(Locale.ROOT))) {
        return holder;
      }
    }
    return null;
  }

  default void defineSynchedVariantData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(
        builder, SynchedDataIndex.VARIANT_TYPE, getDefaultSkinVariantType().name());
  }

  default void addAdditionalVariantData(ValueOutput valueOutput) {
    if (this.getSkinVariantType() != null) {
      valueOutput.putString(EASY_NPC_DATA_VARIANT_TYPE_TAG, this.getSkinVariantType().name());
    }
  }

  default void readAdditionalVariantData(ValueInput valueInput) {
    String variantType = valueInput.getString(EASY_NPC_DATA_VARIANT_TYPE_TAG).orElse("");
    if (!variantType.isEmpty()) {
      this.setSkinVariantType(this.getSkinVariantType(variantType));
    }
  }
}
