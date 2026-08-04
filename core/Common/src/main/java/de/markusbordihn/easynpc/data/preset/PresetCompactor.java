/*
 * Copyright 2026 Markus Bordihn
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

import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import java.util.Set;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attribute;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.DefaultAttributes;

public class PresetCompactor {

  private static final Set<String> ALWAYS_KEPT_TAGS =
      Set.of(
          Entity.ID_TAG,
          PresetData.PRESET_UUID_TAG,
          PresetDataCapable.PRESET_METADATA_TAG,
          ConfigDataCapable.DATA_EASY_NPC_DATA_VERSION_TAG,
          ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG);

  // Keyed by event type, so a merge would bring a deliberately removed event back from the
  // reference. These tags are compared, stored and restored as a whole instead.
  private static final Set<String> ATOMIC_TAGS =
      Set.of(ActionEventDataCapable.DATA_ACTION_DATA_TAG);

  private static final String ATTRIBUTES_TAG = "Attributes";
  private static final String ATTRIBUTE_NAME_TAG = "Name";
  private static final String ATTRIBUTE_BASE_TAG = "Base";
  private static final String ATTRIBUTE_MODIFIERS_TAG = "Modifiers";

  private PresetCompactor() {}

  public static CompoundTag compact(CompoundTag entityData, CompoundTag referenceData) {
    if (entityData == null || entityData.isEmpty() || referenceData == null) {
      return entityData;
    }

    CompoundTag compactedData = entityData.copy();
    PresetNormalizer.removeStatusRuntimeData(compactedData);
    removeUnchangedAttributes(compactedData, referenceData);
    removeEqualTags(compactedData, referenceData, true);

    return compactedData;
  }

  public static CompoundTag expand(CompoundTag entityData, CompoundTag referenceData) {
    if (entityData == null || referenceData == null) {
      return entityData;
    }

    ListTag mergedAttributes = mergeAttributes(entityData, referenceData);
    CompoundTag expandedData = referenceData.copy().merge(entityData);
    if (mergedAttributes != null) {
      expandedData.put(ATTRIBUTES_TAG, mergedAttributes);
    }
    for (String atomicTag : ATOMIC_TAGS) {
      Tag entityValue = entityData.get(atomicTag);
      if (entityValue != null) {
        expandedData.put(atomicTag, entityValue.copy());
      }
    }

    return expandedData;
  }

  private static void removeEqualTags(
      CompoundTag entityData, CompoundTag referenceData, boolean isRootLevel) {
    for (String key : Set.copyOf(entityData.getAllKeys())) {
      if (isRootLevel && ALWAYS_KEPT_TAGS.contains(key)) {
        continue;
      }

      Tag referenceValue = referenceData.get(key);
      if (referenceValue == null) {
        continue;
      }

      Tag entityValue = entityData.get(key);
      if (entityValue.equals(referenceValue)) {
        entityData.remove(key);
        continue;
      }

      if (isRootLevel && ATOMIC_TAGS.contains(key)) {
        continue;
      }

      if (entityValue instanceof CompoundTag entityCompoundTag
          && referenceValue instanceof CompoundTag referenceCompoundTag) {
        removeEqualTags(entityCompoundTag, referenceCompoundTag, false);
        if (entityCompoundTag.isEmpty()) {
          entityData.remove(key);
        }
      }
    }
  }

  private static void removeUnchangedAttributes(CompoundTag entityData, CompoundTag referenceData) {
    if (!entityData.contains(ATTRIBUTES_TAG, Tag.TAG_LIST) || referenceData == null) {
      return;
    }

    ListTag attributeList = entityData.getList(ATTRIBUTES_TAG, Tag.TAG_COMPOUND);
    ListTag referenceAttributeList = referenceData.getList(ATTRIBUTES_TAG, Tag.TAG_COMPOUND);
    AttributeSupplier entityTypeDefaults = getEntityTypeDefaults(entityData);
    ListTag changedAttributeList = new ListTag();
    for (int index = 0; index < attributeList.size(); index++) {
      CompoundTag attributeTag = attributeList.getCompound(index);
      CompoundTag referenceAttributeTag =
          findAttribute(referenceAttributeList, attributeTag.getString(ATTRIBUTE_NAME_TAG));
      if (referenceAttributeTag != null
          ? attributeTag.equals(referenceAttributeTag)
          : isEntityTypeDefault(entityTypeDefaults, attributeTag)) {
        continue;
      }

      changedAttributeList.add(attributeTag);
    }

    if (changedAttributeList.isEmpty()) {
      entityData.remove(ATTRIBUTES_TAG);
    } else {
      entityData.put(ATTRIBUTES_TAG, changedAttributeList);
    }
  }

  private static AttributeSupplier getEntityTypeDefaults(CompoundTag entityData) {
    EntityType<?> entityType =
        EntityType.byString(entityData.getString(Entity.ID_TAG)).orElse(null);
    return entityType != null ? DefaultAttributes.getSupplier(asLivingEntityType(entityType)) : null;
  }

  @SuppressWarnings("unchecked")
  private static EntityType<? extends LivingEntity> asLivingEntityType(EntityType<?> entityType) {
    return (EntityType<? extends LivingEntity>) entityType;
  }

  private static boolean isEntityTypeDefault(
      AttributeSupplier entityTypeDefaults, CompoundTag attributeTag) {
    if (entityTypeDefaults == null || attributeTag.contains(ATTRIBUTE_MODIFIERS_TAG)) {
      return false;
    }

    Attribute attribute =
        BuiltInRegistries.ATTRIBUTE.get(
            ResourceLocation.tryParse(attributeTag.getString(ATTRIBUTE_NAME_TAG)));
    return attribute != null
        && entityTypeDefaults.hasAttribute(attribute)
        && entityTypeDefaults.getBaseValue(attribute) == attributeTag.getDouble(ATTRIBUTE_BASE_TAG);
  }

  private static ListTag mergeAttributes(CompoundTag entityData, CompoundTag referenceData) {
    if (!entityData.contains(ATTRIBUTES_TAG, Tag.TAG_LIST)) {
      return null;
    }

    ListTag mergedAttributes =
        referenceData.contains(ATTRIBUTES_TAG, Tag.TAG_LIST)
            ? referenceData.getList(ATTRIBUTES_TAG, Tag.TAG_COMPOUND).copy()
            : new ListTag();
    ListTag changedAttributes = entityData.getList(ATTRIBUTES_TAG, Tag.TAG_COMPOUND);
    for (int index = 0; index < changedAttributes.size(); index++) {
      CompoundTag changedAttribute = changedAttributes.getCompound(index);
      int referenceIndex =
          findAttributeIndex(mergedAttributes, changedAttribute.getString(ATTRIBUTE_NAME_TAG));
      if (referenceIndex >= 0) {
        mergedAttributes.set(referenceIndex, changedAttribute.copy());
      } else {
        mergedAttributes.add(changedAttribute.copy());
      }
    }

    return mergedAttributes;
  }

  private static CompoundTag findAttribute(ListTag attributes, String attributeName) {
    int index = findAttributeIndex(attributes, attributeName);
    return index >= 0 ? attributes.getCompound(index) : null;
  }

  private static int findAttributeIndex(ListTag attributes, String attributeName) {
    if (attributeName == null || attributeName.isEmpty()) {
      return -1;
    }

    for (int index = 0; index < attributes.size(); index++) {
      if (attributeName.equals(attributes.getCompound(index).getString(ATTRIBUTE_NAME_TAG))) {
        return index;
      }
    }

    return -1;
  }
}
