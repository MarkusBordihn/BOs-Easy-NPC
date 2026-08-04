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

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;

public class PresetNormalizer {

  // Only set while an NPC is alive, so exporting them would carry another NPC's progress along.
  private static final List<String> STATUS_RUNTIME_TAGS =
      List.of(
          StatusDataType.NPC_DATA_LAST_UPDATE.getTagName(),
          StatusDataType.NPC_DATA_LAST_SAVED.getTagName(),
          StatusDataType.SPAWN_ACTION_FIRED.getTagName());
  private static final List<String> NON_PRESET_ENTITY_TAGS = List.of("CanUpdate", "EggLayTime");
  private static final String ATTRIBUTES_TAG = "Attributes";
  private static final String ATTRIBUTE_NAME_TAG = "Name";
  private static final String ATTRIBUTE_MODIFIERS_TAG = "Modifiers";

  // Rolled per spawn by vanilla, so exporting them would freeze one random roll into the preset.
  private static final Set<String> SPAWN_MODIFIER_NAMES =
      Set.of("Random spawn bonus", "Leader zombie bonus", "Zombie reinforcement caller charge");

  private PresetNormalizer() {}

  public static void normalize(CompoundTag entityData) {
    if (entityData == null || entityData.isEmpty()) {
      return;
    }

    removeStatusRuntimeData(entityData);
    NON_PRESET_ENTITY_TAGS.forEach(entityData::remove);
    removeSkinTimestamp(entityData);
    sortAttributes(entityData);
    removeActionIdentifiers(entityData);
  }

  static void removeStatusRuntimeData(CompoundTag entityData) {
    if (!entityData.contains(StatusDataCapable.DATA_STATUS_DATA_TAG)) {
      return;
    }

    CompoundTag statusDataTag = entityData.getCompound(StatusDataCapable.DATA_STATUS_DATA_TAG);
    STATUS_RUNTIME_TAGS.forEach(statusDataTag::remove);
    if (statusDataTag.isEmpty()) {
      entityData.remove(StatusDataCapable.DATA_STATUS_DATA_TAG);
    } else {
      entityData.put(StatusDataCapable.DATA_STATUS_DATA_TAG, statusDataTag);
    }
  }

  private static void removeSkinTimestamp(CompoundTag entityData) {
    if (!entityData.contains(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG)) {
      return;
    }

    CompoundTag skinDataTag = entityData.getCompound(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG);
    skinDataTag.remove(SkinDataEntry.DATA_TIMESTAMP_TAG);
    entityData.put(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG, skinDataTag);
  }

  private static void sortAttributes(CompoundTag entityData) {
    if (!entityData.contains(ATTRIBUTES_TAG, Tag.TAG_LIST)) {
      return;
    }

    ListTag attributeList = entityData.getList(ATTRIBUTES_TAG, Tag.TAG_COMPOUND);
    if (attributeList.isEmpty()) {
      entityData.remove(ATTRIBUTES_TAG);
      return;
    }

    List<CompoundTag> attributes = new ArrayList<>();
    for (int index = 0; index < attributeList.size(); index++) {
      CompoundTag attribute = attributeList.getCompound(index);
      removeSpawnModifiers(attribute);
      attributes.add(attribute);
    }
    attributes.sort(Comparator.comparing(attribute -> attribute.getString(ATTRIBUTE_NAME_TAG)));

    ListTag sortedAttributeList = new ListTag();
    sortedAttributeList.addAll(attributes);
    entityData.put(ATTRIBUTES_TAG, sortedAttributeList);
  }

  private static void removeSpawnModifiers(CompoundTag attribute) {
    if (!attribute.contains(ATTRIBUTE_MODIFIERS_TAG, Tag.TAG_LIST)) {
      return;
    }

    ListTag modifierList = attribute.getList(ATTRIBUTE_MODIFIERS_TAG, Tag.TAG_COMPOUND);
    ListTag keptModifiers = new ListTag();
    for (int index = 0; index < modifierList.size(); index++) {
      CompoundTag modifier = modifierList.getCompound(index);
      if (!SPAWN_MODIFIER_NAMES.contains(modifier.getString(ATTRIBUTE_NAME_TAG))) {
        keptModifiers.add(modifier);
      }
    }

    if (keptModifiers.isEmpty()) {
      attribute.remove(ATTRIBUTE_MODIFIERS_TAG);
    } else {
      attribute.put(ATTRIBUTE_MODIFIERS_TAG, keptModifiers);
    }
  }

  private static void removeActionIdentifiers(CompoundTag entityData) {
    if (!entityData.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG)) {
      return;
    }

    CompoundTag actionDataTag = entityData.getCompound(ActionEventDataCapable.DATA_ACTION_DATA_TAG);
    CompoundTag actionEventSetTag =
        actionDataTag.getCompound(ActionEventSet.DATA_ACTION_EVENT_SET_TAG);
    for (String actionEventName : actionEventSetTag.getAllKeys()) {
      ListTag actionEntries = actionEventSetTag.getList(actionEventName, Tag.TAG_COMPOUND);
      for (int index = 0; index < actionEntries.size(); index++) {
        actionEntries.getCompound(index).remove(ActionDataEntry.DATA_ID_TAG);
      }
    }

    actionDataTag.put(ActionEventSet.DATA_ACTION_EVENT_SET_TAG, actionEventSetTag);
    entityData.put(ActionEventDataCapable.DATA_ACTION_DATA_TAG, actionDataTag);
  }
}
