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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetInheritance {

  public static final int MAX_PARENT_DEPTH = 8;

  private static final List<String> IDENTITY_TAGS =
      List.of(PresetData.UUID_TAG, PresetData.PRESET_UUID_TAG);
  private static final List<String> NON_INHERITED_METADATA_TAGS =
      List.of(PresetMetadata.TAG_NAME, PresetMetadata.TAG_CATEGORY, PresetMetadata.TAG_ACCESS);
  private static final List<String> ENTITY_TYPE_SPECIFIC_TAGS =
      List.of(
          VariantDataCapable.EASY_NPC_DATA_VARIANT_TYPE_TAG,
          SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG,
          ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG,
          ProfessionDataCapable.DATA_PROFESSION_TAG);
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private PresetInheritance() {}

  public static CompoundTag resolve(
      CompoundTag compoundTag,
      Identifier presetLocation,
      Function<Identifier, CompoundTag> parentLoader) {
    if (compoundTag == null
        || parentLoader == null
        || !compoundTag.contains(PresetData.PARENT_TAG)) {
      return compoundTag;
    }

    Set<Identifier> visitedPresets = new LinkedHashSet<>();
    visitedPresets.add(presetLocation);

    return resolve(compoundTag.copy(), presetLocation, parentLoader, visitedPresets, 0);
  }

  private static CompoundTag resolve(
      CompoundTag compoundTag,
      Identifier presetLocation,
      Function<Identifier, CompoundTag> parentLoader,
      Set<Identifier> visitedPresets,
      int parentDepth) {
    if (!compoundTag.contains(PresetData.PARENT_TAG)) {
      return compoundTag;
    }

    Identifier parentLocation =
        Identifier.tryParse(compoundTag.getString(PresetData.PARENT_TAG).orElse(""));
    compoundTag.remove(PresetData.PARENT_TAG);

    if (parentLocation == null) {
      log.error("Preset {} references a parent preset with an invalid id", presetLocation);
      return null;
    }

    if (parentDepth + 1 > MAX_PARENT_DEPTH) {
      log.error(
          "Preset {} builds on more than {} parent presets: {}",
          presetLocation,
          MAX_PARENT_DEPTH,
          visitedPresets);
      return null;
    }

    if (!visitedPresets.add(parentLocation)) {
      log.error(
          "Preset {} references the parent preset {} in a cycle: {}",
          presetLocation,
          parentLocation,
          visitedPresets);
      return null;
    }

    CompoundTag parentCompoundTag = parentLoader.apply(parentLocation);
    if (parentCompoundTag == null) {
      log.error("Unable to load parent preset {} for preset {}", parentLocation, presetLocation);
      return null;
    }

    CompoundTag resolvedParentCompoundTag =
        resolve(
            parentCompoundTag.copy(),
            parentLocation,
            parentLoader,
            visitedPresets,
            parentDepth + 1);
    if (resolvedParentCompoundTag == null) {
      return null;
    }

    if (PresetData.usesEntityDataWrapper(resolvedParentCompoundTag)
        != PresetData.usesEntityDataWrapper(compoundTag)) {
      log.error(
          "Preset {} and its parent preset {} store their entity data differently, "
              + "one of them has to be wrapped in a '{}' tag as well",
          presetLocation,
          parentLocation,
          PresetData.DATA_TAG);
      return null;
    }

    removeInheritedIdentity(resolvedParentCompoundTag);
    if (isDifferentEntityType(compoundTag, resolvedParentCompoundTag)) {
      log.info(
          "Preset {} is a different NPC type than its parent preset {}, "
              + "its skin, model and profession are not inherited",
          presetLocation,
          parentLocation);
      removeInheritedLook(resolvedParentCompoundTag);
    }

    return resolvedParentCompoundTag.merge(compoundTag);
  }

  private static void removeInheritedIdentity(CompoundTag compoundTag) {
    IDENTITY_TAGS.forEach(compoundTag::remove);
    removeFrom(compoundTag, PresetData.DATA_TAG, IDENTITY_TAGS);
    removeFrom(compoundTag, PresetDataCapable.PRESET_METADATA_TAG, NON_INHERITED_METADATA_TAGS);
  }

  private static void removeInheritedLook(CompoundTag compoundTag) {
    if (PresetData.usesEntityDataWrapper(compoundTag)) {
      removeFrom(compoundTag, PresetData.DATA_TAG, ENTITY_TYPE_SPECIFIC_TAGS);
    } else {
      ENTITY_TYPE_SPECIFIC_TAGS.forEach(compoundTag::remove);
    }

    removeFrom(
        compoundTag,
        PresetDataCapable.PRESET_METADATA_TAG,
        List.of(PresetMetadata.TAG_ENTITY_TYPE_ID, PresetMetadata.TAG_VARIANT_TYPE));
  }

  private static boolean isDifferentEntityType(
      CompoundTag compoundTag, CompoundTag parentCompoundTag) {
    String entityTypeId = entityTypeId(compoundTag);
    return !entityTypeId.isEmpty() && !entityTypeId.equals(entityTypeId(parentCompoundTag));
  }

  private static String entityTypeId(CompoundTag compoundTag) {
    CompoundTag entityData =
        PresetData.usesEntityDataWrapper(compoundTag)
            ? compoundTag.getCompoundOrEmpty(PresetData.DATA_TAG)
            : compoundTag;
    return entityData.getString(PresetData.ID_TAG).orElse("");
  }

  private static void removeFrom(
      CompoundTag compoundTag, String childTag, List<String> tagsToRemove) {
    CompoundTag childCompoundTag = compoundTag.getCompoundOrEmpty(childTag);
    if (childCompoundTag.isEmpty()) {
      return;
    }

    tagsToRemove.forEach(childCompoundTag::remove);
    compoundTag.put(childTag, childCompoundTag);
  }
}
