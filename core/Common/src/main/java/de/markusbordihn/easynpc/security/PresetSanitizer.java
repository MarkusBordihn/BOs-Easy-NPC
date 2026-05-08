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

package de.markusbordihn.easynpc.security;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.world.entity.Entity;

public class PresetSanitizer {

  private PresetSanitizer() {}

  public static PresetSanitizationResult sanitize(
      CompoundTag compoundTag, PresetAuthority presetAuthority) {
    if (compoundTag == null || compoundTag.isEmpty() || presetAuthority == null) {
      return new PresetSanitizationResult(compoundTag, presetAuthority, false, List.of());
    }

    CompoundTag sanitizedTag = compoundTag.copy();
    List<PresetSanitizationNotice> notices = new ArrayList<>();
    sanitizeOwner(sanitizedTag, presetAuthority, notices);
    sanitizeTrading(sanitizedTag, presetAuthority, notices);
    sanitizeObjectives(sanitizedTag, presetAuthority, notices);
    sanitizeMovementAndAttributes(sanitizedTag, presetAuthority, notices);
    sanitizeActionAuthority(sanitizedTag, presetAuthority.commandPermissionLevel(), notices);
    sanitizeActionEntries(sanitizedTag, presetAuthority, notices);

    return new PresetSanitizationResult(
        sanitizedTag, presetAuthority, !notices.isEmpty(), List.copyOf(notices));
  }

  public static PresetFeaturePreview preview(
      CompoundTag compoundTag, PresetAuthority presetAuthority) {
    if (compoundTag == null || compoundTag.isEmpty() || presetAuthority == null) {
      return PresetFeaturePreview.empty();
    }

    EnumMap<NpcFeature, PresetFeatureNotice> notices = new EnumMap<>(NpcFeature.class);
    collectPresentFeatures(compoundTag, presetAuthority, notices);

    PresetSanitizationResult result = sanitize(compoundTag, presetAuthority);
    for (PresetSanitizationNotice notice : result.notices()) {
      PresetFeatureNotice featureNotice = toFeatureNotice(notice, presetAuthority);
      if (featureNotice != null) {
        putNotice(notices, featureNotice);
      }
    }

    return new PresetFeaturePreview(List.copyOf(notices.values()));
  }

  public static CompoundTag sanitizeForExport(CompoundTag compoundTag) {
    if (compoundTag == null || compoundTag.isEmpty()) {
      return compoundTag;
    }

    CompoundTag sanitizedTag = compoundTag.copy();
    sanitizedTag.remove(Entity.UUID_TAG);
    sanitizedTag.remove(OwnerDataCapable.DATA_OWNER_TAG);
    sanitizedTag.remove("Pos");
    sanitizedTag.remove("Rotation");
    removeActionAuthority(sanitizedTag);

    return sanitizedTag;
  }

  private static void removeActionAuthority(CompoundTag compoundTag) {
    if (!compoundTag.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG)) {
      return;
    }

    CompoundTag actionDataTag =
        compoundTag.getCompound(ActionEventDataCapable.DATA_ACTION_DATA_TAG);
    actionDataTag.remove(ActionEventDataCapable.DATA_ACTION_PERMISSION_LEVEL_TAG);
  }

  private static void sanitizeOwner(
      CompoundTag compoundTag,
      PresetAuthority presetAuthority,
      List<PresetSanitizationNotice> notices) {
    if (presetAuthority.ownerUuid() != null) {
      if (!compoundTag.hasUUID(OwnerDataCapable.DATA_OWNER_TAG)
          || !presetAuthority
              .ownerUuid()
              .equals(compoundTag.getUUID(OwnerDataCapable.DATA_OWNER_TAG))) {
        notices.add(PresetSanitizationNotice.OWNER_REWRITTEN);
      }
      compoundTag.putUUID(OwnerDataCapable.DATA_OWNER_TAG, presetAuthority.ownerUuid());
      return;
    }

    if (compoundTag.contains(OwnerDataCapable.DATA_OWNER_TAG)) {
      compoundTag.remove(OwnerDataCapable.DATA_OWNER_TAG);
      notices.add(PresetSanitizationNotice.OWNER_REMOVED);
    }
  }

  private static void sanitizeActionAuthority(
      CompoundTag compoundTag,
      CommandPermissionLevel commandPermissionLevel,
      List<PresetSanitizationNotice> notices) {
    if (!compoundTag.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG)) {
      return;
    }

    CompoundTag actionDataTag =
        compoundTag.getCompound(ActionEventDataCapable.DATA_ACTION_DATA_TAG);
    int sanitizedLevel = commandPermissionLevel.minecraftLevel();
    if (!actionDataTag.contains(ActionEventDataCapable.DATA_ACTION_PERMISSION_LEVEL_TAG)
        || actionDataTag.getInt(ActionEventDataCapable.DATA_ACTION_PERMISSION_LEVEL_TAG)
            != sanitizedLevel) {
      notices.add(PresetSanitizationNotice.ACTION_PERMISSION_CLAMPED);
    }
    actionDataTag.putInt(ActionEventDataCapable.DATA_ACTION_PERMISSION_LEVEL_TAG, sanitizedLevel);
  }

  private static void sanitizeTrading(
      CompoundTag compoundTag,
      PresetAuthority presetAuthority,
      List<PresetSanitizationNotice> notices) {
    if (FeatureSecurity.isFeatureAllowed(presetAuthority, NpcFeature.TRADING)) {
      return;
    }

    boolean changed = compoundTag.contains(TradingDataCapable.DATA_TRADING_DATA_TAG);
    compoundTag.remove(TradingDataCapable.DATA_TRADING_DATA_TAG);
    if (changed) {
      notices.add(PresetSanitizationNotice.TRADING_REMOVED);
    }
  }

  private static void sanitizeObjectives(
      CompoundTag compoundTag,
      PresetAuthority presetAuthority,
      List<PresetSanitizationNotice> notices) {
    if (FeatureSecurity.isFeatureAllowed(presetAuthority, NpcFeature.OBJECTIVE)) {
      return;
    }

    if (compoundTag.contains(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG)) {
      compoundTag.remove(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG);
      notices.add(PresetSanitizationNotice.OBJECTIVE_REMOVED);
    }
  }

  private static void sanitizeMovementAndAttributes(
      CompoundTag compoundTag,
      PresetAuthority presetAuthority,
      List<PresetSanitizationNotice> notices) {
    if (!FeatureSecurity.isFeatureAllowed(presetAuthority, NpcFeature.MOVEMENT)
        && compoundTag.contains(NavigationDataCapable.DATA_NAVIGATION_TAG)) {
      compoundTag.remove(NavigationDataCapable.DATA_NAVIGATION_TAG);
      notices.add(PresetSanitizationNotice.MOVEMENT_REMOVED);
    }

    if ((!FeatureSecurity.isFeatureAllowed(presetAuthority, NpcFeature.MOVEMENT)
            || !FeatureSecurity.isFeatureAllowed(presetAuthority, NpcFeature.COMBAT_ATTRIBUTE)
            || !FeatureSecurity.isFeatureAllowed(presetAuthority, NpcFeature.BASE_ATTRIBUTE))
        && compoundTag.contains(EntityAttributes.ENTITY_ATTRIBUTE_TAG)) {
      compoundTag.remove(EntityAttributes.ENTITY_ATTRIBUTE_TAG);
      notices.add(PresetSanitizationNotice.ATTRIBUTE_REMOVED);
    }
  }

  private static void sanitizeActionEntries(
      Tag tag, PresetAuthority presetAuthority, List<PresetSanitizationNotice> notices) {
    if (tag instanceof CompoundTag compoundTag) {
      sanitizeActionEntryPermission(compoundTag, presetAuthority.commandPermissionLevel(), notices);
      for (String key : compoundTag.getAllKeys()) {
        sanitizeActionEntries(compoundTag.get(key), presetAuthority, notices);
      }
    } else if (tag instanceof ListTag listTag) {
      for (int index = listTag.size() - 1; index >= 0; index--) {
        Tag listEntry = listTag.get(index);
        if (listEntry instanceof CompoundTag actionEntryTag
            && isBlockedActionEntry(actionEntryTag, presetAuthority, notices)) {
          listTag.remove(index);
          continue;
        }
        sanitizeActionEntries(listEntry, presetAuthority, notices);
      }
    }
  }

  private static boolean isBlockedActionEntry(
      CompoundTag compoundTag,
      PresetAuthority presetAuthority,
      List<PresetSanitizationNotice> notices) {
    if (!compoundTag.contains(ActionDataEntry.DATA_TYPE_TAG)) {
      return false;
    }

    ActionDataType actionDataType =
        ActionDataType.get(compoundTag.getString(ActionDataEntry.DATA_TYPE_TAG));
    if (actionDataType == null) {
      return false;
    }

    if (FeatureSecurity.isActionTypeAllowed(presetAuthority, actionDataType)) {
      return false;
    }

    PresetSanitizationNotice notice = toActionRemovalNotice(actionDataType);
    if (notice != null) {
      notices.add(notice);
    }

    return notice != null;
  }

  private static void sanitizeActionEntryPermission(
      CompoundTag compoundTag,
      CommandPermissionLevel commandPermissionLevel,
      List<PresetSanitizationNotice> notices) {
    if (!compoundTag.contains(ActionDataEntry.DATA_PERMISSION_LEVEL_TAG)) {
      return;
    }

    CommandPermissionLevel requestedLevel =
        CommandPermissionLevel.fromMinecraftLevel(
            compoundTag.getInt(ActionDataEntry.DATA_PERMISSION_LEVEL_TAG));
    CommandPermissionLevel sanitizedLevel =
        CommandPermissionLevel.min(requestedLevel, commandPermissionLevel);
    if (requestedLevel != sanitizedLevel) {
      compoundTag.putInt(
          ActionDataEntry.DATA_PERMISSION_LEVEL_TAG, sanitizedLevel.minecraftLevel());
      notices.add(PresetSanitizationNotice.COMMAND_PERMISSION_CLAMPED);
    }
  }

  private static void collectPresentFeatures(
      CompoundTag compoundTag,
      PresetAuthority presetAuthority,
      Map<NpcFeature, PresetFeatureNotice> notices) {
    if (compoundTag.contains(TradingDataCapable.DATA_TRADING_DATA_TAG)) {
      addFeaturePresence(notices, presetAuthority, NpcFeature.TRADING);
    }
    if (hasDialogData(compoundTag)) {
      addFeaturePresence(notices, presetAuthority, NpcFeature.DIALOG);
    }
    if (hasObjectiveData(compoundTag)) {
      addFeaturePresence(notices, presetAuthority, NpcFeature.OBJECTIVE);
    }
    if (compoundTag.contains(NavigationDataCapable.DATA_NAVIGATION_TAG)) {
      addFeaturePresence(notices, presetAuthority, NpcFeature.MOVEMENT);
    }
    if (compoundTag.contains("Pos") || compoundTag.contains("Rotation")) {
      addFeaturePresence(notices, presetAuthority, NpcFeature.POSITION);
    }
    collectActionFeatures(compoundTag, presetAuthority, notices);
  }

  private static boolean hasDialogData(CompoundTag compoundTag) {
    if (!compoundTag.contains("DialogData")) {
      return false;
    }
    CompoundTag dialogData = compoundTag.getCompound("DialogData");
    return dialogData.contains("DialogDataSet")
        && !dialogData.getList("DialogDataSet", Tag.TAG_COMPOUND).isEmpty();
  }

  private static boolean hasObjectiveData(CompoundTag compoundTag) {
    if (!compoundTag.contains(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG)) {
      return false;
    }

    CompoundTag objectiveData =
        compoundTag.getCompound(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG);
    return objectiveData.contains(ObjectiveDataSet.DATA_OBJECTIVE_DATA_SET_TAG)
        && !objectiveData
            .getList(ObjectiveDataSet.DATA_OBJECTIVE_DATA_SET_TAG, Tag.TAG_COMPOUND)
            .isEmpty();
  }

  private static void collectActionFeatures(
      Tag tag, PresetAuthority presetAuthority, Map<NpcFeature, PresetFeatureNotice> notices) {
    if (tag instanceof CompoundTag compoundTag) {
      if (compoundTag.contains(ActionDataEntry.DATA_TYPE_TAG)) {
        ActionDataType actionDataType =
            ActionDataType.get(compoundTag.getString(ActionDataEntry.DATA_TYPE_TAG));
        NpcFeature feature = FeatureSecurity.getFeature(actionDataType);
        if (feature != null) {
          addFeaturePresence(notices, presetAuthority, feature);
        }
      }
      for (String key : compoundTag.getAllKeys()) {
        collectActionFeatures(compoundTag.get(key), presetAuthority, notices);
      }
    } else if (tag instanceof ListTag listTag) {
      for (Tag listEntry : listTag) {
        collectActionFeatures(listEntry, presetAuthority, notices);
      }
    }
  }

  private static void addFeaturePresence(
      Map<NpcFeature, PresetFeatureNotice> notices,
      PresetAuthority presetAuthority,
      NpcFeature feature) {
    PresetFeatureStatus status =
        FeatureSecurity.isFeatureAllowed(presetAuthority, feature)
            ? PresetFeatureStatus.ALLOWED
            : PresetFeatureStatus.BLOCKED;
    putNotice(notices, new PresetFeatureNotice(feature, status));
  }

  private static void putNotice(
      Map<NpcFeature, PresetFeatureNotice> notices, PresetFeatureNotice notice) {
    PresetFeatureNotice existingNotice = notices.get(notice.feature());
    if (existingNotice == null || priority(notice.status()) > priority(existingNotice.status())) {
      notices.put(notice.feature(), notice);
    }
  }

  private static int priority(PresetFeatureStatus status) {
    return switch (status) {
      case BLOCKED -> 3;
      case REDUCED -> 2;
      case ALLOWED -> 1;
    };
  }

  private static PresetFeatureNotice toFeatureNotice(
      PresetSanitizationNotice notice, PresetAuthority presetAuthority) {
    return switch (notice) {
      case TRADING_REMOVED ->
          new PresetFeatureNotice(NpcFeature.TRADING, PresetFeatureStatus.BLOCKED);
      case COMMAND_ACTION_REMOVED ->
          new PresetFeatureNotice(NpcFeature.COMMAND_ACTION, PresetFeatureStatus.BLOCKED);
      case SCOREBOARD_ACTION_REMOVED ->
          new PresetFeatureNotice(NpcFeature.SCOREBOARD_ACTION, PresetFeatureStatus.BLOCKED);
      case BLOCK_ACTION_REMOVED ->
          new PresetFeatureNotice(NpcFeature.INTERACT_BLOCK_ACTION, PresetFeatureStatus.BLOCKED);
      case TRADING_ACTION_REMOVED ->
          new PresetFeatureNotice(NpcFeature.OPEN_TRADING_ACTION, PresetFeatureStatus.BLOCKED);
      case OBJECTIVE_REMOVED ->
          new PresetFeatureNotice(NpcFeature.OBJECTIVE, PresetFeatureStatus.BLOCKED);
      case MOVEMENT_REMOVED ->
          new PresetFeatureNotice(NpcFeature.MOVEMENT, PresetFeatureStatus.BLOCKED);
      case POSITION_REMOVED ->
          new PresetFeatureNotice(NpcFeature.POSITION, PresetFeatureStatus.BLOCKED);
      case ATTRIBUTE_REMOVED ->
          new PresetFeatureNotice(NpcFeature.MOVEMENT, PresetFeatureStatus.BLOCKED);
      case ACTION_PERMISSION_CLAMPED, COMMAND_PERMISSION_CLAMPED ->
          new PresetFeatureNotice(
              NpcFeature.COMMAND_ACTION,
              PresetFeatureStatus.REDUCED,
              presetAuthority.commandPermissionLevel());
      default -> null;
    };
  }

  private static PresetSanitizationNotice toActionRemovalNotice(ActionDataType actionDataType) {
    return switch (actionDataType) {
      case COMMAND -> PresetSanitizationNotice.COMMAND_ACTION_REMOVED;
      case SCOREBOARD -> PresetSanitizationNotice.SCOREBOARD_ACTION_REMOVED;
      case INTERACT_BLOCK -> PresetSanitizationNotice.BLOCK_ACTION_REMOVED;
      case OPEN_TRADING_SCREEN -> PresetSanitizationNotice.TRADING_ACTION_REMOVED;
      default -> null;
    };
  }
}
