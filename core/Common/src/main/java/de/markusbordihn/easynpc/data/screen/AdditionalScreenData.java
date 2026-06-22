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

package de.markusbordihn.easynpc.data.screen;

import de.markusbordihn.easynpc.condition.ConditionManager;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogTextData;
import de.markusbordihn.easynpc.data.scoreboard.ScoreboardData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.server.level.ServerPlayer;

public class AdditionalScreenData implements AdditionalScreenDataInterface {

  private static final String ACTION_EVENT_DATA_TAG = "ActionEventData";
  private static final String ACTION_EVENT_TYPE_TAG = "ActionEventType";
  private static final String DIALOG_DATA_TAG = "DialogData";
  private static final String SCOREBOARD_DATA_TAG = "ScoreboardData";
  private static final String DIALOG_BUTTON_LOCK_DATA_TAG = "DialogButtonLockData";
  private static final String EXECUTION_LIMIT_DATA_TAG = "ExecutionLimitData";

  private final ActionEventSet actionEventSet;
  private final ActionEventType actionEventType;
  private final CompoundTag data;
  private final DialogDataSet dialogDataSet;
  private final ScoreboardData scoreboardData;
  private final Set<UUID> lockedDialogButtons;
  private final Set<UUID> lockedExecutionLimitActions;

  public AdditionalScreenData(CompoundTag compoundTag) {
    // Processing know data.
    this.actionEventSet = getActionEventSet(compoundTag);
    this.actionEventType = getActionEventType(compoundTag);
    this.dialogDataSet = getDialogDataSet(compoundTag);
    this.scoreboardData = getScoreboardData(compoundTag);
    this.lockedDialogButtons = getLockedDialogButtons(compoundTag);
    this.lockedExecutionLimitActions = getLockedExecutionLimitActions(compoundTag);

    // Store remaining data and remove already processed data.
    this.data = compoundTag;
    this.data.remove(ACTION_EVENT_DATA_TAG);
    this.data.remove(ACTION_EVENT_TYPE_TAG);
    this.data.remove(DIALOG_DATA_TAG);
    this.data.remove(SCOREBOARD_DATA_TAG);
    this.data.remove(DIALOG_BUTTON_LOCK_DATA_TAG);
    this.data.remove(EXECUTION_LIMIT_DATA_TAG);
  }

  public static void addActionEventType(CompoundTag compoundTag, ActionEventType actionEventType) {
    if (compoundTag == null || actionEventType == null) {
      return;
    }
    compoundTag.putString(ACTION_EVENT_TYPE_TAG, actionEventType.name());
  }

  public static ActionEventType getActionEventType(CompoundTag compoundTag) {
    if (!hasActionEventType(compoundTag)) {
      return ActionEventType.NONE;
    }
    return ActionEventType.get(compoundTag.getString(ACTION_EVENT_TYPE_TAG).orElse(""));
  }

  public static boolean hasActionEventType(CompoundTag compoundTag) {
    return compoundTag != null && compoundTag.contains(ACTION_EVENT_TYPE_TAG);
  }

  public static void addActionEventSet(CompoundTag compoundTag, EasyNPC<?> easyNPC) {
    if (compoundTag == null || easyNPC == null || easyNPC.getEasyNPCActionEventData() == null) {
      return;
    }
    compoundTag.put(
        ACTION_EVENT_DATA_TAG, easyNPC.getEasyNPCActionEventData().getActionEventSet().createTag());
  }

  public static ActionEventSet getActionEventSet(CompoundTag compoundTag) {
    if (!hasActionEventSet(compoundTag)) {
      return new ActionEventSet();
    }
    return new ActionEventSet(compoundTag.getCompoundOrEmpty(ACTION_EVENT_DATA_TAG));
  }

  public static boolean hasActionEventSet(CompoundTag compoundTag) {
    return compoundTag != null && compoundTag.contains(ACTION_EVENT_DATA_TAG);
  }

  public static void addDialogDataSet(CompoundTag compoundTag, EasyNPC<?> easyNPC) {
    if (compoundTag == null || easyNPC == null || easyNPC.getEasyNPCDialogData() == null) {
      return;
    }
    compoundTag.put(DIALOG_DATA_TAG, easyNPC.getEasyNPCDialogData().getDialogDataSet().createTag());
  }

  public static DialogDataSet getDialogDataSet(CompoundTag compoundTag) {
    if (!hasDialogDataSet(compoundTag)) {
      return new DialogDataSet();
    }
    return new DialogDataSet(compoundTag.getCompoundOrEmpty(DIALOG_DATA_TAG));
  }

  public static boolean hasDialogDataSet(CompoundTag compoundTag) {
    return compoundTag != null && compoundTag.contains(DIALOG_DATA_TAG);
  }

  public static void addScoreboardData(CompoundTag compoundTag, ScoreboardData scoreboardData) {
    if (compoundTag == null || scoreboardData == null) {
      return;
    }
    compoundTag.put(SCOREBOARD_DATA_TAG, scoreboardData.createTag());
  }

  public static ScoreboardData getScoreboardData(CompoundTag compoundTag) {
    if (!hasScoreboardData(compoundTag)) {
      return new ScoreboardData();
    }
    return new ScoreboardData(compoundTag.getCompoundOrEmpty(SCOREBOARD_DATA_TAG));
  }

  public static boolean hasScoreboardData(CompoundTag compoundTag) {
    return compoundTag != null && compoundTag.contains(SCOREBOARD_DATA_TAG);
  }

  public static void addDialogDataSet(
      CompoundTag compoundTag, EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    if (compoundTag == null || easyNPC == null || easyNPC.getEasyNPCDialogData() == null) {
      return;
    }

    DialogDataSet dialogDataSet = easyNPC.getEasyNPCDialogData().getDialogDataSet();
    compoundTag.put(DIALOG_DATA_TAG, dialogDataSet.createTag());
    if (serverPlayer != null) {
      Set<String> objectiveNames = extractObjectiveNamesFromDialogDataSet(dialogDataSet);
      if (!objectiveNames.isEmpty()) {
        ScoreboardData scoreboardData = new ScoreboardData(serverPlayer, objectiveNames);
        addScoreboardData(compoundTag, scoreboardData);
      }
      addDialogButtonLockData(compoundTag, dialogDataSet, serverPlayer, easyNPC);
      addExecutionLimitData(compoundTag, dialogDataSet, serverPlayer);
    }
  }

  public static void addDialogButtonLockData(
      CompoundTag compoundTag,
      DialogDataSet dialogDataSet,
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC) {
    if (compoundTag == null
        || dialogDataSet == null
        || !dialogDataSet.hasDialog()
        || serverPlayer == null) {
      return;
    }

    ListTag lockedButtons = new ListTag();
    for (DialogDataEntry dialogEntry : dialogDataSet.getDialogsByLabel()) {
      if (dialogEntry == null) {
        continue;
      }
      for (DialogButtonEntry buttonEntry : dialogEntry.getDialogButtons()) {
        if (buttonEntry == null
            || !buttonEntry.hasConditions()
            || areServerOnlyConditionsAvailable(buttonEntry, serverPlayer, easyNPC)) {
          continue;
        }
        lockedButtons.add(CompoundTagUtils.uuidToTag(buttonEntry.id()));
      }
    }

    if (!lockedButtons.isEmpty()) {
      compoundTag.put(DIALOG_BUTTON_LOCK_DATA_TAG, lockedButtons);
    }
  }

  private static boolean areServerOnlyConditionsAvailable(
      DialogButtonEntry buttonEntry, ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    for (ConditionDataEntry condition : buttonEntry.conditions()) {
      if (requiresServerLockSnapshot(condition)
          && !ConditionManager.evaluate(
              condition,
              serverPlayer,
              buttonEntry.id(),
              easyNPC != null ? easyNPC.getLivingEntity() : null)) {
        return false;
      }
    }
    return true;
  }

  private static boolean requiresServerLockSnapshot(ConditionDataEntry condition) {
    if (condition == null) {
      return false;
    }
    return switch (condition.conditionType()) {
      case SCOREBOARD, ADVANCEMENT, PLAYER_TAG, TEAM, GAMEMODE -> true;
      default -> false;
    };
  }

  public static void addExecutionLimitData(
      CompoundTag compoundTag, DialogDataSet dialogDataSet, ServerPlayer serverPlayer) {
    if (compoundTag == null
        || dialogDataSet == null
        || !dialogDataSet.hasDialog()
        || serverPlayer == null) {
      return;
    }

    ListTag lockedActions = new ListTag();
    for (DialogDataEntry dialogEntry : dialogDataSet.getDialogsByLabel()) {
      if (dialogEntry == null) {
        continue;
      }
      for (DialogButtonEntry buttonEntry : dialogEntry.getDialogButtons()) {
        if (buttonEntry == null
            || !buttonEntry.hasConditions()
            || isExecutionLimitAvailable(buttonEntry, serverPlayer)) {
          continue;
        }
        lockedActions.add(CompoundTagUtils.uuidToTag(buttonEntry.id()));
      }
    }

    if (!lockedActions.isEmpty()) {
      compoundTag.put(EXECUTION_LIMIT_DATA_TAG, lockedActions);
    }
  }

  private static boolean isExecutionLimitAvailable(
      DialogButtonEntry buttonEntry, ServerPlayer serverPlayer) {
    for (ConditionDataEntry condition : buttonEntry.conditions()) {
      if (condition.conditionType() == ConditionType.EXECUTION_LIMIT
          && !ConditionManager.evaluate(condition, serverPlayer, buttonEntry.id())) {
        return false;
      }
    }
    return true;
  }

  public static Set<UUID> getLockedExecutionLimitActions(CompoundTag compoundTag) {
    return getLockedActions(compoundTag, EXECUTION_LIMIT_DATA_TAG);
  }

  public static Set<UUID> getLockedDialogButtons(CompoundTag compoundTag) {
    return getLockedActions(compoundTag, DIALOG_BUTTON_LOCK_DATA_TAG);
  }

  private static Set<UUID> getLockedActions(CompoundTag compoundTag, String dataTag) {
    Set<UUID> lockedActions = new HashSet<>();
    if (compoundTag != null && compoundTag.contains(dataTag)) {
      ListTag listTag = compoundTag.getListOrEmpty(dataTag);
      for (Tag tag : listTag) {
        UUID uuid = CompoundTagUtils.tagToUUID(tag);
        if (uuid != null) {
          lockedActions.add(uuid);
        }
      }
    }
    return lockedActions;
  }

  private static Set<String> extractObjectiveNamesFromDialogDataSet(DialogDataSet dialogDataSet) {
    Set<String> objectiveNames = new HashSet<>();
    if (dialogDataSet == null || !dialogDataSet.hasDialog()) {
      return objectiveNames;
    }

    for (DialogDataEntry dialogEntry : dialogDataSet.getDialogsByLabel()) {
      if (dialogEntry == null) {
        continue;
      }
      if (dialogEntry.getDialogTexts() != null) {
        for (DialogTextData dialogTextData : dialogEntry.getDialogTexts()) {
          if (dialogTextData != null && dialogTextData.text() != null) {
            objectiveNames.addAll(ScoreboardData.parseScoreMacros(dialogTextData.text()));
          }
        }
      }
      for (DialogButtonEntry buttonEntry : dialogEntry.getDialogButtons()) {
        if (buttonEntry == null || !buttonEntry.hasConditions()) {
          continue;
        }
        for (ConditionDataEntry condition : buttonEntry.conditions()) {
          if (condition != null
              && condition.conditionType() == ConditionType.SCOREBOARD
              && condition.hasName()) {
            objectiveNames.add(condition.name());
          }
        }
      }
    }

    return objectiveNames;
  }

  public ActionEventType getActionEventType() {
    return this.actionEventType;
  }

  public ActionEventSet getActionEventSet() {
    return this.actionEventSet;
  }

  public DialogDataSet getDialogDataSet() {
    return this.dialogDataSet;
  }

  public CompoundTag getData() {
    return this.data;
  }

  public CompoundTag get(String dataTag) {
    if (this.data.contains(dataTag)) {
      return this.data.getCompoundOrEmpty(dataTag);
    }
    return new CompoundTag();
  }

  public ListTag getList(String dataTag) {
    if (this.data.contains(dataTag)) {
      return this.data.getListOrEmpty(dataTag);
    }
    return new ListTag();
  }

  public boolean hasDialogDataSet() {
    return this.dialogDataSet != null;
  }

  public ScoreboardData getScoreboardData() {
    return this.scoreboardData;
  }

  public Set<UUID> getLockedExecutionLimitActions() {
    return this.lockedExecutionLimitActions;
  }

  public boolean isDialogButtonLocked(UUID buttonId) {
    return buttonId != null && this.lockedDialogButtons.contains(buttonId);
  }

  public boolean isExecutionLimitReached(UUID actionId) {
    return actionId != null && this.lockedExecutionLimitActions.contains(actionId);
  }
}
