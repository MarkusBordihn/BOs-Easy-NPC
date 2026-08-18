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

package de.markusbordihn.easynpc.api.preset;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.CustomActionCommand;
import de.markusbordihn.easynpc.data.action.MessageActionData;
import de.markusbordihn.easynpc.data.action.ModelAnimationActionData;
import de.markusbordihn.easynpc.data.action.MoveActionData;
import de.markusbordihn.easynpc.data.action.SoundActionData;
import de.markusbordihn.easynpc.data.action.WaitDuration;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import de.markusbordihn.easynpc.utils.ValueUtils;
import java.util.List;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.Identifier;

final class PresetActionValidator {

  private PresetActionValidator() {}

  static void validateActionData(
      CompoundTag entityData,
      String path,
      PresetValidationContext context,
      List<PresetValidationIssue> issues) {
    if (!entityData.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG)) {
      return;
    }

    CompoundTag actionEventSet =
        entityData
            .getCompoundOrEmpty(ActionEventDataCapable.DATA_ACTION_DATA_TAG)
            .getCompoundOrEmpty(ActionEventSet.DATA_ACTION_EVENT_SET_TAG);
    String actionPath =
        PresetValidationSupport.childPath(path, ActionEventDataCapable.DATA_ACTION_DATA_TAG)
            + "/"
            + ActionEventSet.DATA_ACTION_EVENT_SET_TAG;

    for (String actionEventName : actionEventSet.keySet()) {
      ListTag actionEntries = actionEventSet.getListOrEmpty(actionEventName);
      for (int index = 0; index < actionEntries.size(); index++) {
        validateActionEntry(
            actionEntries.getCompoundOrEmpty(index),
            actionPath + "/" + actionEventName + "[" + index + "]",
            context,
            issues);
      }
    }
  }

  static void validateActionEntry(
      CompoundTag actionEntry,
      String path,
      PresetValidationContext context,
      List<PresetValidationIssue> issues) {
    String actionTypeName = actionEntry.getStringOr(ActionDataEntry.DATA_TYPE_TAG, "");
    ActionDataType actionDataType = ActionDataType.get(actionTypeName);

    if (actionDataType == null || actionDataType == ActionDataType.NONE) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.UNKNOWN_ACTION_TYPE,
              path,
              "The action type '" + actionTypeName + "' is not known"));
      return;
    }

    if (actionDataType == ActionDataType.MESSAGE) {
      validateMessageAction(actionEntry, path, issues);
    } else if (actionDataType == ActionDataType.SET_POSE) {
      if (Identifier.tryParse(actionEntry.getStringOr(ActionDataEntry.DATA_POSE_TAG, "")) == null) {
        issues.add(
            PresetValidationIssue.error(
                PresetValidationRule.POSE_ACTION_WITHOUT_POSE,
                path,
                "The pose action needs a valid pose id"));
      }
    } else if (actionDataType == ActionDataType.PLAY_ANIMATION) {
      CompoundTag animationTag = actionEntry.getCompoundOrEmpty(ActionDataEntry.DATA_ANIMATION_TAG);
      if (animationTag.getStringOr(ModelAnimationActionData.DATA_NAME_TAG, "").isBlank()) {
        issues.add(
            PresetValidationIssue.error(
                PresetValidationRule.ANIMATION_ACTION_WITHOUT_NAME,
                path,
                "The animation action needs an animation name"));
      }
      validateAnimationBlend(animationTag, path, issues);
    } else if (actionDataType == ActionDataType.STOP_ANIMATION) {
      validateAnimationBlend(
          actionEntry.getCompoundOrEmpty(ActionDataEntry.DATA_ANIMATION_TAG), path, issues);
    } else if (actionDataType == ActionDataType.SOUND) {
      if (!SoundActionData.fromTag(actionEntry.getCompoundOrEmpty(ActionDataEntry.DATA_SOUND_TAG))
          .hasSoundId()) {
        issues.add(
            PresetValidationIssue.error(
                PresetValidationRule.SOUND_ACTION_WITHOUT_SOUND,
                path,
                "The sound action needs a valid sound id"));
      }
    } else if (actionDataType == ActionDataType.MOVE_TO
        || actionDataType == ActionDataType.MOVE_TO_AND_WAIT) {
      validateMoveAction(actionEntry, path, issues);
    } else if (actionDataType == ActionDataType.SET_OPACITY) {
      validateOpacityValue(
          actionEntry.getStringOr(ActionDataEntry.DATA_COMMAND_TAG, ""), path, issues);
    } else if (actionDataType.requiresArgument()
        && actionEntry.getStringOr(ActionDataEntry.DATA_COMMAND_TAG, "").isBlank()
        && !actionEntry.contains(ActionDataEntry.DATA_BLOCK_POS_TAG)) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.COMMAND_ACTION_WITHOUT_COMMAND,
              path,
              "The action " + actionDataType + " needs a command or a position"));
    }

    if (actionDataType == ActionDataType.CUSTOM) {
      validateCustomActionId(
          actionEntry.getStringOr(ActionDataEntry.DATA_COMMAND_TAG, ""), path, context, issues);
    }

    if (actionDataType == ActionDataType.WAIT) {
      validateWaitDuration(
          actionEntry.getStringOr(ActionDataEntry.DATA_COMMAND_TAG, ""), path, issues);
    }

    if (actionDataType != ActionDataType.MESSAGE) {
      PresetValidationSupport.validateText(
          actionEntry.getStringOr(ActionDataEntry.DATA_COMMAND_TAG, ""), path, issues);
    }
    PresetValidationSupport.validateConditionList(
        actionEntry
            .getCompoundOrEmpty(ConditionDataSet.CONDITION_DATA_SET_TAG)
            .getListOrEmpty(ConditionDataSet.CONDITION_DATA_SET_TAG),
        PresetValidationSupport.childPath(path, ConditionDataSet.CONDITION_DATA_SET_TAG),
        context,
        issues);
  }

  private static void validateAnimationBlend(
      CompoundTag animationTag, String path, List<PresetValidationIssue> issues) {
    if (!animationTag.contains(ModelAnimationActionData.DATA_BLEND_TAG)) {
      return;
    }

    float blend = animationTag.getFloatOr(ModelAnimationActionData.DATA_BLEND_TAG, 0.0F);
    if (!Float.isFinite(blend) || blend < 0.0F) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.ANIMATION_BLEND_OUT_OF_RANGE,
              path,
              "Animation blend ticks must be a finite non-negative value"));
    }
  }

  private static void validateMessageAction(
      CompoundTag actionEntry, String path, List<PresetValidationIssue> issues) {
    MessageActionData messageActionData =
        MessageActionData.fromTag(actionEntry.getCompoundOrEmpty(ActionDataEntry.DATA_MESSAGE_TAG));
    if (!messageActionData.hasTexts()) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.MESSAGE_ACTION_WITHOUT_TEXT,
              path,
              "The message action needs at least one text"));
      return;
    }

    for (int index = 0; index < messageActionData.texts().size(); index++) {
      PresetValidationSupport.validateText(
          messageActionData.texts().get(index),
          PresetValidationSupport.childPath(
              path, ActionDataEntry.DATA_MESSAGE_TAG + "/Texts[" + index + "]"),
          issues);
    }
  }

  private static void validateMoveAction(
      CompoundTag actionEntry, String path, List<PresetValidationIssue> issues) {
    CompoundTag moveTag = actionEntry.getCompoundOrEmpty(ActionDataEntry.DATA_MOVE_TAG);
    MoveActionData moveActionData = MoveActionData.fromTag(moveTag);
    BlockPos blockPos =
        actionEntry.contains(ActionDataEntry.DATA_BLOCK_POS_TAG)
            ? CompoundTagUtils.readBlockPos(
                actionEntry.getCompoundOrEmpty(ActionDataEntry.DATA_BLOCK_POS_TAG))
            : null;
    if (!moveActionData.hasResolvableTarget(blockPos)) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.MOVE_ACTION_WITHOUT_POSITION,
              path,
              "The move action with the target "
                  + moveActionData.targetType()
                  + " needs a position"));
    }

    if (!moveTag.contains(MoveActionData.DATA_TIMEOUT_TAG)) {
      return;
    }

    int timeoutTicks =
        moveTag.getIntOr(MoveActionData.DATA_TIMEOUT_TAG, MoveActionData.DEFAULT_TIMEOUT_TICKS);
    if (timeoutTicks < MoveActionData.MIN_TIMEOUT_TICKS
        || timeoutTicks > MoveActionData.MAX_TIMEOUT_TICKS) {
      issues.add(
          PresetValidationIssue.warning(
              PresetValidationRule.MOVE_ACTION_TIMEOUT_OUT_OF_RANGE,
              path,
              "The move timeout of "
                  + timeoutTicks
                  + " ticks is limited to "
                  + MoveActionData.MIN_TIMEOUT_TICKS
                  + " - "
                  + MoveActionData.MAX_TIMEOUT_TICKS
                  + " ticks"));
    }
  }

  private static void validateOpacityValue(
      String command, String path, List<PresetValidationIssue> issues) {
    if (!ValueUtils.isNumericValue(
        command, DisplayAttributeType.MIN_OPACITY, DisplayAttributeType.MAX_OPACITY)) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.OPACITY_ACTION_WITH_INVALID_VALUE,
              path,
              "The opacity '"
                  + command
                  + "' is not a value between "
                  + DisplayAttributeType.MIN_OPACITY
                  + " and "
                  + DisplayAttributeType.MAX_OPACITY));
    }
  }

  private static void validateWaitDuration(
      String command, String path, List<PresetValidationIssue> issues) {
    if (command.isBlank()) {
      return;
    }

    long ticks = WaitDuration.parseUnclampedTicks(command);
    if (ticks == WaitDuration.INVALID_TICKS) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.WAIT_ACTION_WITH_INVALID_DURATION,
              path,
              "The wait duration '" + command + "' is not a duration like '20s', '400t' or '5m'"));
      return;
    }

    if (ticks > WaitDuration.MAX_TICKS) {
      issues.add(
          PresetValidationIssue.warning(
              PresetValidationRule.WAIT_ACTION_DURATION_OUT_OF_RANGE,
              path,
              "The wait duration '"
                  + command
                  + "' is shortened to "
                  + WaitDuration.MAX_TICKS
                  + " ticks"));
    }
  }

  private static void validateCustomActionId(
      String command,
      String path,
      PresetValidationContext context,
      List<PresetValidationIssue> issues) {
    CustomActionCommand customActionCommand = CustomActionCommand.parse(command);
    if (!customActionCommand.isValid()) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.MALFORMED_CUSTOM_ACTION_ID,
              path,
              "The custom action '" + command + "' does not start with a valid identifier"));
      return;
    }

    if (!context.knownCustomActions().test(customActionCommand.actionId())) {
      issues.add(
          PresetValidationIssue.warning(
              PresetValidationRule.UNKNOWN_CUSTOM_ACTION_ID,
              path,
              "The custom action '"
                  + customActionCommand.actionId()
                  + "' is not registered, so the action is skipped"));
    }
  }
}
