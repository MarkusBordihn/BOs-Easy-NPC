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

import de.markusbordihn.easynpc.api.condition.ConditionRegistry;
import de.markusbordihn.easynpc.condition.ConditionManager;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.execution.ExecutionId;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.server.level.ServerPlayer;

final class DialogLockSnapshot {

  private DialogLockSnapshot() {}

  static ListTag collectLockedDialogButtons(
      DialogDataSet dialogDataSet, ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    return collectLockedButtons(
        dialogDataSet,
        (dialogId, buttonEntry) ->
            !areConditionsAvailable(
                dialogId,
                buttonEntry,
                serverPlayer,
                easyNPC,
                DialogLockSnapshot::requiresServerLockSnapshot));
  }

  static ListTag collectLockedExecutionLimits(
      DialogDataSet dialogDataSet, ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    return collectLockedButtons(
        dialogDataSet,
        (dialogId, buttonEntry) ->
            !areConditionsAvailable(
                dialogId,
                buttonEntry,
                serverPlayer,
                easyNPC,
                condition -> condition.conditionType() == ConditionType.EXECUTION_LIMIT));
  }

  private static ListTag collectLockedButtons(
      DialogDataSet dialogDataSet, DialogButtonPredicate lockedPredicate) {
    ListTag lockedButtons = new ListTag();
    for (DialogDataEntry dialogEntry : dialogDataSet.getDialogsByLabel()) {
      if (dialogEntry == null) {
        continue;
      }

      for (DialogButtonEntry buttonEntry : dialogEntry.getDialogButtons()) {
        if (buttonEntry != null
            && buttonEntry.hasConditions()
            && lockedPredicate.isLocked(dialogEntry.getId(), buttonEntry)) {
          lockedButtons.add(CompoundTagUtils.uuidToTag(buttonEntry.id()));
        }
      }
    }

    return lockedButtons;
  }

  private static boolean areConditionsAvailable(
      UUID dialogId,
      DialogButtonEntry buttonEntry,
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      ConditionPredicate relevantCondition) {
    ExecutionId executionId =
        ExecutionId.dialogButton(
            easyNPC != null ? easyNPC.getEntity() : null, dialogId, buttonEntry.id());
    for (ConditionDataEntry condition : buttonEntry.conditions()) {
      if (relevantCondition.isRelevant(condition)
          && !ConditionManager.evaluate(
              condition,
              serverPlayer,
              executionId,
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
      case SCOREBOARD, ADVANCEMENT, PLAYER_TAG, TEAM, GAMEMODE, NPC_STATE -> true;
      case CUSTOM -> !ConditionRegistry.isEvaluatedOnClient(condition.customConditionId());
      default -> false;
    };
  }

  static boolean hasNoLockableDialog(
      CompoundTag compoundTag, DialogDataSet dialogDataSet, ServerPlayer serverPlayer) {
    return compoundTag == null
        || dialogDataSet == null
        || !dialogDataSet.hasDialog()
        || serverPlayer == null;
  }

  @FunctionalInterface
  private interface DialogButtonPredicate {
    boolean isLocked(UUID dialogId, DialogButtonEntry buttonEntry);
  }

  @FunctionalInterface
  private interface ConditionPredicate {
    boolean isRelevant(ConditionDataEntry condition);
  }
}
