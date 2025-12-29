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

package de.markusbordihn.easynpc.entity.easynpc.handlers.action;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionUtils;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerPlayer;

public class ActionValidator {

  private ActionValidator() {}

  public static boolean validateActionData(
      ActionDataEntry actionDataEntry, ServerPlayer serverPlayer) {
    if (actionDataEntry == null
        || serverPlayer == null
        || !actionDataEntry.isValidAndNotEmpty()
        || serverPlayer.level().isClientSide()) {
      return false;
    }

    return ConditionUtils.evaluateConditions(
        actionDataEntry.conditionDataSet().getConditions(), serverPlayer, actionDataEntry.getId());
  }

  public static boolean validateActionDataWithoutPlayer(ActionDataEntry actionDataEntry) {
    return actionDataEntry != null && actionDataEntry.isValidAndNotEmpty();
  }

  public static boolean validateServerSide(ServerPlayer serverPlayer) {
    return serverPlayer != null && !serverPlayer.level().isClientSide();
  }

  public static boolean validateDialogData(DialogDataCapable<?> dialogData) {
    return dialogData != null && dialogData.hasDialog();
  }

  public static boolean validateNamedDialog(DialogDataCapable<?> dialogData, String dialogLabel) {
    return dialogData != null
        && dialogLabel != null
        && !dialogLabel.isEmpty()
        && dialogData.hasDialog(dialogLabel);
  }

  public static boolean validateTradingData(TradingDataCapable<?> tradingData) {
    return tradingData != null && tradingData.hasTradingData();
  }

  public static boolean validateBlockPos(BlockPos blockPos) {
    return blockPos != null && !blockPos.equals(BlockPos.ZERO);
  }

  public static boolean validateCommand(String command) {
    return command != null && !command.isEmpty();
  }
}
