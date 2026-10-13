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
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 * OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.data.action;

import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.trading.TradingDataSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;

// Execution limits are counted per action id and NPC, see ExecutionId#action.
public final class UniqueActionIdAssigner {

  private UniqueActionIdAssigner() {}

  public static void assign(
      DialogDataSet dialogDataSet, ActionEventSet actionEventSet, TradingDataSet tradingDataSet) {
    Set<UUID> usedActionIds = new HashSet<>();
    if (dialogDataSet != null) {
      assignDialogButtonActionIds(dialogDataSet, usedActionIds);
    }
    if (actionEventSet != null) {
      assignActionEventIds(actionEventSet, usedActionIds);
    }
    if (tradingDataSet != null) {
      assignTradingOfferActionIds(tradingDataSet, usedActionIds);
    }
  }

  private static void assignDialogButtonActionIds(
      DialogDataSet dialogDataSet, Set<UUID> usedActionIds) {
    for (DialogDataEntry dialogDataEntry : dialogDataSet.getDialogsByLabel()) {
      for (DialogButtonEntry dialogButtonEntry : List.copyOf(dialogDataEntry.getDialogButtons())) {
        ActionDataSet actionDataSet = dialogButtonEntry.actionDataSet();
        if (actionDataSet == null) {
          continue;
        }
        ActionDataSet uniqueActionDataSet =
            actionDataSet.withUniqueIds(
                usedActionIds, "dialog:" + dialogDataEntry.getId() + ":" + dialogButtonEntry.id());
        if (uniqueActionDataSet != actionDataSet) {
          dialogDataEntry.setDialogButton(
              dialogButtonEntry.id(), dialogButtonEntry.withActionDataSet(uniqueActionDataSet));
        }
      }
    }
  }

  private static void assignActionEventIds(ActionEventSet actionEventSet, Set<UUID> usedActionIds) {
    for (ActionEventType actionEventType : ActionEventType.values()) {
      if (!actionEventSet.hasActionEvent(actionEventType)) {
        continue;
      }
      ActionDataSet actionDataSet = actionEventSet.getActionEvents(actionEventType);
      ActionDataSet uniqueActionDataSet =
          actionDataSet.withUniqueIds(usedActionIds, "event:" + actionEventType.name());
      if (uniqueActionDataSet != actionDataSet) {
        actionEventSet.setActionEvent(actionEventType, uniqueActionDataSet);
      }
    }
  }

  private static void assignTradingOfferActionIds(
      TradingDataSet tradingDataSet, Set<UUID> usedActionIds) {
    for (Integer offerIndex : new TreeSet<>(tradingDataSet.getOfferActions().keySet())) {
      ActionDataSet actionDataSet = tradingDataSet.getOfferAction(offerIndex);
      ActionDataSet uniqueActionDataSet =
          actionDataSet.withUniqueIds(usedActionIds, "trading:" + offerIndex);
      if (uniqueActionDataSet != actionDataSet) {
        tradingDataSet.setOfferAction(offerIndex, uniqueActionDataSet);
      }
    }
  }
}
