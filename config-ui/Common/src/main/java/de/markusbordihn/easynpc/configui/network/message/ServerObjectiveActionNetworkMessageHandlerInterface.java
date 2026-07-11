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

package de.markusbordihn.easynpc.configui.network.message;

import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.data.editor.EditorType;
import de.markusbordihn.easynpc.configui.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.configui.network.message.server.AddOrUpdateObjectiveMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeActionEventMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeTradingOfferActionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenActionConditionDataEditorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenActionConditionDataEntryEditorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenActionDataEditorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenActionDataEntryEditorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.RemoveObjectiveMessage;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import java.util.UUID;

public interface ServerObjectiveActionNetworkMessageHandlerInterface {

  default void actionEventChange(
      UUID uuid, ActionEventType actionEventType, ActionDataSet actionDataSet) {
    if (uuid != null && actionEventType != null && actionDataSet != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeActionEventMessage(uuid, actionEventType, actionDataSet));
    }
  }

  default void addOrUpdateObjective(UUID uuid, ObjectiveDataEntry objectiveDataEntry) {
    if (uuid != null && objectiveDataEntry != null) {
      NetworkHandlerManager.sendMessageToServer(
          new AddOrUpdateObjectiveMessage(uuid, objectiveDataEntry));
    }
  }

  default void openActionDataEditor(
      UUID uuid, ActionEventType actionEventType, ConfigurationType configurationType) {
    if (uuid != null && actionEventType != null && actionEventType != ActionEventType.NONE) {
      NetworkHandlerManager.sendMessageToServer(
          new OpenActionDataEditorMessage(
              uuid,
              Constants.EMPTY_UUID,
              Constants.EMPTY_UUID,
              actionEventType,
              configurationType,
              EditorType.NONE,
              0));
    }
  }

  default void openActionDataEditor(
      UUID uuid, EditorType editorType, UUID dialogId, UUID dialogButtonId) {
    if (uuid != null && editorType != null && dialogId != null && dialogButtonId != null) {
      NetworkHandlerManager.sendMessageToServer(
          new OpenActionDataEditorMessage(
              uuid,
              dialogId,
              dialogButtonId,
              ActionEventType.NONE,
              ConfigurationType.NONE,
              editorType,
              0));
    }
  }

  default void openActionDataEntryEditor(
      UUID uuid,
      EditorType editorType,
      UUID dialogId,
      UUID dialogButtonId,
      ActionDataEntry actionDataEntry) {
    if (uuid != null
        && editorType != null
        && dialogId != null
        && dialogButtonId != null
        && actionDataEntry != null) {
      NetworkHandlerManager.sendMessageToServer(
          new OpenActionDataEntryEditorMessage(
              uuid,
              dialogId,
              dialogButtonId,
              actionDataEntry.id(),
              ActionEventType.NONE,
              ConfigurationType.NONE,
              editorType,
              0));
    }
  }

  default void openActionDataEntryEditor(
      UUID uuid,
      ActionEventType actionEventType,
      ConfigurationType configurationType,
      ActionDataEntry actionDataEntry) {
    if (uuid != null && actionEventType != null && actionDataEntry != null) {
      NetworkHandlerManager.sendMessageToServer(
          new OpenActionDataEntryEditorMessage(
              uuid,
              Constants.EMPTY_UUID,
              Constants.EMPTY_UUID,
              actionDataEntry.id(),
              actionEventType,
              configurationType,
              EditorType.NONE,
              0));
    }
  }

  default void openTradingOfferActionEditor(
      UUID uuid, int offerIndex, ConfigurationType configurationType) {
    if (uuid != null && offerIndex >= 0) {
      NetworkHandlerManager.sendMessageToServer(
          new OpenActionDataEditorMessage(
              uuid,
              Constants.EMPTY_UUID,
              Constants.EMPTY_UUID,
              ActionEventType.NONE,
              configurationType,
              EditorType.TRADING_OFFER_ACTION,
              offerIndex));
    }
  }

  default void openTradingOfferActionEntryEditor(
      UUID uuid,
      int offerIndex,
      ConfigurationType configurationType,
      ActionDataEntry actionDataEntry) {
    if (uuid != null && offerIndex >= 0 && actionDataEntry != null) {
      NetworkHandlerManager.sendMessageToServer(
          new OpenActionDataEntryEditorMessage(
              uuid,
              Constants.EMPTY_UUID,
              Constants.EMPTY_UUID,
              actionDataEntry.id(),
              ActionEventType.NONE,
              configurationType,
              EditorType.TRADING_OFFER_ACTION,
              offerIndex));
    }
  }

  default void changeTradingOfferAction(UUID uuid, int offerIndex, ActionDataSet actionDataSet) {
    if (uuid != null && offerIndex >= 0 && actionDataSet != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeTradingOfferActionMessage(uuid, offerIndex, actionDataSet));
    }
  }

  default void openActionConditionDataEditor(
      UUID uuid,
      UUID actionDataEntryId,
      ActionEventType actionEventType,
      ConfigurationType configurationType,
      EditorType editorType,
      UUID dialogId,
      UUID dialogButtonId,
      int contextIndex) {
    if (uuid != null && actionDataEntryId != null) {
      NetworkHandlerManager.sendMessageToServer(
          new OpenActionConditionDataEditorMessage(
              uuid,
              actionDataEntryId,
              actionEventType != null ? actionEventType : ActionEventType.NONE,
              configurationType != null ? configurationType : ConfigurationType.NONE,
              editorType != null ? editorType : EditorType.NONE,
              dialogId != null ? dialogId : Constants.EMPTY_UUID,
              dialogButtonId != null ? dialogButtonId : Constants.EMPTY_UUID,
              contextIndex));
    }
  }

  default void openActionConditionDataEntryEditor(
      UUID uuid,
      UUID actionDataEntryId,
      ActionEventType actionEventType,
      ConfigurationType configurationType,
      EditorType editorType,
      UUID dialogId,
      UUID dialogButtonId,
      int contextIndex,
      ConditionDataEntry conditionDataEntry) {
    if (uuid != null && actionDataEntryId != null && conditionDataEntry != null) {
      NetworkHandlerManager.sendMessageToServer(
          new OpenActionConditionDataEntryEditorMessage(
              uuid,
              actionDataEntryId,
              actionEventType != null ? actionEventType : ActionEventType.NONE,
              configurationType != null ? configurationType : ConfigurationType.NONE,
              editorType != null ? editorType : EditorType.NONE,
              dialogId != null ? dialogId : Constants.EMPTY_UUID,
              dialogButtonId != null ? dialogButtonId : Constants.EMPTY_UUID,
              contextIndex,
              conditionDataEntry));
    }
  }

  default void removeObjective(UUID uuid, ObjectiveDataEntry objectiveDataEntry) {
    if (uuid != null && objectiveDataEntry != null) {
      NetworkHandlerManager.sendMessageToServer(
          new RemoveObjectiveMessage(uuid, objectiveDataEntry));
    }
  }
}
