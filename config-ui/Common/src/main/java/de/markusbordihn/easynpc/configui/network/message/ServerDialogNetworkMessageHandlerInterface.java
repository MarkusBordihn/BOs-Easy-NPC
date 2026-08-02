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
import de.markusbordihn.easynpc.configui.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.configui.network.message.server.OpenConditionDataEditorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenConditionDataEntryEditorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenDialogButtonEditorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenDialogEditorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenDialogOptionsEditorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenDialogTextEditorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.RemoveDialogButtonMessage;
import de.markusbordihn.easynpc.configui.network.message.server.RemoveDialogMessage;
import de.markusbordihn.easynpc.configui.network.message.server.SaveDialogButtonMessage;
import de.markusbordihn.easynpc.configui.network.message.server.SaveDialogMessage;
import de.markusbordihn.easynpc.configui.network.message.server.SaveDialogSetMessage;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import java.util.UUID;

public interface ServerDialogNetworkMessageHandlerInterface {

  default void openDialogEditor(UUID uuid, UUID dialogId) {
    if (uuid != null && dialogId != null) {
      NetworkHandlerManager.sendMessageToServer(new OpenDialogEditorMessage(uuid, dialogId));
    }
  }

  default void openDialogOptionsEditor(UUID uuid, UUID dialogId) {
    if (uuid != null && dialogId != null) {
      NetworkHandlerManager.sendMessageToServer(new OpenDialogOptionsEditorMessage(uuid, dialogId));
    }
  }

  default void openDialogTextEditor(UUID uuid, UUID dialogId) {
    if (uuid != null && dialogId != null) {
      NetworkHandlerManager.sendMessageToServer(new OpenDialogTextEditorMessage(uuid, dialogId));
    }
  }

  default void openDialogButtonEditor(UUID uuid, UUID dialogId, UUID dialogButtonId) {
    if (uuid != null && dialogId != null && dialogButtonId != null) {
      NetworkHandlerManager.sendMessageToServer(
          new OpenDialogButtonEditorMessage(uuid, dialogId, dialogButtonId));
    }
  }

  default void openConditionDataEditor(UUID uuid, UUID dialogId) {
    openConditionDataEditor(uuid, dialogId, Constants.EMPTY_UUID);
  }

  default void openConditionDataEditor(UUID uuid, UUID dialogId, UUID dialogButtonId) {
    if (uuid != null && dialogId != null) {
      NetworkHandlerManager.sendMessageToServer(
          new OpenConditionDataEditorMessage(
              uuid, dialogId, dialogButtonId != null ? dialogButtonId : Constants.EMPTY_UUID));
    }
  }

  default void openConditionDataEntryEditor(
      UUID uuid, UUID dialogId, ConditionDataEntry conditionDataEntry) {
    openConditionDataEntryEditor(uuid, dialogId, Constants.EMPTY_UUID, conditionDataEntry);
  }

  default void openConditionDataEntryEditor(
      UUID uuid, UUID dialogId, UUID dialogButtonId, ConditionDataEntry conditionDataEntry) {
    if (uuid != null && dialogId != null && conditionDataEntry != null) {
      NetworkHandlerManager.sendMessageToServer(
          new OpenConditionDataEntryEditorMessage(
              uuid,
              dialogId,
              dialogButtonId != null ? dialogButtonId : Constants.EMPTY_UUID,
              conditionDataEntry));
    }
  }

  default void saveDialogButton(
      UUID uuid, UUID dialogId, UUID dialogButtonId, DialogButtonEntry dialogButtonEntry) {
    if (uuid != null && dialogId != null && dialogButtonId != null && dialogButtonEntry != null) {
      NetworkHandlerManager.sendMessageToServer(
          new SaveDialogButtonMessage(uuid, dialogId, dialogButtonId, dialogButtonEntry));
    }
  }

  default void removeDialog(UUID uuid, UUID dialogId) {
    if (uuid != null && dialogId != null) {
      NetworkHandlerManager.sendMessageToServer(new RemoveDialogMessage(uuid, dialogId));
    }
  }

  default void removeDialogButton(UUID uuid, UUID dialogId, UUID dialogButtonId) {
    if (uuid != null && dialogId != null && dialogButtonId != null) {
      NetworkHandlerManager.sendMessageToServer(
          new RemoveDialogButtonMessage(uuid, dialogId, dialogButtonId));
    }
  }

  default void saveDialogSet(UUID uuid, DialogDataSet dialogDataSet) {
    if (uuid != null && dialogDataSet != null) {
      NetworkHandlerManager.sendMessageToServer(new SaveDialogSetMessage(uuid, dialogDataSet));
    }
  }

  default void saveDialog(UUID uuid, UUID dialogId, DialogDataEntry dialogData) {
    if (uuid != null && dialogId != null && dialogData != null) {
      NetworkHandlerManager.sendMessageToServer(new SaveDialogMessage(uuid, dialogId, dialogData));
    }
  }

  default void openDialogEditor(UUID uuid) {
    openDialogEditor(uuid, Constants.EMPTY_UUID);
  }

  default void openDialogButtonEditor(UUID uuid, UUID dialogId) {
    openDialogButtonEditor(uuid, dialogId, Constants.EMPTY_UUID);
  }
}
