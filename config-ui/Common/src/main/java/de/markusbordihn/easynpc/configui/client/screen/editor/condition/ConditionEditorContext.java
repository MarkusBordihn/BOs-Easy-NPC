/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.client.screen.editor.condition;

import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.EditorScreen;
import de.markusbordihn.easynpc.configui.data.editor.EditorType;
import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import java.util.UUID;

public abstract class ConditionEditorContext {

  protected final EditorScreen<?> screen;

  protected ConditionEditorContext(EditorScreen<?> screen) {
    this.screen = screen;
  }

  public static ConditionEditorContext resolve(EditorScreen<?> screen) {
    UUID actionDataEntryId = screen.getActionDataEntryUUID();
    if (actionDataEntryId != null && !Constants.EMPTY_UUID.equals(actionDataEntryId)) {
      return new ActionContext(screen);
    }

    UUID dialogButtonId = screen.getDialogButtonUUID();
    if (dialogButtonId != null && !Constants.EMPTY_UUID.equals(dialogButtonId)) {
      return new DialogButtonContext(screen);
    }

    return new DialogContext(screen);
  }

  public abstract ConditionDataSet loadConditionDataSet();

  public abstract void saveConditionDataSet(ConditionDataSet conditionDataSet);

  public abstract void openParentEditor();

  public abstract void openConditionListEditor();

  public abstract void openConditionEntryEditor(ConditionDataEntry conditionDataEntry);

  public abstract String breadcrumbLabel();

  public abstract String helpTextKey();

  public boolean isActionContext() {
    return false;
  }

  private static final class ActionContext extends ConditionEditorContext {

    private ActionContext(EditorScreen<?> screen) {
      super(screen);
    }

    private ActionDataEntry findActionDataEntry() {
      UUID actionDataEntryId = this.screen.getActionDataEntryUUID();
      ActionDataSet actionDataSet = getActionDataSet();
      if (actionDataEntryId == null || actionDataSet == null) {
        return null;
      }
      return actionDataSet.getEntryOrDefault(actionDataEntryId);
    }

    private ActionDataSet getActionDataSet() {
      AdditionalScreenData screenData = this.screen.getAdditionalScreenData();
      EditorType formerEditorType = screenData.getEditorType();
      ActionEventType actionEventType = screenData.getActionEventType();

      if (formerEditorType == EditorType.TRADING_OFFER_ACTION) {
        return screenData.getTradingOfferActionDataSet();
      } else if (formerEditorType == EditorType.DIALOG_BUTTON) {
        return this.screen.getDialogButtonData() != null
            ? this.screen.getDialogButtonData().actionDataSet()
            : null;
      } else if (actionEventType != null && actionEventType != ActionEventType.NONE) {
        return screenData.getActionEventSet().getActionEvents(actionEventType);
      }
      return null;
    }

    @Override
    public ConditionDataSet loadConditionDataSet() {
      ActionDataEntry entry = findActionDataEntry();
      return entry != null ? entry.conditionDataSet() : new ConditionDataSet();
    }

    @Override
    public void saveConditionDataSet(ConditionDataSet conditionDataSet) {
      ActionDataEntry currentEntry = findActionDataEntry();
      if (currentEntry == null) {
        return;
      }
      ActionDataSet actionDataSet = getActionDataSet();
      if (actionDataSet != null) {
        actionDataSet.put(
            this.screen.getActionDataEntryUUID(),
            currentEntry.withConditionDataSet(conditionDataSet));
      }

      AdditionalScreenData screenData = this.screen.getAdditionalScreenData();
      EditorType formerEditorType = screenData.getEditorType();
      ActionEventType actionEventType = screenData.getActionEventType();

      if (formerEditorType == EditorType.TRADING_OFFER_ACTION) {
        NetworkMessageHandlerManager.getServerHandler()
            .changeTradingOfferAction(
                this.screen.getEasyNPCUUID(), this.screen.getPageIndex(), actionDataSet);
      } else if (formerEditorType == EditorType.DIALOG_BUTTON) {
        if (this.screen.getDialogButtonData() != null) {
          NetworkMessageHandlerManager.getServerHandler()
              .saveDialogButton(
                  this.screen.getEasyNPCUUID(),
                  this.screen.getDialogUUID(),
                  this.screen.getDialogButtonUUID(),
                  this.screen.getDialogButtonData().withActionDataSet(actionDataSet));
        }
      } else if (actionEventType != null && actionEventType != ActionEventType.NONE) {
        NetworkMessageHandlerManager.getServerHandler()
            .actionEventChange(this.screen.getEasyNPCUUID(), actionEventType, actionDataSet);
      }
    }

    @Override
    public void openParentEditor() {
      AdditionalScreenData screenData = this.screen.getAdditionalScreenData();
      EditorType formerEditorType = screenData.getEditorType();
      ActionEventType actionEventType = screenData.getActionEventType();
      ConfigurationType configurationType = screenData.getConfigurationType();
      ActionDataEntry entry = findActionDataEntry();
      ActionDataEntry actionDataEntry = entry != null ? entry : new ActionDataEntry();

      if (formerEditorType == EditorType.TRADING_OFFER_ACTION) {
        NetworkMessageHandlerManager.getServerHandler()
            .openTradingOfferActionEntryEditor(
                this.screen.getEasyNPCUUID(),
                this.screen.getPageIndex(),
                configurationType,
                actionDataEntry);
      } else if (formerEditorType == EditorType.DIALOG_BUTTON) {
        NetworkMessageHandlerManager.getServerHandler()
            .openActionDataEntryEditor(
                this.screen.getEasyNPCUUID(),
                formerEditorType,
                this.screen.getDialogUUID(),
                this.screen.getDialogButtonUUID(),
                actionDataEntry);
      } else if (actionEventType != null && actionEventType != ActionEventType.NONE) {
        NetworkMessageHandlerManager.getServerHandler()
            .openActionDataEntryEditor(
                this.screen.getEasyNPCUUID(), actionEventType, configurationType, actionDataEntry);
      }
    }

    @Override
    public void openConditionListEditor() {
      AdditionalScreenData screenData = this.screen.getAdditionalScreenData();
      NetworkMessageHandlerManager.getServerHandler()
          .openActionConditionDataEditor(
              this.screen.getEasyNPCUUID(),
              this.screen.getActionDataEntryUUID(),
              screenData.getActionEventType(),
              screenData.getConfigurationType(),
              screenData.getEditorType(),
              this.screen.getDialogUUID(),
              this.screen.getDialogButtonUUID(),
              this.screen.getPageIndex());
    }

    @Override
    public void openConditionEntryEditor(ConditionDataEntry conditionDataEntry) {
      AdditionalScreenData screenData = this.screen.getAdditionalScreenData();
      NetworkMessageHandlerManager.getServerHandler()
          .openActionConditionDataEntryEditor(
              this.screen.getEasyNPCUUID(),
              this.screen.getActionDataEntryUUID(),
              screenData.getActionEventType(),
              screenData.getConfigurationType(),
              screenData.getEditorType(),
              this.screen.getDialogUUID(),
              this.screen.getDialogButtonUUID(),
              this.screen.getPageIndex(),
              conditionDataEntry);
    }

    @Override
    public String breadcrumbLabel() {
      return "Actions";
    }

    @Override
    public String helpTextKey() {
      return "condition.help_text.action";
    }

    @Override
    public boolean isActionContext() {
      return true;
    }
  }

  private static final class DialogButtonContext extends ConditionEditorContext {

    private DialogButtonContext(EditorScreen<?> screen) {
      super(screen);
    }

    @Override
    public ConditionDataSet loadConditionDataSet() {
      DialogButtonEntry buttonData = this.screen.getDialogButtonData();
      return buttonData != null
          ? new ConditionDataSet(buttonData.conditions())
          : new ConditionDataSet();
    }

    @Override
    public void saveConditionDataSet(ConditionDataSet conditionDataSet) {
      DialogButtonEntry buttonData = this.screen.getDialogButtonData();
      if (buttonData == null) {
        return;
      }
      NetworkMessageHandlerManager.getServerHandler()
          .saveDialogButton(
              this.screen.getEasyNPCUUID(),
              this.screen.getDialogUUID(),
              this.screen.getDialogButtonUUID(),
              buttonData.withConditions(conditionDataSet.getConditions()));
    }

    @Override
    public void openParentEditor() {
      NetworkMessageHandlerManager.getServerHandler()
          .openDialogButtonEditor(
              this.screen.getEasyNPCUUID(),
              this.screen.getDialogUUID(),
              this.screen.getDialogButtonUUID());
    }

    @Override
    public void openConditionListEditor() {
      NetworkMessageHandlerManager.getServerHandler()
          .openConditionDataEditor(
              this.screen.getEasyNPCUUID(),
              this.screen.getDialogUUID(),
              this.screen.getDialogButtonUUID());
    }

    @Override
    public void openConditionEntryEditor(ConditionDataEntry conditionDataEntry) {
      NetworkMessageHandlerManager.getServerHandler()
          .openConditionDataEntryEditor(
              this.screen.getEasyNPCUUID(),
              this.screen.getDialogUUID(),
              this.screen.getDialogButtonUUID(),
              conditionDataEntry);
    }

    @Override
    public String breadcrumbLabel() {
      DialogButtonEntry buttonData = this.screen.getDialogButtonData();
      return buttonData != null ? buttonData.getButtonName(21).getString() : "Button";
    }

    @Override
    public String helpTextKey() {
      return "condition.help_text.dialog_button";
    }
  }

  private static final class DialogContext extends ConditionEditorContext {

    private DialogContext(EditorScreen<?> screen) {
      super(screen);
    }

    @Override
    public ConditionDataSet loadConditionDataSet() {
      DialogDataEntry dialogData = this.screen.getDialogData();
      return dialogData != null && dialogData.getConditions() != null
          ? new ConditionDataSet(dialogData.getConditions())
          : new ConditionDataSet();
    }

    @Override
    public void saveConditionDataSet(ConditionDataSet conditionDataSet) {
      DialogDataEntry dialogData = this.screen.getDialogData();
      if (dialogData == null) {
        return;
      }
      dialogData.setConditions(conditionDataSet.getConditions());
      NetworkMessageHandlerManager.getServerHandler()
          .saveDialog(this.screen.getEasyNPCUUID(), this.screen.getDialogUUID(), dialogData);
    }

    @Override
    public void openParentEditor() {
      NetworkMessageHandlerManager.getServerHandler()
          .openDialogEditor(this.screen.getEasyNPCUUID(), this.screen.getDialogUUID());
    }

    @Override
    public void openConditionListEditor() {
      NetworkMessageHandlerManager.getServerHandler()
          .openConditionDataEditor(this.screen.getEasyNPCUUID(), this.screen.getDialogUUID());
    }

    @Override
    public void openConditionEntryEditor(ConditionDataEntry conditionDataEntry) {
      NetworkMessageHandlerManager.getServerHandler()
          .openConditionDataEntryEditor(
              this.screen.getEasyNPCUUID(), this.screen.getDialogUUID(), conditionDataEntry);
    }

    @Override
    public String breadcrumbLabel() {
      DialogDataEntry dialogData = this.screen.getDialogData();
      return dialogData != null ? dialogData.getName(21) : "Dialog";
    }

    @Override
    public String helpTextKey() {
      return "condition.help_text.dialog";
    }
  }
}
