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

package de.markusbordihn.easynpc.configui.client.screen.editor.action.entry;

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import java.util.LinkedHashSet;
import java.util.UUID;
import net.minecraft.client.gui.GuiGraphicsExtractor;

public class OpenNamedDialogEntry extends ActionEntryWidget {

  private final DialogDataSet dialogDataSet;
  private final ActionDataType actionDataType;
  private TextField dialogNameTextField;
  private TextField targetUuidTextField;
  private TargetType targetType;
  private SpinButton<TargetType> targetTypeButton;
  private boolean showInvalidDialogName = false;
  private boolean showInvalidUuid = false;

  public OpenNamedDialogEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
    this.dialogDataSet = screen.getDialogDataSet();
    this.actionDataType =
        screen.getActionDataType() == ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL
            ? ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL
            : ActionDataType.OPEN_NAMED_DIALOG;
    this.targetType = actionDataEntry.targetUUID() != null ? TargetType.UUID : TargetType.SELF;
  }

  private void validateDialogName(String dialogName) {
    if (dialogName == null || dialogName.isEmpty()) {
      return;
    }
    this.showInvalidDialogName =
        this.targetType != TargetType.SELF
            || this.dialogDataSet == null
            || !this.dialogDataSet.hasDialog(dialogName);
  }

  private void validateUuid(String uuidString) {
    if (uuidString == null || uuidString.isEmpty()) {
      this.showInvalidUuid = false;
      return;
    }
    try {
      UUID.fromString(uuidString);
      this.showInvalidUuid = false;
    } catch (IllegalArgumentException e) {
      this.showInvalidUuid = true;
    }
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasActionData = hasActionData(this.actionDataType);

    // Named Dialog Value
    this.dialogNameTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft + 1, editorTop + 40, 301, 16));
    this.dialogNameTextField.setMaxLength(512);
    this.dialogNameTextField.setValue(hasActionData ? this.actionDataEntry.command() : "");
    this.dialogNameTextField.setResponder(this::validateDialogName);

    // Target Type Selector
    LinkedHashSet<TargetType> targetTypes = new LinkedHashSet<>();
    targetTypes.add(TargetType.SELF);
    targetTypes.add(TargetType.UUID);
    this.targetTypeButton =
        this.screen.addActionEntryWidget(
            new SpinButton<>(
                editorLeft,
                this.dialogNameTextField.getY() + this.dialogNameTextField.getHeight() + 2,
                80,
                16,
                targetTypes,
                this.targetType,
                this::onTargetTypeChange));

    // Target UUID Field (only visible for CUSTOM)
    this.targetUuidTextField =
        this.screen.addActionEntryWidget(
            new TextField(
                this.font,
                this.targetTypeButton.getX() + this.targetTypeButton.getWidth() + 2,
                this.targetTypeButton.getY(),
                220,
                16));
    this.targetUuidTextField.setMaxLength(36);
    this.targetUuidTextField.setValue(
        hasActionData && this.actionDataEntry.targetUUID() != null
            ? this.actionDataEntry.targetUUID().toString()
            : "");
    this.targetUuidTextField.setResponder(this::validateUuid);
    this.targetUuidTextField.visible = this.targetType == TargetType.UUID;
  }

  private void onTargetTypeChange(SpinButton<?> spinButton) {
    this.targetType = (TargetType) spinButton.get();
    if (this.targetUuidTextField != null) {
      this.targetUuidTextField.visible = this.targetType == TargetType.UUID;
    }
  }

  @Override
  public void render(GuiGraphicsExtractor guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.named_dialog",
        editorLeft + 2,
        editorTop + 27,
        Constants.FONT_COLOR_DEFAULT);

    if (this.showInvalidDialogName && this.targetType == TargetType.SELF) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "action.invalid_dialog_name",
          editorLeft + 95,
          editorTop + 62,
          Constants.FONT_COLOR_RED);
    }

    if (this.showInvalidUuid && this.targetType == TargetType.UUID) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "action.invalid_uuid",
          editorLeft + 90,
          editorTop + 78,
          Constants.FONT_COLOR_RED);
    }
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    if (this.targetType == TargetType.UUID && this.targetUuidTextField != null) {
      String uuidString = this.targetUuidTextField.getValue();
      try {
        UUID targetUUID = UUID.fromString(uuidString);
        return new ActionDataEntry(
            this.actionDataType, targetUUID, this.dialogNameTextField.getValue());
      } catch (IllegalArgumentException e) {
        return new ActionDataEntry(this.actionDataType, this.dialogNameTextField.getValue());
      }
    }
    return new ActionDataEntry(this.actionDataType, this.dialogNameTextField.getValue());
  }

  @Override
  public boolean hasChanged() {
    // Invalid dialog name for self target type, no changes allowed.
    if (this.showInvalidDialogName && this.targetType == TargetType.SELF) {
      return false;
    }

    // Invalid UUID for uuid target type, no changes allowed.
    if (this.showInvalidUuid && this.targetType == TargetType.UUID) {
      return false;
    }

    // Check for changes for the different fields.
    boolean dialogNameChanged =
        this.dialogNameTextField != null
            && !this.dialogNameTextField.getValue().equals(this.actionDataEntry.command());
    boolean targetTypeChanged =
        (this.targetType == TargetType.UUID) == (this.actionDataEntry.targetUUID() == null);
    boolean targetUuidChanged = false;
    if (this.targetType == TargetType.UUID && this.targetUuidTextField != null) {
      String currentUuid = this.targetUuidTextField.getValue();
      String originalUuid =
          this.actionDataEntry.targetUUID() != null
              ? this.actionDataEntry.targetUUID().toString()
              : "";
      targetUuidChanged = !currentUuid.equals(originalUuid);
    }

    return dialogNameChanged || targetTypeChanged || targetUuidChanged;
  }

  private enum TargetType {
    SELF,
    UUID
  }
}
