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
import net.minecraft.client.gui.GuiGraphics;

public class OpenNamedDialogEntry extends ActionEntryWidget {

  private final DialogDataSet dialogDataSet;
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
    boolean hasActionData = hasActionData(ActionDataType.OPEN_NAMED_DIALOG);

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
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
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
            ActionDataType.OPEN_NAMED_DIALOG, targetUUID, this.dialogNameTextField.getValue());
      } catch (IllegalArgumentException e) {
        return new ActionDataEntry(
            ActionDataType.OPEN_NAMED_DIALOG, this.dialogNameTextField.getValue());
      }
    }
    return new ActionDataEntry(
        ActionDataType.OPEN_NAMED_DIALOG, this.dialogNameTextField.getValue());
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
