package de.markusbordihn.easynpc.client.screen.editor.action.entry;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.client.screen.editor.action.ActionDataEntryEditorScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;

public class OpenNamedDialogEntry extends ActionEntryWidget {

  private final DialogDataSet dialogDataSet;
  private TextField dialogNameTextField;
  private boolean showInvalidDialogName = false;

  public OpenNamedDialogEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
    this.dialogDataSet = screen.getDialogDataSet();
  }

  private void validateDialogName(String dialogName) {
    if (dialogName == null || dialogName.isEmpty()) {
      return;
    }
    this.showInvalidDialogName =
        this.dialogDataSet == null || !this.dialogDataSet.hasDialog(dialogName);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasActionData = hasActionData(ActionDataType.OPEN_NAMED_DIALOG);

    // Named Dialog Value
    this.dialogNameTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft, editorTop + 20, 275, 16));
    this.dialogNameTextField.setMaxLength(512);
    this.dialogNameTextField.setValue(hasActionData ? this.actionDataEntry.getCommand() : "");
    this.dialogNameTextField.setResponder(this::validateDialogName);
  }

  @Override
  public void render(PoseStack poseStack, int editorLeft, int editorTop) {
    Text.drawString(
        poseStack,
        this.font,
        "Named Dialog",
        editorLeft + 2,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);

    if (this.showInvalidDialogName) {
      Text.drawString(
          poseStack,
          this.font,
          "Invalid Dialog Name",
          editorLeft + 2,
          editorTop + 40,
          Constants.FONT_COLOR_RED);
    }
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    return new ActionDataEntry(
        ActionDataType.OPEN_NAMED_DIALOG, this.dialogNameTextField.getValue());
  }

  @Override
  public boolean hasChanged() {
    return this.dialogNameTextField != null
        && !this.dialogNameTextField.getValue().equals(this.actionDataEntry.getCommand())
        && !this.showInvalidDialogName;
  }
}
