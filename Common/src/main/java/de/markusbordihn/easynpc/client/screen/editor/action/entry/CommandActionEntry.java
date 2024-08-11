package de.markusbordihn.easynpc.client.screen.editor.action.entry;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;

public class CommandActionEntry extends ActionEntryWidget {

  private TextField actionValueTextField;
  private Checkbox debugCheckbox;
  private Checkbox executeAsUserCheckbox;

  public CommandActionEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasActionData = hasActionData(ActionDataType.COMMAND);

    // Command Value
    this.actionValueTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft, editorTop + 20, 275, 16));
    this.actionValueTextField.setMaxLength(512);
    this.actionValueTextField.setValue(hasActionData ? this.actionDataEntry.command() : "");

    // Execute as User
    this.executeAsUserCheckbox =
        this.screen.addActionEntryWidget(
            new Checkbox(
                editorLeft,
                editorTop + 40,
                "execute_as_player",
                hasActionData && this.actionDataEntry.executeAsUser()));

    // Debug
    this.debugCheckbox =
        this.screen.addActionEntryWidget(
            new Checkbox(
                editorLeft + 200,
                editorTop + 40,
                "debug",
                hasActionData && this.actionDataEntry.enableDebug()));
  }

  @Override
  public void render(PoseStack poseStack, int editorLeft, int editorTop) {
    Text.drawString(
        poseStack,
        this.font,
        "Action Command",
        editorLeft + 2,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    return new ActionDataEntry(
        ActionDataType.COMMAND,
        this.actionValueTextField.getValue(),
        this.executeAsUserCheckbox.selected(),
        this.debugCheckbox.selected());
  }

  @Override
  public boolean hasChanged() {
    return (this.actionValueTextField != null
            && !this.actionValueTextField.getValue().equals(this.actionDataEntry.command()))
        || (this.executeAsUserCheckbox != null
            && this.executeAsUserCheckbox.selected() != this.actionDataEntry.executeAsUser())
        || (this.debugCheckbox != null
            && this.debugCheckbox.selected() != this.actionDataEntry.enableDebug());
  }
}
