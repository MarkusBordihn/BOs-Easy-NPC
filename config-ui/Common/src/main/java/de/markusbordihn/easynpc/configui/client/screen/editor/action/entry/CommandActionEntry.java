package de.markusbordihn.easynpc.configui.client.screen.editor.action.entry;

import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import net.minecraft.client.gui.GuiGraphics;

public class CommandActionEntry extends ActionEntryWidget {

  private TextField actionValueTextField;
  private Checkbox debugCheckbox;
  private Checkbox executeAsUserCheckbox;
  private boolean showDialogCommandHint;
  private boolean showExecuteAsUserWarning;

  public CommandActionEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
    this.showExecuteAsUserWarning = !screen.currentEventRequiresServerPlayer();
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
    this.actionValueTextField.setResponder(this::checkForDialogCommand);

    // Execute as User
    boolean executeAsUserValue = hasActionData && this.actionDataEntry.executeAsUser();
    if (this.showExecuteAsUserWarning) {
      executeAsUserValue = false;
    }

    this.executeAsUserCheckbox =
        this.screen.addActionEntryWidget(
            new Checkbox(editorLeft, editorTop + 40, "execute_as_player", executeAsUserValue));

    // Disable checkbox if server player is not available
    if (this.showExecuteAsUserWarning) {
      this.executeAsUserCheckbox.active = false;
    }

    // Debug
    this.debugCheckbox =
        this.screen.addActionEntryWidget(
            new Checkbox(
                editorLeft + 200,
                editorTop + 40,
                "debug",
                hasActionData && this.actionDataEntry.enableDebug()));
  }

  private void checkForDialogCommand(String command) {
    this.showDialogCommandHint = DialogCommandParser.isDialogOpenCommand(command);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.command",
        editorLeft + 2,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);

    if (this.showDialogCommandHint) {
      Text.drawString(
          guiGraphics,
          this.font,
          "Hint: Use 'Open Named Dialog' action type instead",
          editorLeft + 2,
          editorTop + 60,
          Constants.FONT_COLOR_YELLOW);
    }

    if (this.showExecuteAsUserWarning) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "action.executeAsUser.disabled.line1",
          editorLeft + 2,
          editorTop + 75,
          Constants.FONT_COLOR_DARK_GREEN);
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "action.executeAsUser.disabled.line2",
          editorLeft + 2,
          editorTop + 85,
          Constants.FONT_COLOR_DARK_GREEN);
    }
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    String command = this.actionValueTextField.getValue();

    ActionDataEntry parsedDialog = DialogCommandParser.parseDialogCommand(command);
    if (parsedDialog != null) {
      return parsedDialog;
    }

    return new ActionDataEntry(
        ActionDataType.COMMAND,
        command,
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
