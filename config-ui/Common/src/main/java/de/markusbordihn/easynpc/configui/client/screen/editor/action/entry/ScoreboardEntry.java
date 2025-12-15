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

import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.scoreboard.ScoreboardOperation;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.gui.GuiGraphics;

public class ScoreboardEntry extends ActionEntryWidget {

  private TextField scoreboardNameTextField;
  private TextField scoreboardValueTextField;
  private Checkbox debugCheckbox;
  private ScoreboardOperation currentOperation;

  public ScoreboardEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
    this.currentOperation = parseOperation(actionDataEntry);
  }

  private ScoreboardOperation parseOperation(ActionDataEntry actionDataEntry) {
    if (!hasActionData(ActionDataType.SCOREBOARD) || actionDataEntry.command() == null) {
      return ScoreboardOperation.INCREASE;
    }
    return ScoreboardOperation.fromCommand(actionDataEntry.command());
  }

  private String[] parseCommandData(ActionDataEntry actionDataEntry) {
    if (!hasActionData(ActionDataType.SCOREBOARD) || actionDataEntry.command() == null) {
      return new String[] {"", "1"};
    }

    String command = actionDataEntry.command();
    String[] parts = command.split(":", 3);
    if (parts.length < 2) {
      return new String[] {"", "1"};
    }

    return new String[] {parts[1], parts.length > 2 ? parts[2] : "1"};
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasActionData = hasActionData(ActionDataType.SCOREBOARD);
    String[] commandData = parseCommandData(actionDataEntry);
    String scoreboardName = commandData[0];
    String scoreboardValue = commandData[1];

    java.util.Set<TranslatableScoreboardOperation> operationSet = new java.util.LinkedHashSet<>();
    for (ScoreboardOperation op : ScoreboardOperation.values()) {
      operationSet.add(new TranslatableScoreboardOperation(op));
    }

    this.screen.addActionEntryWidget(
        new SpinButton<>(
            editorLeft,
            editorTop + 20,
            120,
            16,
            operationSet,
            new TranslatableScoreboardOperation(this.currentOperation),
            spinButton -> {
              Object value = spinButton.get();
              if (value instanceof TranslatableScoreboardOperation translatable) {
                this.currentOperation = translatable.getOperation();
                if (this.scoreboardValueTextField != null) {
                  this.scoreboardValueTextField.setVisible(
                      this.currentOperation == ScoreboardOperation.SET);
                }
              }
            }));

    this.scoreboardNameTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft + 125, editorTop + 20, 150, 16));
    this.scoreboardNameTextField.setMaxLength(32);
    this.scoreboardNameTextField.setValue(scoreboardName);

    this.scoreboardValueTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft + 125, editorTop + 45, 150, 16));
    this.scoreboardValueTextField.setMaxLength(10);
    this.scoreboardValueTextField.setValue(scoreboardValue);
    this.scoreboardValueTextField.setVisible(this.currentOperation == ScoreboardOperation.SET);

    this.debugCheckbox =
        this.screen.addActionEntryWidget(
            new Checkbox(
                editorLeft,
                editorTop + 70,
                "debug",
                hasActionData && this.actionDataEntry.enableDebug()));
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.name",
        editorLeft + 125,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);

    if (this.currentOperation == ScoreboardOperation.SET) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "action.value",
          editorLeft + 85,
          editorTop + 49,
          Constants.FONT_COLOR_DEFAULT);
    }
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    String scoreboardName = this.scoreboardNameTextField.getValue().trim();
    String value =
        this.currentOperation == ScoreboardOperation.SET
            ? this.scoreboardValueTextField.getValue().trim()
            : "1";
    String command = this.currentOperation.getCommandName() + ":" + scoreboardName + ":" + value;

    return new ActionDataEntry(
        ActionDataType.SCOREBOARD, command, false, this.debugCheckbox.selected());
  }

  @Override
  public boolean hasChanged() {
    ActionDataEntry currentEntry = getActionDataEntry();
    return !this.actionDataEntry.actionDataType().equals(ActionDataType.SCOREBOARD)
        || !this.actionDataEntry.command().equals(currentEntry.command())
        || this.actionDataEntry.enableDebug() != currentEntry.enableDebug();
  }

  private static class TranslatableScoreboardOperation {
    private final ScoreboardOperation operation;

    TranslatableScoreboardOperation(ScoreboardOperation operation) {
      this.operation = operation;
    }

    ScoreboardOperation getOperation() {
      return this.operation;
    }

    @Override
    public String toString() {
      return TextComponent.getTranslatedConfigText(this.operation.getTranslationKey()).getString();
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj) return true;
      if (!(obj instanceof TranslatableScoreboardOperation other)) return false;
      return this.operation == other.operation;
    }

    @Override
    public int hashCode() {
      return this.operation.hashCode();
    }
  }
}
