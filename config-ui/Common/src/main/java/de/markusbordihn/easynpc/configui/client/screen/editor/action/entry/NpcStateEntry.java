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

package de.markusbordihn.easynpc.configui.client.screen.editor.action.entry;

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.editor.TargetType;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.state.StateActionCommand;
import de.markusbordihn.easynpc.data.state.StateEntry;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.data.state.StateOperation;
import de.markusbordihn.easynpc.data.state.StateValueType;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.ValueUtils;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.UUID;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Tooltip;

public class NpcStateEntry extends ActionEntryWidget {

  private TextField stateNameTextField;
  private TextField stateValueTextField;
  private SpinButton<StateValueType> valueTypeButton;
  private Checkbox stateFlagCheckbox;
  private Checkbox debugCheckbox;
  private TextField targetUuidTextField;
  private StateOperation currentOperation;
  private StateValueType currentValueType;
  private TargetType currentTargetType;

  public NpcStateEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
    StateActionCommand stateActionCommand = this.parseCommandData();
    this.currentOperation = stateActionCommand.operation();
    this.currentValueType = stateActionCommand.valueType();
    this.currentTargetType =
        actionDataEntry.targetUUID() != null ? TargetType.UUID : TargetType.SELF;
  }

  private StateActionCommand parseCommandData() {
    if (!hasActionData(ActionDataType.NPC_STATE) || this.actionDataEntry.command() == null) {
      return StateActionCommand.EMPTY;
    }

    StateActionCommand stateActionCommand =
        StateActionCommand.parse(this.actionDataEntry.command());
    return stateActionCommand.operation() != null ? stateActionCommand : StateActionCommand.EMPTY;
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasActionData = hasActionData(ActionDataType.NPC_STATE);
    StateActionCommand stateActionCommand = this.parseCommandData();

    this.screen.addActionEntryWidget(
        new SpinButton<>(
            editorLeft,
            editorTop + 20,
            120,
            16,
            new LinkedHashSet<>(Arrays.asList(StateOperation.values())),
            this.currentOperation,
            spinButton -> {
              this.currentOperation = spinButton.get();
              this.applyOperation();
            }));

    this.stateNameTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft + 125, editorTop + 20, 150, 16));
    this.stateNameTextField.setMaxLength(StateIdentifier.MAX_LENGTH);
    this.stateNameTextField.setFilter(StateIdentifier::isValidInput);
    this.stateNameTextField.setValue(stateActionCommand.stateName());

    this.valueTypeButton =
        this.screen.addActionEntryWidget(
            new SpinButton<>(
                editorLeft,
                editorTop + 45,
                120,
                16,
                new LinkedHashSet<>(Arrays.asList(StateValueType.values())),
                this.currentValueType,
                spinButton -> {
                  this.currentValueType = spinButton.get();
                  this.applyOperation();
                }));

    this.stateValueTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft + 125, editorTop + 45, 150, 16));
    this.stateValueTextField.setMaxLength(StateEntry.MAX_TEXT_VALUE_LENGTH);
    this.stateValueTextField.setValue(stateActionCommand.value());

    this.stateFlagCheckbox =
        this.screen.addActionEntryWidget(
            new Checkbox(
                editorLeft + 125,
                editorTop + 45,
                TextComponent.getText(""),
                StateValueType.FLAG.parse(stateActionCommand.value()).asFlag(),
                false));

    this.screen.addActionEntryWidget(
        new SpinButton<>(
            editorLeft,
            editorTop + 70,
            120,
            16,
            TargetType.valueSet(),
            this.currentTargetType,
            spinButton -> {
              this.currentTargetType = spinButton.get();
              this.applyTargetType();
            }));

    this.targetUuidTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft + 125, editorTop + 70, 150, 16));
    this.targetUuidTextField.setMaxLength(TargetType.MAX_UUID_LENGTH);
    this.targetUuidTextField.setValue(
        this.actionDataEntry.targetUUID() != null
            ? this.actionDataEntry.targetUUID().toString()
            : "");

    this.debugCheckbox =
        this.screen.addActionEntryWidget(
            new Checkbox(
                editorLeft,
                editorTop + 95,
                "debug",
                hasActionData && this.actionDataEntry.enableDebug()));
    this.debugCheckbox.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText("action.debug.tooltip")));

    this.applyOperation();
    this.applyTargetType();
  }

  private void applyTargetType() {
    this.targetUuidTextField.setVisible(this.currentTargetType == TargetType.UUID);
  }

  private UUID targetUUID() {
    return this.currentTargetType == TargetType.UUID
        ? ValueUtils.getUuidValue(this.targetUuidTextField.getValue())
        : null;
  }

  private boolean usesValueType() {
    return this.currentOperation == StateOperation.SET;
  }

  private boolean usesFlagValue() {
    return this.usesValueType() && this.currentValueType == StateValueType.FLAG;
  }

  private void applyOperation() {
    this.valueTypeButton.visible = this.usesValueType();
    this.stateValueTextField.setVisible(
        this.currentOperation.requiresValue() && !this.usesFlagValue());
    this.stateFlagCheckbox.visible = this.usesFlagValue();
  }

  @Override
  public void render(GuiGraphicsExtractor guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.name",
        editorLeft + 125,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);

    if (this.currentOperation.requiresValue() && !this.usesValueType()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "action.value",
          editorLeft + 85,
          editorTop + 49,
          Constants.FONT_COLOR_DEFAULT);
    }

    if (!this.stateName().isEmpty() && !StateIdentifier.isValid(this.stateName())) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "action.npc_state.invalid_name",
          editorLeft,
          editorTop + 117,
          Constants.FONT_COLOR_RED);
    } else if (!this.hasValidTargetUUID()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "action.invalid_uuid",
          editorLeft,
          editorTop + 117,
          Constants.FONT_COLOR_RED);
    }
  }

  private boolean hasValidTargetUUID() {
    return this.currentTargetType != TargetType.UUID
        || this.targetUuidTextField.getValue().trim().isEmpty()
        || this.targetUUID() != null;
  }

  @Override
  public boolean isValid() {
    return this.stateNameTextField == null
        || (StateIdentifier.isValid(this.stateName()) && this.hasValidTargetUUID());
  }

  private String stateName() {
    return this.stateNameTextField != null ? this.stateNameTextField.getValue().trim() : "";
  }

  private String stateValue() {
    return this.usesFlagValue()
        ? String.valueOf(this.stateFlagCheckbox.selected())
        : this.stateValueTextField.getValue().trim();
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    return new ActionDataEntry(
            ActionDataType.NPC_STATE,
            new StateActionCommand(
                    this.currentOperation,
                    this.stateName(),
                    this.currentValueType,
                    this.stateValue())
                .toCommand(),
            false,
            this.debugCheckbox.selected())
        .withTargetUUID(this.targetUUID());
  }

  @Override
  public boolean hasChanged() {
    ActionDataEntry currentEntry = this.getActionDataEntry();
    return !this.actionDataEntry.actionDataType().equals(ActionDataType.NPC_STATE)
        || !this.actionDataEntry.command().equals(currentEntry.command())
        || !Objects.equals(this.actionDataEntry.targetUUID(), currentEntry.targetUUID())
        || this.actionDataEntry.enableDebug() != currentEntry.enableDebug();
  }
}
