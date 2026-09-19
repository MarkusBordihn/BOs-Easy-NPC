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

package de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry;

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.condition.NpcStateCondition;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.editor.TargetType;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.ConditionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.state.StateEntry;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.data.state.StateValueType;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.ValueUtils;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.UUID;
import net.minecraft.client.gui.GuiGraphicsExtractor;

public class NpcStateConditionEntry extends ConditionEntryWidget {

  private TextField nameTextField;
  private SpinButton<ConditionOperationType> operationTypeButton;
  private TextField valueTextField;
  private Checkbox valueCheckbox;
  private TextField targetUuidTextField;
  private StateValueType currentValueType;
  private TargetType currentTargetType;

  public NpcStateConditionEntry(
      ConditionDataEntry conditionDataEntry,
      ConditionDataSet conditionDataSet,
      ConditionDataEntryEditorContainerScreen<?> screen) {
    super(conditionDataEntry, conditionDataSet, screen);
    this.currentValueType =
        hasConditionData(ConditionType.NPC_STATE)
            ? NpcStateCondition.valueTypeOf(this.conditionDataEntry)
            : StateValueType.NUMBER;
    this.currentTargetType =
        this.conditionDataEntry.hasTargetUUID() ? TargetType.UUID : TargetType.SELF;
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasConditionData = hasConditionData(ConditionType.NPC_STATE);
    this.nameTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font,
                editorLeft + 110,
                editorTop,
                180,
                hasConditionData ? this.conditionDataEntry.name() : "",
                StateIdentifier.MAX_LENGTH));
    this.nameTextField.setFilter(StateIdentifier::isValidInput);

    this.screen.addConditionEntryWidget(
        new SpinButton<>(
            editorLeft + 110,
            editorTop + 25,
            180,
            16,
            new LinkedHashSet<>(Arrays.asList(StateValueType.values())),
            this.currentValueType,
            button -> {
              if (button.get() != this.currentValueType) {
                this.currentValueType = button.get();
                this.applyValueType();
              }
            }));

    this.operationTypeButton =
        this.screen.addConditionEntryWidget(
            new SpinButton<>(
                editorLeft + 110,
                editorTop + 50,
                180,
                16,
                this.currentValueType.supportedOperationTypes(),
                hasConditionData
                    ? this.conditionDataEntry.operationType()
                    : ConditionOperationType.EQUALS,
                button -> this.applyValueWidgets()));

    this.valueTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font,
                editorLeft + 110,
                editorTop + 75,
                180,
                this.currentValueType == StateValueType.TEXT
                    ? this.conditionText()
                    : String.valueOf(hasConditionData ? this.conditionDataEntry.value() : 0),
                StateEntry.MAX_TEXT_VALUE_LENGTH));

    this.valueCheckbox =
        this.screen.addConditionEntryWidget(
            new Checkbox(
                editorLeft + 110,
                editorTop + 75,
                TextComponent.getText(""),
                hasConditionData && this.conditionDataEntry.value() != 0,
                false));

    this.screen.addConditionEntryWidget(
        new SpinButton<>(
            editorLeft + 110,
            editorTop + 100,
            80,
            16,
            TargetType.valueSet(),
            this.currentTargetType,
            button -> {
              this.currentTargetType = button.get();
              this.applyTargetType();
            }));

    this.targetUuidTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font,
                editorLeft + 110,
                editorTop + 125,
                180,
                this.conditionDataEntry.hasTargetUUID()
                    ? this.conditionDataEntry.targetUUID().toString()
                    : "",
                TargetType.MAX_UUID_LENGTH));

    this.applyValueWidgets();
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

  private String conditionText() {
    return this.conditionDataEntry.hasCustomData() ? this.conditionDataEntry.customData() : "";
  }

  private void applyValueType() {
    this.operationTypeButton.setValues(
        this.currentValueType.supportedOperationTypes(),
        this.currentValueType.supports(this.operationTypeButton.get())
            ? this.operationTypeButton.get()
            : ConditionOperationType.EQUALS);
    this.valueTextField.setValue(this.currentValueType == StateValueType.TEXT ? "" : "0");
    this.applyValueWidgets();
  }

  private boolean isExistenceOperation() {
    return this.operationTypeButton.get().isExistenceOperation();
  }

  private void applyValueWidgets() {
    boolean hasValue = !this.isExistenceOperation();
    this.valueTextField.setFilter(
        this.currentValueType == StateValueType.TEXT
            ? value -> value.length() <= StateEntry.MAX_TEXT_VALUE_LENGTH
            : ValueUtils::isNumericValue);
    this.valueTextField.setVisible(hasValue && this.currentValueType != StateValueType.FLAG);
    this.valueCheckbox.visible = hasValue && this.currentValueType == StateValueType.FLAG;
  }

  @Override
  public void render(GuiGraphicsExtractor guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.npc_state.name",
        editorLeft,
        editorTop + 4,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.npc_state.value_type",
        editorLeft,
        editorTop + 29,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.npc_state.operation",
        editorLeft,
        editorTop + 54,
        Constants.FONT_COLOR_BLACK);
    if (!this.isExistenceOperation()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          this.valueLabel(),
          editorLeft,
          editorTop + 79,
          Constants.FONT_COLOR_BLACK);
    }

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.npc_state.target",
        editorLeft,
        editorTop + 104,
        Constants.FONT_COLOR_BLACK);

    if (this.currentTargetType == TargetType.UUID && !this.hasValidTargetUUID()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "condition.invalid_uuid",
          editorLeft,
          editorTop + 145,
          Constants.FONT_COLOR_RED);
    }
  }

  private boolean hasValidTargetUUID() {
    return this.targetUuidTextField.getValue().trim().isEmpty() || this.targetUUID() != null;
  }

  private String valueLabel() {
    return switch (this.currentValueType) {
      case NUMBER -> "condition.npc_state.value";
      case FLAG -> "condition.npc_state.flag_value";
      case TEXT -> "condition.npc_state.text_value";
    };
  }

  @Override
  public ConditionDataEntry getConditionDataEntry() {
    return new ConditionDataEntry(
            ConditionType.NPC_STATE,
            this.currentValueType,
            this.operationTypeButton.get(),
            this.nameTextField.getValue(),
            this.numberValue())
        .withCustomData(
            this.currentValueType == StateValueType.TEXT && !this.isExistenceOperation()
                ? this.valueTextField.getValue()
                : "")
        .withTargetUUID(this.targetUUID());
  }

  private int numberValue() {
    if (this.isExistenceOperation()) {
      return 0;
    }

    if (this.currentValueType == StateValueType.FLAG) {
      return this.valueCheckbox.selected() ? 1 : 0;
    }

    if (this.currentValueType == StateValueType.TEXT) {
      return 0;
    }

    try {
      return Integer.parseInt(this.valueTextField.getValue());
    } catch (NumberFormatException e) {
      return 0;
    }
  }
}
