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
import de.markusbordihn.easynpc.configui.client.screen.components.HelpIcon;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.MoveActionData;
import de.markusbordihn.easynpc.data.action.MoveTargetType;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.ValueUtils;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.stream.Collectors;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.core.BlockPos;

public class MoveActionEntry extends ActionEntryWidget {

  private static final int COORDINATE_FIELD_WIDTH = 70;
  private static final int COORDINATE_MAX_LENGTH = 8;
  private static final int HELP_ICON_HEIGHT = 12;
  private static final int LABEL_HEIGHT = 8;
  private static final int LABEL_LEFT = 2;
  private static final int NUMBER_FIELD_WIDTH = 60;
  private static final int POSITION_FIELD_TOP = 44;
  private static final int POSITION_LABEL_TOP = 33;
  private static final int ROW_WIDTH = 275;
  private static final int SECOND_COLUMN_LEFT = 100;
  private static final int TARGET_FIELD_TOP = 13;
  private static final int TARGET_LABEL_TOP = 2;
  private static final int TELEPORT_TOP = 98;
  private static final int THIRD_COLUMN_LEFT = 200;
  private static final int VALUE_FIELD_TOP = 75;
  private static final int VALUE_LABEL_TOP = 64;

  private final ActionDataType actionDataType;

  private SpinButton<MoveTargetType> targetTypeButton;
  private TextField blockPosXTextField;
  private TextField blockPosYTextField;
  private TextField blockPosZTextField;
  private Checkbox speedOverrideCheckbox;
  private TextField speedTextField;
  private TextField radiusTextField;
  private TextField timeoutTextField;
  private Checkbox teleportCheckbox;

  public MoveActionEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen,
      ActionDataType actionDataType) {
    super(actionDataEntry, actionDataSet, screen);
    this.actionDataType = actionDataType;
  }

  private static int centeredTop(int labelTop, int widgetHeight) {
    return labelTop + LABEL_HEIGHT / 2 - widgetHeight / 2;
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    MoveActionData moveActionData = this.currentMoveActionData();
    BlockPos blockPos = this.currentBlockPos();

    this.blockPosXTextField =
        this.coordinateField(editorLeft, editorTop + POSITION_FIELD_TOP, blockPos.getX());
    this.blockPosYTextField =
        this.coordinateField(
            editorLeft + SECOND_COLUMN_LEFT, editorTop + POSITION_FIELD_TOP, blockPos.getY());
    this.blockPosZTextField =
        this.coordinateField(
            editorLeft + THIRD_COLUMN_LEFT, editorTop + POSITION_FIELD_TOP, blockPos.getZ());

    this.targetTypeButton =
        this.screen.addActionEntryWidget(
            new SpinButton<>(
                editorLeft,
                editorTop + TARGET_FIELD_TOP,
                ROW_WIDTH,
                16,
                Arrays.stream(MoveTargetType.values())
                    .collect(Collectors.toCollection(LinkedHashSet::new)),
                moveActionData.targetType(),
                button -> this.applyTargetType(button.get())));
    this.targetTypeButton.setLabelProvider(
        moveTargetType -> TextComponent.getTranslatedConfigText(moveTargetType.getId()));

    this.speedTextField =
        this.numberField(
            editorLeft,
            editorTop + VALUE_FIELD_TOP,
            String.valueOf(moveActionData.speedModifier()));
    boolean overridesSpeed =
        moveActionData.speedModifier() != MoveActionData.DEFAULT_SPEED_MODIFIER;
    this.speedOverrideCheckbox =
        this.screen.addActionEntryWidget(
            new Checkbox(
                editorLeft + NUMBER_FIELD_WIDTH + 4,
                editorTop + VALUE_FIELD_TOP,
                TextComponent.getTranslatedConfigText("action.move.custom_speed"),
                overridesSpeed,
                false,
                checkbox -> this.speedTextField.setEditable(checkbox.selected())));
    this.speedOverrideCheckbox.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText("action.move.speed.tooltip")));
    this.speedTextField.setEditable(overridesSpeed);
    this.radiusTextField =
        this.numberField(
            editorLeft + SECOND_COLUMN_LEFT,
            editorTop + VALUE_FIELD_TOP,
            String.valueOf(moveActionData.arrivalRadius()));
    this.timeoutTextField =
        this.numberField(
            editorLeft + THIRD_COLUMN_LEFT,
            editorTop + VALUE_FIELD_TOP,
            String.valueOf(moveActionData.timeoutTicks()));

    this.screen.addActionEntryWidget(
        new HelpIcon(
            this.helpIconLeft(editorLeft, "action.move.speed"),
            centeredTop(editorTop + VALUE_LABEL_TOP, HELP_ICON_HEIGHT),
            "action.move.speed.tooltip"));

    this.screen.addActionEntryWidget(
        new HelpIcon(
            this.helpIconLeft(editorLeft + SECOND_COLUMN_LEFT, "action.move.radius"),
            centeredTop(editorTop + VALUE_LABEL_TOP, HELP_ICON_HEIGHT),
            "action.move.radius.tooltip"));

    this.teleportCheckbox =
        this.screen.addActionEntryWidget(
            new Checkbox(
                editorLeft,
                editorTop + TELEPORT_TOP,
                TextComponent.getTranslatedConfigText("action.move.teleport"),
                moveActionData.teleportOnTimeout(),
                true));

    this.applyTargetType(moveActionData.targetType());
  }

  private TextField coordinateField(int left, int top, int value) {
    TextField textField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, left, top, COORDINATE_FIELD_WIDTH, 16));
    textField.setMaxLength(COORDINATE_MAX_LENGTH);
    textField.setFilter(ValueUtils::isNumericValue);
    textField.setValue(String.valueOf(value));
    return textField;
  }

  private TextField numberField(int left, int top, String value) {
    TextField textField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, left, top, NUMBER_FIELD_WIDTH, 16));
    textField.setFilter(ValueUtils::isFloatValue);
    textField.setValue(value);
    return textField;
  }

  private int helpIconLeft(int labelLeft, String labelTranslationKey) {
    return labelLeft
        + LABEL_LEFT
        + this.font.width(TextComponent.getTranslatedConfigText(labelTranslationKey))
        + 4;
  }

  private void applyTargetType(MoveTargetType moveTargetType) {
    boolean requiresPosition = moveTargetType.requiresPosition();
    this.blockPosXTextField.setVisible(requiresPosition);
    this.blockPosYTextField.setVisible(requiresPosition);
    this.blockPosZTextField.setVisible(requiresPosition);
  }

  private boolean requiresPosition() {
    return this.targetTypeButton != null && this.targetTypeButton.get().requiresPosition();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.move.target",
        editorLeft + LABEL_LEFT,
        editorTop + TARGET_LABEL_TOP,
        Constants.FONT_COLOR_DEFAULT);

    if (this.requiresPosition()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "action.move.position",
          editorLeft + LABEL_LEFT,
          editorTop + POSITION_LABEL_TOP,
          Constants.FONT_COLOR_DEFAULT);
    }

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.move.speed",
        editorLeft + LABEL_LEFT,
        editorTop + VALUE_LABEL_TOP,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.move.radius",
        editorLeft + SECOND_COLUMN_LEFT + LABEL_LEFT,
        editorTop + VALUE_LABEL_TOP,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.move.timeout",
        editorLeft + THIRD_COLUMN_LEFT + LABEL_LEFT,
        editorTop + VALUE_LABEL_TOP,
        Constants.FONT_COLOR_DEFAULT);

    if (!this.isValid()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "action.move.missing_position",
          editorLeft + SECOND_COLUMN_LEFT,
          editorTop + POSITION_LABEL_TOP,
          Constants.FONT_COLOR_RED);
    }
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    ActionDataEntry entry =
        new ActionDataEntry(this.actionDataType).withMoveActionData(this.selectedMoveActionData());

    return this.requiresPosition() ? entry.withBlockPos(this.selectedBlockPos()) : entry;
  }

  @Override
  public boolean hasChanged() {
    return !this.hasActionData(this.actionDataType)
        || !this.selectedMoveActionData().equals(this.currentMoveActionData())
        || !this.selectedBlockPos().equals(this.currentBlockPos());
  }

  @Override
  public boolean isValid() {
    return this.targetTypeButton != null
        && this.selectedMoveActionData().hasResolvableTarget(this.selectedBlockPos());
  }

  private MoveActionData currentMoveActionData() {
    if (!this.hasActionData(this.actionDataType) || this.actionDataEntry.moveActionData() == null) {
      return MoveActionData.DEFAULT;
    }

    return this.actionDataEntry.moveActionData();
  }

  private BlockPos currentBlockPos() {
    return this.hasActionData(this.actionDataType) && this.actionDataEntry.hasBlockPos()
        ? this.actionDataEntry.blockPos()
        : BlockPos.ZERO;
  }

  private MoveActionData selectedMoveActionData() {
    if (this.targetTypeButton == null) {
      return MoveActionData.DEFAULT;
    }

    return new MoveActionData(
        this.targetTypeButton.get(),
        this.speedOverrideCheckbox.selected()
            ? this.doubleValue(this.speedTextField, MoveActionData.DEFAULT_SPEED_MODIFIER)
            : MoveActionData.DEFAULT_SPEED_MODIFIER,
        (float) this.doubleValue(this.radiusTextField, MoveActionData.DEFAULT_ARRIVAL_RADIUS),
        (int) this.doubleValue(this.timeoutTextField, MoveActionData.DEFAULT_TIMEOUT_TICKS),
        this.teleportCheckbox.selected());
  }

  private BlockPos selectedBlockPos() {
    if (this.blockPosXTextField == null) {
      return BlockPos.ZERO;
    }

    return new BlockPos(
        this.intValue(this.blockPosXTextField),
        this.intValue(this.blockPosYTextField),
        this.intValue(this.blockPosZTextField));
  }

  private double doubleValue(TextField textField, double defaultValue) {
    String value = textField.getValue().trim();
    if (!ValueUtils.isFloatValue(value) || value.isEmpty() || "-".equals(value)) {
      return defaultValue;
    }

    return Double.parseDouble(value);
  }

  private int intValue(TextField textField) {
    String value = textField.getValue().trim();
    if (!ValueUtils.isNumericValue(value) || value.isEmpty() || "-".equals(value)) {
      return 0;
    }

    return Integer.parseInt(value);
  }
}
