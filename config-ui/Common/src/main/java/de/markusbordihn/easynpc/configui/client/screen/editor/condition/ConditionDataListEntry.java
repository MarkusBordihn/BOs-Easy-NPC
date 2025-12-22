/*
 * Copyright 2023 Markus Bordihn
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

import de.markusbordihn.easynpc.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.client.screen.components.EditButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.TextUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.ObjectSelectionList;
import net.minecraft.network.chat.Component;

public class ConditionDataListEntry extends ObjectSelectionList.Entry<ConditionDataListEntry> {

  // Column position constants
  public static final int ID_LEFT_POS = 0;
  public static final int TYPE_LEFT_POS = 22;
  public static final int VALUE_LEFT_POS = 110;
  public static final int OPTIONS_LEFT_POS = 250;

  // Layout constants
  private static final int ENTRY_HEIGHT = 21;
  private static final int FIELD_LEFT_OFFSET = 5;
  private static final int FIELD_TOP_OFFSET = 5;
  private static final int COLUMN_SEPARATOR_WIDTH = 1;
  private static final int COLUMN_SEPARATOR_OFFSET = 3;
  private static final int BUTTON_SPACING = 2;
  private static final int BUTTON_SIZE = 18;
  private static final int VALUE_MAX_LENGTH = 21;
  private static final int LIST_WIDTH = 309;

  // Color constants
  private static final int COLOR_SEPARATOR_LINE = 0xffaaaaaa;
  private static final int COLOR_COLUMN_SEPARATOR = 0xff666666;

  private final Font font;
  private final int leftPos;
  private final int topPos;
  private final ConditionDataEntry conditionDataEntry;
  private final ConditionType conditionType;
  private final EditButton editButton;
  private final DeleteButton deleteButton;

  public ConditionDataListEntry(
      Minecraft minecraft,
      ConditionDataEntry conditionDataEntry,
      int leftPos,
      int topPos,
      OnEdit onEdit,
      OnRemove onRemove) {
    super();

    // Set font and position
    this.font = minecraft.font;
    this.leftPos = leftPos;
    this.topPos = topPos;

    // Set condition data entry
    this.conditionDataEntry = conditionDataEntry;
    this.conditionType =
        conditionDataEntry != null ? conditionDataEntry.conditionType() : ConditionType.NONE;

    // Adding general buttons
    this.editButton =
        new EditButton(
            this.leftPos + OPTIONS_LEFT_POS + 4,
            this.topPos,
            BUTTON_SIZE,
            BUTTON_SIZE,
            onPress -> {
              if (onEdit != null) {
                onEdit.edit(conditionDataEntry);
              }
            });
    this.deleteButton =
        new DeleteButton(
            this.editButton.getX() + this.editButton.getWidth() + BUTTON_SPACING,
            this.topPos,
            onPress -> {
              if (onRemove != null) {
                onRemove.remove(conditionDataEntry);
              }
            });
  }

  @Override
  public Component getNarration() {
    return TextComponent.getText(this.conditionType.name() + ":" + this.conditionDataEntry.name());
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    super.mouseClicked(mouseX, mouseY, button);
    this.editButton.mouseClicked(mouseX, mouseY, button);
    this.deleteButton.mouseClicked(mouseX, mouseY, button);
    return button == 0;
  }

  @Override
  public void render(
      GuiGraphics guiGraphics,
      int entryId,
      int top,
      int left,
      int entryWidth,
      int entryHeight,
      int mouseX,
      int mouseY,
      boolean isSelected,
      float partialTicks) {

    // Draw separator line
    guiGraphics.fill(
        this.leftPos,
        top + entryHeight + 2,
        this.leftPos + LIST_WIDTH,
        top + entryHeight + 3,
        COLOR_SEPARATOR_LINE);

    int fieldsLeft = this.leftPos + FIELD_LEFT_OFFSET;
    int fieldTop = top + FIELD_TOP_OFFSET;

    // Condition Entry ID
    Text.drawString(
        guiGraphics,
        this.font,
        String.valueOf(entryId),
        fieldsLeft + ID_LEFT_POS + 2,
        fieldTop,
        Constants.FONT_COLOR_BLACK);

    // Condition Type
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "conditionType." + this.conditionType.name().toLowerCase(),
        fieldsLeft + TYPE_LEFT_POS + 2,
        fieldTop,
        Constants.FONT_COLOR_BLACK);

    // Value preview
    renderValuePreview(guiGraphics, fieldsLeft, fieldTop);

    // Edit and delete buttons
    this.editButton.render(guiGraphics, mouseX, mouseY, partialTicks);
    this.editButton.setY(top);
    this.deleteButton.render(guiGraphics, mouseX, mouseY, partialTicks);
    this.deleteButton.setY(top);

    // Render separator lines
    this.renderSeparatorLines(guiGraphics, top);
  }

  private void renderValuePreview(GuiGraphics guiGraphics, int fieldsLeft, int fieldTop) {
    String valuePreview =
        switch (this.conditionType) {
          case SCOREBOARD ->
              TextUtils.limitString(
                  this.conditionDataEntry.name()
                      + " "
                      + this.conditionDataEntry.operationType().name()
                      + " "
                      + this.conditionDataEntry.value(),
                  VALUE_MAX_LENGTH);
          case EXECUTION_LIMIT ->
              TextUtils.limitString(
                  this.conditionDataEntry.value() + " (" + this.conditionDataEntry.text() + ")",
                  VALUE_MAX_LENGTH);
          default -> "-";
        };
    Text.drawString(
        guiGraphics,
        this.font,
        valuePreview,
        fieldsLeft + VALUE_LEFT_POS + 2,
        fieldTop,
        Constants.FONT_COLOR_BLACK);
  }

  public void renderSeparatorLines(GuiGraphics guiGraphics, int top) {
    // Draw vertical separator line for headers
    int separatorTop = top - 1;
    int separatorLeft = this.leftPos + FIELD_LEFT_OFFSET;
    guiGraphics.fill(
        separatorLeft + TYPE_LEFT_POS - COLUMN_SEPARATOR_OFFSET,
        separatorTop,
        separatorLeft + TYPE_LEFT_POS - COLUMN_SEPARATOR_OFFSET + COLUMN_SEPARATOR_WIDTH,
        separatorTop + ENTRY_HEIGHT,
        COLOR_COLUMN_SEPARATOR);
    guiGraphics.fill(
        separatorLeft + VALUE_LEFT_POS - COLUMN_SEPARATOR_OFFSET,
        separatorTop,
        separatorLeft + VALUE_LEFT_POS - COLUMN_SEPARATOR_OFFSET + COLUMN_SEPARATOR_WIDTH,
        separatorTop + ENTRY_HEIGHT,
        COLOR_COLUMN_SEPARATOR);
    guiGraphics.fill(
        separatorLeft + OPTIONS_LEFT_POS - COLUMN_SEPARATOR_OFFSET,
        separatorTop,
        separatorLeft + OPTIONS_LEFT_POS - COLUMN_SEPARATOR_OFFSET + COLUMN_SEPARATOR_WIDTH,
        separatorTop + ENTRY_HEIGHT,
        COLOR_COLUMN_SEPARATOR);
  }

  public interface OnRemove {
    void remove(ConditionDataEntry conditionDataEntry);
  }

  public interface OnEdit {
    void edit(ConditionDataEntry conditionDataEntry);
  }
}
