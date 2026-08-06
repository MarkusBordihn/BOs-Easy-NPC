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

import de.markusbordihn.easynpc.client.screen.components.DrawBorder;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.configui.client.screen.components.EditButton;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.HandItemType;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.ObjectSelectionList;
import net.minecraft.client.input.MouseButtonEvent;
import net.minecraft.network.chat.Component;

public class ConditionDataListEntry extends ObjectSelectionList.Entry<ConditionDataListEntry> {

  public static final int ID_LEFT_POS = 0;
  public static final int TYPE_LEFT_POS = 22;
  public static final int VALUE_LEFT_POS = 110;
  public static final int OPTIONS_LEFT_POS = 250;

  private static final int ENTRY_HEIGHT = 21;
  private static final int FIELD_LEFT_OFFSET = 5;
  private static final int FIELD_TOP_OFFSET = 8;
  private static final int COLUMN_SEPARATOR_OFFSET = 3;
  private static final int BUTTON_SPACING = 2;
  private static final int BUTTON_SIZE = 18;
  private static final int VALUE_MAX_LENGTH = 21;
  private static final int LIST_WIDTH = 309;

  private static final int COLOR_SEPARATOR_LINE = 0xffaaaaaa;
  private static final int COLOR_COLUMN_SEPARATOR = 0xff666666;

  private final Font font;
  private final int leftPos;
  private final int topPos;
  private final ConditionDataEntry conditionDataEntry;
  private final ConditionType conditionType;
  private final EditButton editButton;
  private final DeleteButton deleteButton;
  private int entryIndex = 0;

  public ConditionDataListEntry(
      Minecraft minecraft,
      ConditionDataEntry conditionDataEntry,
      int leftPos,
      int topPos,
      OnEdit onEdit,
      OnRemove onRemove) {
    super();

    this.font = minecraft.font;
    this.leftPos = leftPos;
    this.topPos = topPos;

    this.conditionDataEntry = conditionDataEntry;
    this.conditionType =
        conditionDataEntry != null ? conditionDataEntry.conditionType() : ConditionType.NONE;

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

  public void setEntryIndex(int index) {
    this.entryIndex = index;
  }

  @Override
  public Component getNarration() {
    return TextComponent.getText(this.conditionType.name() + ":" + this.conditionDataEntry.name());
  }

  @Override
  public boolean mouseClicked(MouseButtonEvent mouseButtonEvent, boolean doubleClick) {
    super.mouseClicked(mouseButtonEvent, doubleClick);
    this.editButton.mouseClicked(mouseButtonEvent, doubleClick);
    this.deleteButton.mouseClicked(mouseButtonEvent, doubleClick);
    return mouseButtonEvent.button() == 0;
  }

  @Override
  public void renderContent(
      GuiGraphics guiGraphics, int mouseX, int mouseY, boolean isHovered, float partialTicks) {

    int top = this.getY();
    int entryHeight = this.getHeight();

    guiGraphics.fill(
        this.leftPos,
        top + entryHeight + 2,
        this.leftPos + LIST_WIDTH,
        top + entryHeight + 3,
        COLOR_SEPARATOR_LINE);

    int fieldsLeft = this.leftPos + FIELD_LEFT_OFFSET;
    int fieldTop = top + FIELD_TOP_OFFSET;

    Text.drawString(
        guiGraphics,
        this.font,
        String.valueOf(this.entryIndex),
        fieldsLeft + ID_LEFT_POS + 2,
        fieldTop,
        Constants.FONT_COLOR_BLACK);

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "conditionType." + this.conditionType.name().toLowerCase(),
        fieldsLeft + TYPE_LEFT_POS + 2,
        fieldTop,
        Constants.FONT_COLOR_BLACK);

    renderValuePreview(guiGraphics, fieldsLeft, fieldTop, mouseX, mouseY);

    int buttonTop = top + 4;
    this.editButton.setY(buttonTop);
    this.editButton.render(guiGraphics, mouseX, mouseY, partialTicks);
    this.deleteButton.setY(buttonTop);
    this.deleteButton.render(guiGraphics, mouseX, mouseY, partialTicks);

    this.renderSeparatorLines(guiGraphics, top);
  }

  private void renderValuePreview(
      GuiGraphics guiGraphics, int fieldsLeft, int fieldTop, int mouseX, int mouseY) {
    String fullValue =
        switch (this.conditionType) {
          case SCOREBOARD ->
              this.conditionDataEntry.name()
                  + " "
                  + this.conditionDataEntry.operationType().getSymbol()
                  + " "
                  + this.conditionDataEntry.value();
          case EXECUTION_LIMIT -> buildExecutionLimitPreview(this.conditionDataEntry);
          case HAS_ITEM_IN_HAND, HAS_ITEM_IN_INVENTORY -> buildItemPreview(this.conditionDataEntry);
          case ADVANCEMENT, PLAYER_TAG, TEAM, GAMEMODE -> this.conditionDataEntry.name();
          case EXPERIENCE_LEVEL, PLAYER_HEALTH, NPC_HEALTH, TIME_OF_DAY ->
              this.conditionDataEntry.operationType().getSymbol()
                  + " "
                  + this.conditionDataEntry.value();
          case WEATHER ->
              this.conditionDataEntry.subType() != null
                  ? ((Enum<?>) this.conditionDataEntry.subType()).name()
                  : "-";
          case RELATIONSHIP -> buildRelationshipPreview(this.conditionDataEntry);
          case ENTITY_HEALTH ->
              this.conditionDataEntry.operationType().getSymbol()
                  + " "
                  + this.conditionDataEntry.value()
                  + " ("
                  + this.conditionDataEntry.name()
                  + ")";
          default -> "-";
        };
    Text.drawLimitedHoverString(
        guiGraphics,
        this.font,
        fullValue,
        fieldsLeft + VALUE_LEFT_POS + 2,
        fieldTop,
        Constants.FONT_COLOR_BLACK,
        VALUE_MAX_LENGTH,
        mouseX,
        mouseY);
  }

  private String buildExecutionLimitPreview(ConditionDataEntry conditionDataEntry) {
    if (conditionDataEntry.subType() == null) {
      return String.valueOf(conditionDataEntry.value());
    }
    return conditionDataEntry.value()
        + " ("
        + ((Enum<?>) conditionDataEntry.subType()).name()
        + ")";
  }

  private String buildRelationshipPreview(ConditionDataEntry conditionDataEntry) {
    if (conditionDataEntry.subType() == null) {
      return "-";
    }

    String relationshipName = ((Enum<?>) conditionDataEntry.subType()).name();
    if (!conditionDataEntry.hasName()) {
      return relationshipName;
    }

    return relationshipName + " (" + conditionDataEntry.name() + ")";
  }

  private String buildItemPreview(ConditionDataEntry conditionDataEntry) {
    String prefix =
        conditionDataEntry.operationType() == ConditionOperationType.NOT_EQUALS ? "NOT " : "";
    String quantity = conditionDataEntry.value() > 1 ? conditionDataEntry.value() + " x " : "";
    String suffix = "";
    if (conditionDataEntry.subType() instanceof HandItemType handItemType) {
      suffix =
          switch (handItemType) {
            case MAIN_HAND -> " [M]";
            case OFF_HAND -> " [O]";
            default -> "";
          };
    }
    return prefix + quantity + conditionDataEntry.name() + suffix;
  }

  public void renderSeparatorLines(GuiGraphics guiGraphics, int top) {
    int separatorTop = top - 1;
    int separatorLeft = this.leftPos + FIELD_LEFT_OFFSET;
    DrawBorder.drawVerticalSeparator(
        guiGraphics,
        separatorLeft + TYPE_LEFT_POS - COLUMN_SEPARATOR_OFFSET,
        separatorTop,
        ENTRY_HEIGHT,
        COLOR_COLUMN_SEPARATOR);
    DrawBorder.drawVerticalSeparator(
        guiGraphics,
        separatorLeft + VALUE_LEFT_POS - COLUMN_SEPARATOR_OFFSET,
        separatorTop,
        ENTRY_HEIGHT,
        COLOR_COLUMN_SEPARATOR);
    DrawBorder.drawVerticalSeparator(
        guiGraphics,
        separatorLeft + OPTIONS_LEFT_POS - COLUMN_SEPARATOR_OFFSET,
        separatorTop,
        ENTRY_HEIGHT,
        COLOR_COLUMN_SEPARATOR);
  }

  public interface OnRemove {
    void remove(ConditionDataEntry conditionDataEntry);
  }

  public interface OnEdit {
    void edit(ConditionDataEntry conditionDataEntry);
  }
}
