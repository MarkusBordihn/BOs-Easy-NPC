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

package de.markusbordihn.easynpc.client.screen.editor.action;

import static net.minecraft.client.gui.GuiComponent.fill;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.client.screen.components.EditButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.UpDownButton;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.utils.TextUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.components.ObjectSelectionList;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;

public class ActionDataListEntry extends ObjectSelectionList.Entry<ActionDataListEntry> {

  public static final int ID_LEFT_POS = 0;
  public static final int TYPE_LEFT_POS = 22;
  public static final int VALUE_LEFT_POS = 90;
  public static final int OPTIONS_LEFT_POS = 230;
  private final Font font;
  private final int leftPos;
  private final int topPos;
  private final int entryHeight = 21;
  private final ActionDataEntry actionDataEntry;
  private final ActionDataType actionDataType;
  private final int actionDateEntriesSize;
  private final EditButton editButton;
  private final DeleteButton deleteButton;
  private final UpDownButton upAndDownButton;

  public ActionDataListEntry(
      Minecraft minecraft,
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      int leftPos,
      int topPos,
      OnUp onUp,
      OnDown onDown,
      OnEdit onEdit,
      OnRemove onRemove) {
    super();

    // Set font and position
    this.font = minecraft.font;
    this.leftPos = leftPos;
    this.topPos = topPos;

    // Set action data entry
    this.actionDataEntry = actionDataEntry;
    this.actionDataType =
        actionDataEntry != null ? actionDataEntry.actionDataType() : ActionDataType.NONE;
    this.actionDateEntriesSize = actionDataSet != null ? actionDataSet.getEntries().size() : 1;

    // Adding general buttons
    this.upAndDownButton =
        new UpDownButton(
            this.leftPos + OPTIONS_LEFT_POS + 4,
            this.topPos,
            18,
            18,
            onPress -> {
              if (onUp != null) {
                onUp.changeOrder(actionDataEntry);
              }
            },
            onPress -> {
              if (onDown != null) {
                onDown.changeOrder(actionDataEntry);
              }
            });
    this.editButton =
        new EditButton(
            this.upAndDownButton.x + this.upAndDownButton.getWidth() + 2,
            this.topPos,
            18,
            18,
            onPress -> {
              if (onEdit != null) {
                onEdit.edit(actionDataEntry);
              }
            });
    this.deleteButton =
        new DeleteButton(
            this.editButton.x + this.editButton.getWidth() + 2,
            this.topPos,
            onPress -> {
              if (onRemove != null) {
                onRemove.remove(actionDataEntry);
              }
            });
  }

  @Override
  public Component getNarration() {
    return new TextComponent(this.actionDataType.name() + ":" + this.actionDataEntry.command());
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    super.mouseClicked(mouseX, mouseY, button);
    this.upAndDownButton.mouseClicked(mouseX, mouseY, button);
    this.editButton.mouseClicked(mouseX, mouseY, button);
    this.deleteButton.mouseClicked(mouseX, mouseY, button);
    return button == 0;
  }

  @Override
  public void render(
      PoseStack poseStack,
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
    fill(
        poseStack,
        this.leftPos,
        top + entryHeight + 2,
        this.leftPos + 309,
        top + entryHeight + 3,
        0xffaaaaaa);

    int fieldsLeft = this.leftPos + 5;

    // Action Entry ID
    Text.drawString(
        poseStack,
        this.font,
        String.valueOf(entryId),
        fieldsLeft + ID_LEFT_POS + 2,
        top + 5,
        Constants.FONT_COLOR_BLACK);

    // Action Type
    Text.drawConfigString(
        poseStack,
        this.font,
        this.actionDataType.getId(),
        fieldsLeft + TYPE_LEFT_POS + 2,
        top + 5,
        Constants.FONT_COLOR_BLACK);

    // Value preview
    if (this.actionDataType == ActionDataType.COMMAND
        || this.actionDataType == ActionDataType.OPEN_NAMED_DIALOG) {
      Text.drawString(
          poseStack,
          this.font,
          TextUtils.limitString(this.actionDataEntry.command(), 22),
          fieldsLeft + VALUE_LEFT_POS + 2,
          top + 5,
          Constants.FONT_COLOR_BLACK);
    } else if (this.actionDataType == ActionDataType.INTERACT_BLOCK) {
      Text.drawString(
          poseStack,
          this.font,
          TextUtils.limitString(this.actionDataEntry.blockPos().toString(), 22),
          fieldsLeft + VALUE_LEFT_POS + 2,
          top + 5,
          Constants.FONT_COLOR_BLACK);
    }

    // Up and down buttons
    this.upAndDownButton.render(poseStack, mouseX, mouseY, partialTicks);
    this.upAndDownButton.y = top;
    this.upAndDownButton.enableUpButton(entryId > 0);
    this.upAndDownButton.enableDownButton(entryId < this.actionDateEntriesSize - 1);

    // Edit and delete buttons
    this.editButton.render(poseStack, mouseX, mouseY, partialTicks);
    this.editButton.y = top;
    this.deleteButton.render(poseStack, mouseX, mouseY, partialTicks);
    this.deleteButton.y = top;

    // Render separator lines
    this.renderSeparatorLines(poseStack, top);
  }

  public void renderSeparatorLines(PoseStack poseStack, int top) {
    // Draw vertical separator line for headers
    int separatorTop = top - 1;
    int separatorLeft = this.leftPos + 5;
    fill(
        poseStack,
        separatorLeft + ActionDataListEntry.TYPE_LEFT_POS - 3,
        separatorTop,
        separatorLeft + ActionDataListEntry.TYPE_LEFT_POS - 2,
        separatorTop + entryHeight,
        0xff666666);
    fill(
        poseStack,
        separatorLeft + ActionDataListEntry.VALUE_LEFT_POS - 3,
        separatorTop,
        separatorLeft + ActionDataListEntry.VALUE_LEFT_POS - 2,
        separatorTop + entryHeight,
        0xff666666);
    fill(
        poseStack,
        separatorLeft + ActionDataListEntry.OPTIONS_LEFT_POS - 3,
        separatorTop,
        separatorLeft + ActionDataListEntry.OPTIONS_LEFT_POS - 2,
        separatorTop + entryHeight,
        0xff666666);
  }

  public interface OnRemove {
    void remove(ActionDataEntry actionDataEntry);
  }

  public interface OnEdit {
    void edit(ActionDataEntry actionDataEntry);
  }

  public interface OnUp {
    void changeOrder(ActionDataEntry actionDataEntry);
  }

  public interface OnDown {
    void changeOrder(ActionDataEntry actionDataEntry);
  }
}
