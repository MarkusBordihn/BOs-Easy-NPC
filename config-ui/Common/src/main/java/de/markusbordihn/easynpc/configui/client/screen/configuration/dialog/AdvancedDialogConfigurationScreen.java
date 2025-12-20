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

package de.markusbordihn.easynpc.configui.client.screen.configuration.dialog;

import de.markusbordihn.easynpc.client.screen.components.AddButton;
import de.markusbordihn.easynpc.client.screen.components.CopyButton;
import de.markusbordihn.easynpc.client.screen.components.EditButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextEditButton;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogPriority;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.Comparator;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.ObjectSelectionList;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class AdvancedDialogConfigurationScreen<T extends ConfigurationMenu>
    extends DialogConfigurationScreen<T> {

  private static final int COLUMN_PRIORITY_WIDTH = 25;
  private static final int COLUMN_LABEL_WIDTH = 95;
  private static final int COLUMN_NAME_WIDTH = 96;
  private static final int COLUMN_TEXT_WIDTH = 96;

  private static final int COLUMN_PRIORITY_START = 4;
  private static final int COLUMN_LABEL_START = COLUMN_PRIORITY_START + COLUMN_PRIORITY_WIDTH + 4;
  private static final int COLUMN_NAME_START = COLUMN_LABEL_START + COLUMN_LABEL_WIDTH;
  private static final int COLUMN_TEXT_START = COLUMN_NAME_START + COLUMN_NAME_WIDTH;

  private static final int LIST_AREA_TOP_OFFSET = 20;
  private static final int LIST_AREA_BOTTOM = 190;
  private static final int HEADER_HEIGHT = 18;
  private static final int FOOTER_HEIGHT = 20;

  private static final float TEXT_SCALE = 0.75f;

  Button newDialogButton;

  DialogList dialogList;

  public AdvancedDialogConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.advancedDialogButton.active = false;

    // Add new dialog button
    this.newDialogButton =
        this.addRenderableWidget(
            new AddButton(
                this.contentLeftPos + 4,
                this.contentTopPos + 193,
                300,
                "dialog.add",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openDialogEditor(this.getEasyNPCUUID())));

    // Dialog List
    this.dialogList = new DialogList();
    this.addWidget(this.dialogList);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    int listLeft = this.leftPos + COLUMN_PRIORITY_START;
    int listRight = this.leftPos + COLUMN_TEXT_START + COLUMN_TEXT_WIDTH + 4;
    int listTop = this.contentTopPos + LIST_AREA_TOP_OFFSET;
    int listBottom = this.contentTopPos + LIST_AREA_BOTTOM + FOOTER_HEIGHT;

    // Gray background for dialog list
    guiGraphics.fill(listLeft, listTop, listRight, listBottom, 0xffeeeeee);

    // Draw vertical separator lines for entries
    guiGraphics.fill(
        this.leftPos + COLUMN_LABEL_START - 1,
        listTop,
        this.leftPos + COLUMN_LABEL_START,
        this.contentTopPos + LIST_AREA_BOTTOM,
        0xffbbbbbb);
    guiGraphics.fill(
        this.leftPos + COLUMN_NAME_START,
        listTop,
        this.leftPos + COLUMN_NAME_START + 1,
        this.contentTopPos + LIST_AREA_BOTTOM,
        0xffbbbbbb);
    guiGraphics.fill(
        this.leftPos + COLUMN_TEXT_START - 1,
        listTop,
        this.leftPos + COLUMN_TEXT_START,
        this.contentTopPos + LIST_AREA_BOTTOM,
        0xffbbbbbb);

    // Render dialog list
    if (this.dialogList != null) {
      this.dialogList.renderSelectionList(guiGraphics, x, y, partialTicks);
    }

    // Header background
    guiGraphics.fill(
        listLeft, this.contentTopPos, listRight, this.contentTopPos + HEADER_HEIGHT, 0xffaaaaaa);

    // Footer background
    guiGraphics.fill(
        listLeft, this.contentTopPos + LIST_AREA_BOTTOM + 1, listRight, listBottom, 0xffc6c6c6);

    // Dialog Data Set header
    int headerLeft = this.leftPos + COLUMN_PRIORITY_START + 5;
    Text.drawString(
        guiGraphics,
        this.font,
        "Prio",
        headerLeft,
        this.contentTopPos + 5,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "label_id",
        this.leftPos + COLUMN_LABEL_START + 3,
        this.contentTopPos + 5,
        Constants.FONT_COLOR_BLACK);
    Text.drawString(
        guiGraphics,
        this.font,
        "Name",
        this.leftPos + COLUMN_NAME_START + 3,
        this.contentTopPos + 5,
        Constants.FONT_COLOR_BLACK);
    Text.drawString(
        guiGraphics,
        this.font,
        "Text",
        this.leftPos + COLUMN_TEXT_START + 2,
        this.contentTopPos + 5,
        Constants.FONT_COLOR_BLACK);

    // Draw vertical separator lines for headers
    guiGraphics.fill(
        this.leftPos + COLUMN_LABEL_START - 1,
        this.contentTopPos,
        this.leftPos + COLUMN_LABEL_START,
        this.contentTopPos + HEADER_HEIGHT,
        0xff666666);
    guiGraphics.fill(
        this.leftPos + COLUMN_NAME_START,
        this.contentTopPos,
        this.leftPos + COLUMN_NAME_START + 1,
        this.contentTopPos + HEADER_HEIGHT,
        0xff666666);
    guiGraphics.fill(
        this.leftPos + COLUMN_TEXT_START - 1,
        this.contentTopPos,
        this.leftPos + COLUMN_TEXT_START,
        this.contentTopPos + HEADER_HEIGHT,
        0xff666666);

    // Re-render button for visibility
    if (this.newDialogButton != null) {
      this.newDialogButton.render(guiGraphics, x, y, partialTicks);
    }
  }

  class DialogList
      extends ObjectSelectionList<AdvancedDialogConfigurationScreen<?>.DialogList.Entry> {
    DialogList() {
      super(
          AdvancedDialogConfigurationScreen.this.minecraft,
          AdvancedDialogConfigurationScreen.this.width + 60,
          177,
          AdvancedDialogConfigurationScreen.this.contentTopPos + 15,
          19);
      this.setRenderHeader(false, 0);

      // Add all dialog data sets, sorted by priority (descending) then by label
      AdvancedDialogConfigurationScreen.this.getDialogDataSet().getDialogsByLabel().stream()
          .filter(dialogData -> dialogData != null && dialogData.getId() != null)
          .sorted(
              Comparator.comparingInt(DialogDataEntry::getPriority)
                  .reversed()
                  .thenComparing(Comparator.comparing(DialogDataEntry::getLabel)))
          .forEach(
              dialogData ->
                  this.addEntry(
                      new AdvancedDialogConfigurationScreen<?>.DialogList.Entry(dialogData)));
    }

    public void renderSelectionList(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
      if (this.getItemCount() > 0) {
        super.render(guiGraphics, x, y, partialTicks);
      }
    }

    @Override
    protected void renderSelection(
        GuiGraphics guiGraphics, int unused1, int unused2, int unused3, int unused4, int unused5) {
      // Do not render selection.
    }

    @Override
    protected void renderListSeparators(GuiGraphics guiGraphics) {
      // Do not render list separators.
    }

    @Override
    protected void renderListBackground(GuiGraphics guiGraphics) {
      // Do not render list background.
    }

    class Entry
        extends ObjectSelectionList.Entry<AdvancedDialogConfigurationScreen<?>.DialogList.Entry> {

      final DialogDataEntry dialogData;
      final EditButton editButton;
      final CopyButton copyLabelButton;
      final TextEditButton textEditButton;

      public Entry(DialogDataEntry dialogData) {
        super();
        this.dialogData = dialogData;
        this.editButton =
            new EditButton(
                0,
                0,
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openDialogEditor(
                            AdvancedDialogConfigurationScreen.this.getEasyNPCUUID(),
                            this.dialogData.getId()));
        this.copyLabelButton =
            new CopyButton(
                0,
                0,
                onPress -> {
                  Minecraft minecraft = Minecraft.getInstance();
                  minecraft.keyboardHandler.setClipboard(dialogData.getLabel());
                });
        this.textEditButton =
            new TextEditButton(
                0,
                0,
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openDialogTextEditor(
                            AdvancedDialogConfigurationScreen.this.getEasyNPCUUID(),
                            this.dialogData.getId()));
      }

      @Override
      public Component getNarration() {
        return TextComponent.getTextComponent(dialogData.getName());
      }

      @Override
      public boolean mouseClicked(double mouseX, double mouseY, int button) {
        super.mouseClicked(mouseX, mouseY, button);
        this.copyLabelButton.mouseClicked(mouseX, mouseY, button);
        this.editButton.mouseClicked(mouseX, mouseY, button);
        this.textEditButton.mouseClicked(mouseX, mouseY, button);
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

        // Position
        int leftPos = left - 80;
        int buttonWidth = 16;

        // Render edit button and tooltip
        this.editButton.setX(leftPos + COLUMN_NAME_START - buttonWidth - 7);
        this.editButton.setY(top);
        this.editButton.render(guiGraphics, mouseX, mouseY, partialTicks);
        if (this.editButton.isHovered()) {
          guiGraphics.renderTooltip(
              AdvancedDialogConfigurationScreen.this.font,
              TextComponent.getTranslatedConfigText("dialog.edit_dialog", dialogData.getName()),
              mouseX,
              mouseY);
        }

        // Render copy button and tooltip
        this.copyLabelButton.setX(this.editButton.getX() - this.editButton.getWidth());
        this.copyLabelButton.setY(top);
        this.copyLabelButton.render(guiGraphics, mouseX, mouseY, partialTicks);
        if (this.copyLabelButton.isHovered()) {
          guiGraphics.renderTooltip(
              AdvancedDialogConfigurationScreen.this.font,
              TextComponent.getTranslatedConfigText(
                  "dialog.copy_dialog_label", dialogData.getLabel()),
              mouseX,
              mouseY);
        }

        // Render edit text button and tooltip
        this.textEditButton.setX(leftPos + COLUMN_TEXT_START - 5);
        this.textEditButton.setY(top);
        this.textEditButton.render(guiGraphics, mouseX, mouseY, partialTicks);
        if (this.textEditButton.isHovered()) {
          guiGraphics.renderTooltip(
              AdvancedDialogConfigurationScreen.this.font,
              TextComponent.getTranslatedConfigText(
                  "dialog.edit_dialog_text", dialogData.getText()),
              mouseX,
              mouseY);
        }

        int dialogDataTopPos = Math.round((top + 5) / TEXT_SCALE);
        int fontColor =
            switch (dialogData.getPriority()) {
              case DialogPriority.CRITICAL -> Constants.FONT_COLOR_RED;
              case DialogPriority.HIGH -> Constants.FONT_COLOR_DARK_GREEN;
              case DialogPriority.NORMAL -> Constants.FONT_COLOR_BLACK;
              case DialogPriority.LOW -> Constants.FONT_COLOR_GRAY;
              case DialogPriority.FALLBACK -> Constants.FONT_COLOR_LIGHT_GRAY;
              case DialogPriority.MANUAL_ONLY -> Constants.FONT_COLOR_GRAY;
              default -> Constants.FONT_COLOR_BLACK;
            };

        guiGraphics.pose().pushPose();
        guiGraphics.pose().scale(TEXT_SCALE, TEXT_SCALE, TEXT_SCALE);
        Text.drawString(
            guiGraphics,
            AdvancedDialogConfigurationScreen.this.font,
            String.valueOf(dialogData.getPriority()),
            Math.round((leftPos + COLUMN_PRIORITY_START - 1) / TEXT_SCALE),
            dialogDataTopPos,
            fontColor);
        Text.drawString(
            guiGraphics,
            AdvancedDialogConfigurationScreen.this.font,
            dialogData.getLabel(14),
            Math.round((leftPos + COLUMN_LABEL_START - 4) / TEXT_SCALE),
            dialogDataTopPos,
            fontColor);
        Text.drawString(
            guiGraphics,
            AdvancedDialogConfigurationScreen.this.font,
            dialogData.getName(21),
            Math.round((leftPos + COLUMN_NAME_START - 2) / TEXT_SCALE),
            dialogDataTopPos,
            fontColor);
        Text.drawString(
            guiGraphics,
            AdvancedDialogConfigurationScreen.this.font,
            dialogData.getText(17),
            Math.round((leftPos + COLUMN_TEXT_START + 14) / TEXT_SCALE),
            dialogDataTopPos,
            fontColor);

        guiGraphics.pose().popPose();

        // Draw separator line
        int listLeft = AdvancedDialogConfigurationScreen.this.leftPos + COLUMN_PRIORITY_START;
        int listRight =
            AdvancedDialogConfigurationScreen.this.leftPos
                + COLUMN_TEXT_START
                + COLUMN_TEXT_WIDTH
                + 4;
        guiGraphics.fill(listLeft, top + 17, listRight, top + 18, 0xffaaaaaa);
      }
    }
  }
}
