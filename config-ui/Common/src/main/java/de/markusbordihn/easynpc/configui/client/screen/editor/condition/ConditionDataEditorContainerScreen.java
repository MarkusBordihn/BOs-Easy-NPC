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

import de.markusbordihn.easynpc.client.screen.components.AddButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.EditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.components.DialogButton;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class ConditionDataEditorContainerScreen<T extends EditorMenu> extends EditorScreen<T> {

  // Layout constants
  private static final int HOME_BUTTON_X_OFFSET = 3;
  private static final int HOME_BUTTON_Y_OFFSET = 3;
  private static final int HOME_BUTTON_WIDTH = 10;
  private static final int HOME_BUTTON_HEIGHT = 16;
  private static final int NAVIGATION_BUTTON_WIDTH = 140;
  private static final int LIST_X_OFFSET = 5;
  private static final int LIST_Y_START = 40;
  private static final int LIST_Y_END = 200;
  private static final int LIST_TOTAL_WIDTH = 314;
  private static final int HEADER_Y_OFFSET = 25;
  private static final int HEADER_HEIGHT = 18;
  private static final int ENTRY_HEIGHT = 21;
  private static final int FOOTER_Y_OFFSET = 210;
  private static final int FOOTER_HEIGHT = 31;
  private static final int ADD_BUTTON_X_OFFSET = 7;
  private static final int ADD_BUTTON_WIDTH = 300;

  // Color constants
  private static final int COLOR_LIST_BACKGROUND = 0xffeeeeee;
  private static final int COLOR_HEADER_BACKGROUND = 0xffaaaaaa;
  private static final int COLOR_FOOTER_BACKGROUND = 0xffc6c6c6;
  private static final int COLOR_SEPARATOR = 0xff666666;

  private final ConditionDataSet conditionDataSet;
  protected Button homeButton;
  protected Button dialogButton;
  protected Button conditionsButton;
  protected Button newConditionDataEntryButton;
  ConditionDataList conditionDataList;

  public ConditionDataEditorContainerScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.conditionDataSet = getConditionDataSet();
  }

  private ConditionDataSet getConditionDataSet() {
    DialogDataEntry dialogData = this.getDialogData();
    if (dialogData != null && dialogData.getConditions() != null) {
      ConditionDataSet conditionDataSet = new ConditionDataSet();
      for (ConditionDataEntry entry : dialogData.getConditions()) {
        conditionDataSet.add(entry);
      }
      return conditionDataSet;
    }
    log.error("No valid condition data set found!");
    return new ConditionDataSet();
  }

  @Override
  public void init() {
    super.init();

    // Home Button
    this.homeButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + HOME_BUTTON_X_OFFSET,
                this.topPos + HOME_BUTTON_Y_OFFSET,
                HOME_BUTTON_WIDTH,
                HOME_BUTTON_HEIGHT,
                "<",
                onPress -> handleBackNavigation()));

    // Dialog Button
    this.dialogButton =
        this.addRenderableWidget(
            new DialogButton(
                this.homeButton.getX() + this.homeButton.getWidth(),
                this.topPos + HOME_BUTTON_Y_OFFSET,
                NAVIGATION_BUTTON_WIDTH,
                this.getDialogData().getName(21),
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openDialogEditor(this.getEasyNPCUUID(), this.getDialogUUID())));

    // Conditions Button (inactive)
    this.conditionsButton =
        this.addRenderableWidget(
            new DialogButton(
                this.dialogButton.getX() + this.dialogButton.getWidth(),
                this.topPos + HOME_BUTTON_Y_OFFSET,
                NAVIGATION_BUTTON_WIDTH,
                "Conditions",
                onPress -> {}));
    this.conditionsButton.active = false;

    // New Condition Data Entry Button
    this.newConditionDataEntryButton =
        this.addRenderableWidget(
            new AddButton(
                this.leftPos + ADD_BUTTON_X_OFFSET,
                this.topPos + FOOTER_Y_OFFSET,
                ADD_BUTTON_WIDTH,
                "condition.add",
                onPress -> handleNewConditionDataEntry()));

    // Condition Data List
    this.conditionDataList =
        new ConditionDataList(
            this.conditionDataSet,
            this.minecraft,
            this.width + 50,
            this.height - 60,
            this.leftPos + LIST_X_OFFSET,
            this.topPos + LIST_Y_START,
            this.topPos + LIST_Y_END,
            ENTRY_HEIGHT,
            this::handleEditConditionDataEntry,
            this::handleDeleteConditionDataEntry);
    this.addWidget(this.conditionDataList);
  }

  private void handleBackNavigation() {
    NetworkMessageHandlerManager.getServerHandler()
        .openDialogEditor(this.getEasyNPCUUID(), this.getDialogUUID());
  }

  private void handleNewConditionDataEntry() {
    NetworkMessageHandlerManager.getServerHandler()
        .openConditionDataEntryEditor(
            this.getEasyNPCUUID(),
            this.getDialogUUID(),
            new ConditionDataEntry(ConditionType.SCOREBOARD));
  }

  private void handleDeleteConditionDataEntry(ConditionDataEntry conditionDataEntry) {
    if (this.minecraft == null
        || this.conditionDataSet == null
        || conditionDataEntry == null
        || conditionDataEntry.getId().equals(Constants.EMPTY_UUID)) {
      return;
    }
    this.minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed) {
                this.conditionDataSet.remove(conditionDataEntry.getId());
                updateConditionDataSet();
                navigateToConditionDataEditor();
              } else {
                this.minecraft.setScreen(this);
              }
            },
            TextComponent.getTranslatedConfigText("removeConditionDataEntry.deleteQuestion"),
            TextComponent.getTranslatedConfigText(
                "removeConditionDataEntry.deleteWarning",
                conditionDataEntry.conditionType().name()),
            TextComponent.getTranslatedConfigText("removeConditionDataEntry.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  private void updateConditionDataSet() {
    DialogDataEntry dialogData = this.getDialogData();
    dialogData.setConditions(this.conditionDataSet.getConditions());
    NetworkMessageHandlerManager.getServerHandler()
        .saveDialog(this.getEasyNPCUUID(), this.getDialogUUID(), dialogData);
  }

  private void handleEditConditionDataEntry(ConditionDataEntry conditionDataEntry) {
    log.info("Editing Condition Data Entry {}: {}", conditionDataEntry.getId(), conditionDataEntry);
    NetworkMessageHandlerManager.getServerHandler()
        .openConditionDataEntryEditor(
            this.getEasyNPCUUID(), this.getDialogUUID(), conditionDataEntry);
  }

  private void navigateToConditionDataEditor() {
    NetworkMessageHandlerManager.getServerHandler()
        .openConditionDataEditor(this.getEasyNPCUUID(), this.getDialogUUID());
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Gray background for condition list
    guiGraphics.fill(
        this.leftPos + LIST_X_OFFSET,
        this.topPos + HEADER_Y_OFFSET + 5,
        this.leftPos + LIST_TOTAL_WIDTH,
        this.topPos + LIST_Y_END,
        COLOR_LIST_BACKGROUND);

    // Render Condition Data List
    if (this.conditionDataList != null) {
      this.conditionDataList.render(guiGraphics, x, y, partialTicks);
    }

    // Render Header
    renderHeader(guiGraphics);

    // Footer background
    renderFooter(guiGraphics);

    // Re-render button for visibility
    if (this.newConditionDataEntryButton != null) {
      this.newConditionDataEntryButton.render(guiGraphics, x, y, partialTicks);
    }
  }

  private void renderHeader(GuiGraphics guiGraphics) {
    // Header background
    guiGraphics.fill(
        this.leftPos + LIST_X_OFFSET,
        this.topPos + HEADER_Y_OFFSET,
        this.leftPos + LIST_TOTAL_WIDTH,
        this.topPos + HEADER_Y_OFFSET + HEADER_HEIGHT,
        COLOR_HEADER_BACKGROUND);

    // Header labels
    int headerLeft = this.leftPos + 10;
    int headerTop = this.topPos + HEADER_Y_OFFSET + 5;
    Text.drawString(
        guiGraphics,
        this.font,
        "ID",
        headerLeft + ConditionDataListEntry.ID_LEFT_POS,
        headerTop,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "type",
        headerLeft + ConditionDataListEntry.TYPE_LEFT_POS,
        headerTop,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "value",
        headerLeft + ConditionDataListEntry.VALUE_LEFT_POS,
        headerTop,
        Constants.FONT_COLOR_BLACK);
    Text.drawString(
        guiGraphics,
        this.font,
        "Action",
        headerLeft + ConditionDataListEntry.OPTIONS_LEFT_POS,
        headerTop,
        Constants.FONT_COLOR_BLACK);

    // Draw vertical separator line for headers
    int separatorTop = headerTop - 5;
    guiGraphics.fill(
        headerLeft + ConditionDataListEntry.TYPE_LEFT_POS - 3,
        separatorTop,
        headerLeft + ConditionDataListEntry.TYPE_LEFT_POS - 2,
        separatorTop + HEADER_HEIGHT,
        COLOR_SEPARATOR);
    guiGraphics.fill(
        headerLeft + ConditionDataListEntry.VALUE_LEFT_POS - 3,
        separatorTop,
        headerLeft + ConditionDataListEntry.VALUE_LEFT_POS - 2,
        separatorTop + HEADER_HEIGHT,
        COLOR_SEPARATOR);
    guiGraphics.fill(
        headerLeft + ConditionDataListEntry.OPTIONS_LEFT_POS - 3,
        separatorTop,
        headerLeft + ConditionDataListEntry.OPTIONS_LEFT_POS - 2,
        separatorTop + HEADER_HEIGHT,
        COLOR_SEPARATOR);
  }

  private void renderFooter(GuiGraphics guiGraphics) {
    // Footer background
    guiGraphics.fill(
        this.leftPos + LIST_X_OFFSET,
        this.topPos + LIST_Y_END,
        this.leftPos + LIST_TOTAL_WIDTH,
        this.topPos + LIST_Y_END + FOOTER_HEIGHT,
        COLOR_FOOTER_BACKGROUND);
  }
}
