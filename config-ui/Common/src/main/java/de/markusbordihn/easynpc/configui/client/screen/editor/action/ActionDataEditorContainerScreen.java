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

package de.markusbordihn.easynpc.configui.client.screen.editor.action;

import de.markusbordihn.easynpc.client.screen.components.AddButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.components.ActionsButton;
import de.markusbordihn.easynpc.configui.client.screen.components.DialogButtonButton;
import de.markusbordihn.easynpc.configui.data.editor.EditorType;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class ActionDataEditorContainerScreen<T extends EditorMenu>
    extends de.markusbordihn.easynpc.configui.client.screen.EditorScreen<T> {

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

  private final ActionDataSet actionDataSet;
  private final ActionEventType actionEventType;
  private final ConfigurationType configurationType;
  private final EditorType editorType;
  private final boolean isDialogButtonContext;
  private final boolean isActionEventContext;
  protected Button homeButton;
  protected Button navigationLevelOne;
  protected Button navigationLevelTwo;
  protected Button newActionDataEntryButton;
  ActionDataList actionDataList;

  public ActionDataEditorContainerScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.actionEventType = this.getAdditionalScreenData().getActionEventType();
    this.configurationType = this.getAdditionalScreenData().getConfigurationType();
    this.editorType = this.getAdditionalScreenData().getEditorType();
    this.isDialogButtonContext =
        this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON;
    this.isActionEventContext =
        this.actionEventType != null && this.actionEventType != ActionEventType.NONE;
    this.actionDataSet = getActionDataSet();
  }

  private ActionDataSet getActionDataSet() {
    if (this.isActionEventContext) {
      return this.getAdditionalScreenData().getActionEventSet().getActionEvents(actionEventType);
    } else if (this.isDialogButtonContext) {
      return this.getDialogButtonData().actionDataSet();
    } else {
      log.error("No valid action data set found!");
      return null;
    }
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

    // Level 1 Navigation Buttons
    if (this.isActionEventContext) {
      this.navigationLevelOne =
          this.addRenderableWidget(
              new ActionsButton(
                  this.homeButton.getX() + this.homeButton.getWidth(),
                  this.topPos + HOME_BUTTON_Y_OFFSET,
                  NAVIGATION_BUTTON_WIDTH,
                  this.actionEventType.name(),
                  onPress -> navigateToActionDataEditor()));
      this.navigationLevelOne.active = false;
    } else if (this.isDialogButtonContext) {
      this.navigationLevelOne =
          this.addRenderableWidget(
              new DialogButtonButton(
                  this.homeButton.getX() + this.homeButton.getWidth(),
                  this.topPos + HOME_BUTTON_Y_OFFSET,
                  NAVIGATION_BUTTON_WIDTH,
                  this.getDialogButtonData().getButtonName(21).getString(),
                  onPress ->
                      NetworkMessageHandlerManager.getServerHandler()
                          .openDialogButtonEditor(
                              this.getEasyNPCUUID(),
                              this.getDialogUUID(),
                              this.getDialogButtonUUID())));
    } else {
      this.navigationLevelOne =
          this.addRenderableWidget(
              new ActionsButton(
                  this.homeButton.getX() + this.homeButton.getWidth(),
                  this.topPos + HOME_BUTTON_Y_OFFSET,
                  NAVIGATION_BUTTON_WIDTH,
                  "Actions",
                  onPress -> navigateToActionDataEditor()));
      this.navigationLevelOne.active = false;
    }

    // Level 2 Navigation Buttons
    if (this.isDialogButtonContext) {
      this.navigationLevelTwo =
          this.addRenderableWidget(
              new ActionsButton(
                  this.navigationLevelOne.getX() + this.navigationLevelOne.getWidth(),
                  this.topPos + HOME_BUTTON_Y_OFFSET,
                  NAVIGATION_BUTTON_WIDTH,
                  "Actions",
                  onPress -> navigateToActionDataEditor()));
      this.navigationLevelTwo.active = false;
    }

    // New Action Data Entry Button
    this.newActionDataEntryButton =
        this.addRenderableWidget(
            new AddButton(
                this.leftPos + ADD_BUTTON_X_OFFSET,
                this.topPos + FOOTER_Y_OFFSET,
                ADD_BUTTON_WIDTH,
                "action.add",
                onPress -> handleNewActionDataEntry()));

    // Action Data List
    this.actionDataList =
        new ActionDataList(
            this.actionDataSet,
            this.minecraft,
            this.width + 50,
            this.height - 60,
            this.leftPos + LIST_X_OFFSET,
            this.topPos + LIST_Y_START,
            this.topPos + LIST_Y_END,
            ENTRY_HEIGHT,
            this::handleMoveUpOrderActionDataEntry,
            this::handleMoveDownOrderActionDataEntry,
            this::handleEditActionDataEntry,
            this::handleDeleteActionDataEntry);
    this.addWidget(this.actionDataList);
  }

  private void navigateToActionDataEditor() {
    if (configurationType != null && configurationType != ConfigurationType.NONE) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionDataEditor(this.getEasyNPCUUID(), actionEventType, configurationType);
    } else if (this.isDialogButtonContext) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionDataEditor(
              this.getEasyNPCUUID(), editorType, this.getDialogUUID(), this.getDialogButtonUUID());
    } else {
      log.error("No valid navigation found!");
      NetworkMessageHandlerManager.getServerHandler()
          .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.MAIN);
    }
  }

  private void handleBackNavigation() {
    if (configurationType != null && configurationType != ConfigurationType.NONE) {
      NetworkMessageHandlerManager.getServerHandler()
          .openConfiguration(this.getEasyNPCUUID(), configurationType);
    } else if (this.isDialogButtonContext) {
      NetworkMessageHandlerManager.getServerHandler()
          .openDialogButtonEditor(
              this.getEasyNPCUUID(), this.getDialogUUID(), this.getDialogButtonUUID());
    } else {
      log.error("No valid back navigation found!");
      NetworkMessageHandlerManager.getServerHandler()
          .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.MAIN);
    }
  }

  private void handleNewActionDataEntry() {
    if (this.isActionEventContext) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionDataEntryEditor(
              this.getEasyNPCUUID(),
              this.actionEventType,
              this.configurationType,
              new ActionDataEntry());
    } else if (this.isDialogButtonContext) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionDataEntryEditor(
              this.getEasyNPCUUID(),
              this.editorType,
              this.getDialogUUID(),
              this.getDialogButtonUUID(),
              new ActionDataEntry());
    } else {
      log.error("No valid new action data entry found!");
    }
  }

  private void handleDeleteActionDataEntry(ActionDataEntry actionDataEntry) {
    if (this.minecraft == null
        || this.actionDataSet == null
        || actionDataEntry == null
        || actionDataEntry.getId().equals(Constants.EMPTY_UUID)) {
      return;
    }
    this.minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed) {
                this.actionDataSet.remove(actionDataEntry.getId());
                updateActionDataSet();
                this.navigateToActionDataEditor();
              } else {
                this.minecraft.setScreen(this);
              }
            },
            TextComponent.getTranslatedConfigText("removeActionDataEntry.deleteQuestion"),
            TextComponent.getTranslatedConfigText(
                "removeActionDataEntry.deleteWarning", actionDataEntry.actionDataType().name()),
            TextComponent.getTranslatedConfigText("removeActionDataEntry.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  private void updateActionDataSet() {
    if (this.isActionEventContext) {
      NetworkMessageHandlerManager.getServerHandler()
          .actionEventChange(this.getEasyNPCUUID(), this.actionEventType, this.actionDataSet);
    } else if (this.isDialogButtonContext) {
      NetworkMessageHandlerManager.getServerHandler()
          .saveDialogButton(
              this.getEasyNPCUUID(),
              this.getDialogUUID(),
              this.getDialogButtonUUID(),
              this.getDialogButtonData().withActionDataSet(this.actionDataSet));
    } else {
      log.error(
          "Unable to update Action Data Set {} for {}!", this.actionDataSet, this.getEasyNPCUUID());
    }
  }

  private void handleEditActionDataEntry(ActionDataEntry actionDataEntry) {
    log.info("Editing Action Data Entry {}: {}", actionDataEntry.getId(), actionDataEntry);
    if (this.isActionEventContext) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionDataEntryEditor(
              this.getEasyNPCUUID(), this.actionEventType, this.configurationType, actionDataEntry);
    } else if (this.isDialogButtonContext) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionDataEntryEditor(
              this.getEasyNPCUUID(),
              this.editorType,
              this.getDialogUUID(),
              this.getDialogButtonUUID(),
              actionDataEntry);
    } else {
      log.error("Unable to edit action data entry {}!", actionDataEntry);
    }
  }

  private void handleMoveUpOrderActionDataEntry(ActionDataEntry actionDataEntry) {
    log.info("Moving up Action Data Entry {}: {}", actionDataEntry.getId(), actionDataEntry);
    this.actionDataSet.moveUp(actionDataEntry);
    updateActionDataSet();
    this.navigateToActionDataEditor();
  }

  private void handleMoveDownOrderActionDataEntry(ActionDataEntry actionDataEntry) {
    log.info("Moving down Action Data Entry {}: {}", actionDataEntry.getId(), actionDataEntry);
    this.actionDataSet.moveDown(actionDataEntry);
    updateActionDataSet();
    this.navigateToActionDataEditor();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Gray background for dialog list
    guiGraphics.fill(
        this.leftPos + LIST_X_OFFSET,
        this.topPos + HEADER_Y_OFFSET + 5,
        this.leftPos + LIST_TOTAL_WIDTH,
        this.topPos + LIST_Y_END,
        COLOR_LIST_BACKGROUND);

    // Render Action Data List
    if (this.actionDataList != null) {
      this.actionDataList.render(guiGraphics, x, y, partialTicks);
    }

    // Render Header
    renderHeader(guiGraphics);

    // Footer background
    renderFooter(guiGraphics);

    // Re-render button for visibility
    if (this.newActionDataEntryButton != null) {
      this.newActionDataEntryButton.render(guiGraphics, x, y, partialTicks);
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
        headerLeft + ActionDataListEntry.ID_LEFT_POS,
        headerTop,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "type",
        headerLeft + ActionDataListEntry.TYPE_LEFT_POS,
        headerTop,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "value",
        headerLeft + ActionDataListEntry.VALUE_LEFT_POS,
        headerTop,
        Constants.FONT_COLOR_BLACK);
    Text.drawString(
        guiGraphics,
        this.font,
        "Action",
        headerLeft + ActionDataListEntry.OPTIONS_LEFT_POS,
        headerTop,
        Constants.FONT_COLOR_BLACK);

    // Draw vertical separator line for headers
    int separatorTop = headerTop - 5;
    guiGraphics.fill(
        headerLeft + ActionDataListEntry.TYPE_LEFT_POS - 3,
        separatorTop,
        headerLeft + ActionDataListEntry.TYPE_LEFT_POS - 2,
        separatorTop + HEADER_HEIGHT,
        COLOR_SEPARATOR);
    guiGraphics.fill(
        headerLeft + ActionDataListEntry.VALUE_LEFT_POS - 3,
        separatorTop,
        headerLeft + ActionDataListEntry.VALUE_LEFT_POS - 2,
        separatorTop + HEADER_HEIGHT,
        COLOR_SEPARATOR);
    guiGraphics.fill(
        headerLeft + ActionDataListEntry.OPTIONS_LEFT_POS - 3,
        separatorTop,
        headerLeft + ActionDataListEntry.OPTIONS_LEFT_POS - 2,
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
