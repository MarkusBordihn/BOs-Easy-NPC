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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.EditorScreen;
import de.markusbordihn.easynpc.client.screen.components.ActionsButton;
import de.markusbordihn.easynpc.client.screen.components.AddButton;
import de.markusbordihn.easynpc.client.screen.components.DialogButtonButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class ActionDataEditorContainerScreen<T extends EditorMenu> extends EditorScreen<T> {

  private final ActionDataSet actionDataSet;
  private final ActionEventType actionEventType;
  private final ConfigurationType configurationType;
  private final EditorType editorType;
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
    this.actionDataSet = getActionDataSet();
  }

  private ActionDataSet getActionDataSet() {
    if (this.actionEventType != null && this.actionEventType != ActionEventType.NONE) {
      return this.getAdditionalScreenData().getActionEventSet().getActionEvents(actionEventType);
    } else if (this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON) {
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
                this.leftPos + 3, this.topPos + 3, 10, 18, "<", onPress -> handleBackNavigation()));

    // Level 1 Navigation Buttons
    if (this.actionEventType != null && this.actionEventType != ActionEventType.NONE) {
      this.navigationLevelOne =
          this.addRenderableWidget(
              new ActionsButton(
                  this.homeButton.getX() + this.homeButton.getWidth(),
                  this.topPos + 3,
                  140,
                  this.actionEventType.name(),
                  onPress -> navigateToActionDataEditor()));
      this.navigationLevelOne.active = false;
    } else if (this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON) {
      this.navigationLevelOne =
          this.addRenderableWidget(
              new DialogButtonButton(
                  this.homeButton.getX() + this.homeButton.getWidth(),
                  this.topPos + 3,
                  140,
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
                  this.topPos + 3,
                  140,
                  "Actions",
                  onPress -> navigateToActionDataEditor()));
      this.navigationLevelOne.active = false;
    }

    // Level 2 Navigation Buttons
    if (this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON) {
      this.navigationLevelTwo =
          this.addRenderableWidget(
              new ActionsButton(
                  this.navigationLevelOne.getX() + this.navigationLevelOne.getWidth(),
                  this.topPos + 3,
                  140,
                  "Actions",
                  onPress -> navigateToActionDataEditor()));
      this.navigationLevelTwo.active = false;
    }

    // New Action Data Entry Button
    this.newActionDataEntryButton =
        this.addRenderableWidget(
            new AddButton(
                this.leftPos + 7,
                this.topPos + 210,
                300,
                "action.add",
                onPress -> handleNewActionDataEntry()));

    // Action Data List
    this.actionDataList =
        new ActionDataList(
            this.actionDataSet,
            this.minecraft,
            this.width + 50,
            this.height - 60,
            this.leftPos + 5,
            this.topPos + 40,
            this.topPos + 200,
            21,
            this::handleMoveUpOrderActionDataEntry,
            this::handleMoveDownOrderActionDataEntry,
            this::handleEditActionDataEntry,
            this::handleDeleteActionDataEntry);
    this.addWidget(this.actionDataList);
  }

  private void navigateToActionDataEditor() {
    NetworkMessageHandlerManager.getServerHandler()
        .openActionDataEditor(this.getEasyNPCUUID(), actionEventType, configurationType);
  }

  private void handleBackNavigation() {
    if (configurationType != null && configurationType != ConfigurationType.NONE) {
      NetworkMessageHandlerManager.getServerHandler()
          .openConfiguration(this.getEasyNPCUUID(), configurationType);
    } else if (editorType != null
        && editorType == EditorType.DIALOG_BUTTON
        && this.getDialogUUID() != null
        && this.getDialogButtonUUID() != null) {
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
    if (configurationType != null && configurationType != ConfigurationType.NONE) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionDataEntryEditor(
              this.getEasyNPCUUID(),
              this.actionEventType,
              this.configurationType,
              new ActionDataEntry());
    } else if (this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON) {
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
        || actionDataEntry.getId() == Constants.EMPTY_UUID) {
      return;
    }
    this.minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed) {
                this.actionDataSet.remove(actionDataEntry.getId());
                if (this.actionEventType != null && this.actionEventType != ActionEventType.NONE) {
                  NetworkMessageHandlerManager.getServerHandler()
                      .actionEventChange(
                          this.getEasyNPCUUID(), this.actionEventType, this.actionDataSet);
                } else if (this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON) {
                  NetworkMessageHandlerManager.getServerHandler()
                      .saveDialogButton(
                          this.getEasyNPCUUID(),
                          this.getDialogUUID(),
                          this.getDialogButtonUUID(),
                          this.getDialogButtonData().withActionDataSet(this.actionDataSet));
                } else {
                  log.error(
                      "Unable to delete Action Data Set {} for {}!",
                      this.actionDataSet,
                      this.getEasyNPCUUID());
                }
                this.navigateToActionDataEditor();
              } else {
                this.minecraft.setScreen(this);
              }
            },
            Component.translatable(Constants.TEXT_PREFIX + "removeActionDataEntry.deleteQuestion"),
            Component.translatable(
                Constants.TEXT_PREFIX + "removeActionDataEntry.deleteWarning",
                actionDataEntry.getType()),
            Component.translatable(Constants.TEXT_PREFIX + "removeActionDataEntry.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  private void handleEditActionDataEntry(ActionDataEntry actionDataEntry) {
    log.info("Editing Action Data Entry {}: {}", actionDataEntry.getId(), actionDataEntry);
    if (this.actionEventType != null && this.actionEventType != ActionEventType.NONE) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionDataEntryEditor(
              this.getEasyNPCUUID(), this.actionEventType, this.configurationType, actionDataEntry);
    } else if (this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON) {
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
    if (this.actionEventType != null && this.actionEventType != ActionEventType.NONE) {
      this.actionDataSet.moveUp(actionDataEntry);
      NetworkMessageHandlerManager.getServerHandler()
          .actionEventChange(this.getEasyNPCUUID(), this.actionEventType, this.actionDataSet);
    } else if (this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON) {
      this.actionDataSet.moveUp(actionDataEntry);
      NetworkMessageHandlerManager.getServerHandler()
          .saveDialogButton(
              this.getEasyNPCUUID(),
              this.getDialogUUID(),
              this.getDialogButtonUUID(),
              this.getDialogButtonData().withActionDataSet(this.actionDataSet));
    } else {
      log.error(
          "Unable to move up Action Data Set {} for {}!",
          this.actionDataSet,
          this.getEasyNPCUUID());
    }
    this.navigateToActionDataEditor();
  }

  private void handleMoveDownOrderActionDataEntry(ActionDataEntry actionDataEntry) {
    log.info("Moving down Action Data Entry {}: {}", actionDataEntry.getId(), actionDataEntry);
    if (this.actionEventType != null && this.actionEventType != ActionEventType.NONE) {
      this.actionDataSet.moveDown(actionDataEntry);
      NetworkMessageHandlerManager.getServerHandler()
          .actionEventChange(this.getEasyNPCUUID(), this.actionEventType, this.actionDataSet);
    } else if (this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON) {
      this.actionDataSet.moveDown(actionDataEntry);
      NetworkMessageHandlerManager.getServerHandler()
          .saveDialogButton(
              this.getEasyNPCUUID(),
              this.getDialogUUID(),
              this.getDialogButtonUUID(),
              this.getDialogButtonData().withActionDataSet(this.actionDataSet));
    } else {
      log.error(
          "Unable to move down Action Data Set {} for {}!",
          this.actionDataSet,
          this.getEasyNPCUUID());
    }
    this.navigateToActionDataEditor();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Gray background for dialog list
    guiGraphics.fill(
        this.leftPos + 5, this.topPos + 30, this.leftPos + 314, this.topPos + 200, 0xffeeeeee);

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
        this.leftPos + 5, this.topPos + 25, this.leftPos + 314, this.topPos + 43, 0xffaaaaaa);

    // Header labels
    int headerLeft = this.leftPos + 10;
    int headerTop = this.topPos + 30;
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
        separatorTop + 18,
        0xff666666);
    guiGraphics.fill(
        headerLeft + ActionDataListEntry.VALUE_LEFT_POS - 3,
        separatorTop,
        headerLeft + ActionDataListEntry.VALUE_LEFT_POS - 2,
        separatorTop + 18,
        0xff666666);
    guiGraphics.fill(
        headerLeft + ActionDataListEntry.OPTIONS_LEFT_POS - 3,
        separatorTop,
        headerLeft + ActionDataListEntry.OPTIONS_LEFT_POS - 2,
        separatorTop + 18,
        0xff666666);
  }

  private void renderFooter(GuiGraphics guiGraphics) {
    // Footer background
    guiGraphics.fill(
        this.leftPos + 5, this.topPos + 200, this.leftPos + 314, this.topPos + 231, 0xffc6c6c6);
  }
}
