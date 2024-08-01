/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.client.screen.editor.action;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.EditorScreen;
import de.markusbordihn.easynpc.client.screen.components.ActionButton;
import de.markusbordihn.easynpc.client.screen.components.ActionsButton;
import de.markusbordihn.easynpc.client.screen.components.CancelButton;
import de.markusbordihn.easynpc.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.editor.action.entry.ActionEntryWidget;
import de.markusbordihn.easynpc.client.screen.editor.action.entry.CloseDialogEntry;
import de.markusbordihn.easynpc.client.screen.editor.action.entry.CommandActionEntry;
import de.markusbordihn.easynpc.client.screen.editor.action.entry.InteractBlockEntry;
import de.markusbordihn.easynpc.client.screen.editor.action.entry.OpenNamedDialogEntry;
import de.markusbordihn.easynpc.client.screen.editor.action.entry.OpenTradingScreenEntry;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.UUID;
import java.util.stream.Collectors;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.Widget;
import net.minecraft.client.gui.components.events.GuiEventListener;
import net.minecraft.client.gui.narration.NarratableEntry;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.player.Inventory;

public class ActionDataEntryEditorContainerScreen<T extends EditorMenu> extends EditorScreen<T> {

  private final ActionDataEntry actionDataEntry;
  private final ActionDataSet actionDataSet;
  private final ActionEventType actionEventType;
  private final ConfigurationType configurationType;
  private final EditorType editorType;
  private final UUID actionDataEntryId;
  protected Button actionDataTypeButton;
  protected Button cancelButton;
  protected Button deleteButton;
  protected Button homeButton;
  protected Button saveButton;
  protected Button navigationLevelOne;
  protected Button navigationLevelTwo;
  protected int contentTop;
  private ActionEntryWidget actionEntryWidget;
  private ActionDataType actionDataType;

  public ActionDataEntryEditorContainerScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.actionEventType = this.additionalScreenData.getActionEventType();
    this.configurationType = this.additionalScreenData.getConfigurationType();
    this.editorType = this.additionalScreenData.getEditorType();
    this.actionDataSet = getActionDataSet();

    // Action Data Entry
    this.actionDataEntryId = this.getActionDataEntryUUID();
    this.actionDataEntry = this.getActionDataEntry();
    this.actionDataType =
        this.actionDataEntry != ActionDataEntry.EMPTY
            ? this.actionDataEntry.getType()
            : ActionDataType.COMMAND;
  }

  private ActionDataSet getActionDataSet() {
    if (this.actionEventType != null && this.actionEventType != ActionEventType.NONE) {
      return this.additionalScreenData.getActionEventSet().getActionEvents(actionEventType);
    } else if (this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON) {
      return this.getDialogButtonData().getActionDataSet();
    }
    log.error("No valid action data set found for {}!", this.getEasyNPCUUID());
    return new ActionDataSet();
  }

  private ActionDataEntry getActionDataEntry() {
    if (this.actionDataSet != null
        && this.actionDataSet != ActionDataSet.EMPTY
        && this.actionDataEntryId != null
        && this.actionDataSet.contains(this.actionDataEntryId)) {
      return this.actionDataSet.getEntryOrDefault(this.actionDataEntryId);
    } else if (this.actionDataSet != null
        && this.actionDataSet != ActionDataSet.EMPTY
        && this.actionDataEntryId != null) {
      log.error(
          "No valid action data entry found for {} in {}!",
          this.actionDataEntryId,
          this.actionDataSet);
    }
    return ActionDataEntry.EMPTY;
  }

  private void navigateToActionDataEditor() {
    if (this.actionEventType != null && this.actionEventType != ActionEventType.NONE) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionDataEditor(this.getEasyNPCUUID(), actionEventType, configurationType);
    } else if (this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionDataEditor(
              this.getEasyNPCUUID(),
              this.editorType,
              this.getDialogUUID(),
              this.getDialogButtonUUID());
    } else {
      log.error("No valid action event type found for {}!", this.getEasyNPCUUID());
    }
  }

  private void saveActionDataEntry() {
    if (this.actionDataSet == null) {
      return;
    }

    // Remove existing and prepare new action data entry
    if (this.actionDataEntryId != null) {
      this.actionDataSet.remove(this.actionDataEntryId);
    }
    ActionDataEntry newActionDataEntry =
        actionEntryWidget != null ? actionEntryWidget.getActionDataEntry() : ActionDataEntry.EMPTY;
    this.actionDataSet.add(newActionDataEntry);

    // Save action data set
    if (this.actionEventType != null && this.actionEventType != ActionEventType.NONE) {
      NetworkMessageHandlerManager.getServerHandler()
          .actionEventChange(this.getEasyNPCUUID(), actionEventType, this.actionDataSet);
    } else if (this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON) {
      this.getDialogButtonData().setActionDataSet(this.actionDataSet);
      NetworkMessageHandlerManager.getServerHandler()
          .saveDialogButton(
              this.getEasyNPCUUID(),
              this.getDialogUUID(),
              this.getDialogButtonUUID(),
              this.getDialogButtonData());
    } else {
      log.error(
          "Unable to save Action Data Set {} for {}!", this.actionDataSet, this.getEasyNPCUUID());
    }
  }

  private void deleteActionDataEntry() {
    if (this.minecraft == null
        || this.actionDataSet == null
        || this.actionDataEntryId == null
        || this.actionDataEntryId == Constants.EMPTY_UUID) {
      return;
    }
    this.minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed) {
                this.actionDataSet.remove(this.actionDataEntryId);
                if (this.actionEventType != null && this.actionEventType != ActionEventType.NONE) {
                  NetworkMessageHandlerManager.getServerHandler()
                      .actionEventChange(
                          this.getEasyNPCUUID(), this.actionEventType, this.actionDataSet);
                } else if (this.editorType != null && this.editorType == EditorType.DIALOG_BUTTON) {
                  this.getDialogButtonData().setActionDataSet(this.actionDataSet);
                  NetworkMessageHandlerManager.getServerHandler()
                      .saveDialogButton(
                          this.getEasyNPCUUID(),
                          this.getDialogUUID(),
                          this.getDialogButtonUUID(),
                          this.getDialogButtonData());
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
            new TranslatableComponent(
                Constants.TEXT_PREFIX + "removeActionDataEntry.deleteQuestion"),
            new TranslatableComponent(
                Constants.TEXT_PREFIX + "removeActionDataEntry.deleteWarning",
                this.actionDataEntry.getType()),
            new TranslatableComponent(Constants.TEXT_PREFIX + "removeActionDataEntry.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  protected void changeActionDataType(SpinButton<?> spinButton) {
    log.info("Change action data type to {}", spinButton.get());
    this.actionDataType = (ActionDataType) spinButton.get();
    this.clearWidgets();
    init();
  }

  @Override
  public void init() {
    super.init();

    this.contentTop = this.topPos + 20;

    // Home Button
    this.homeButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + 3,
                this.topPos + 3,
                10,
                18,
                "<",
                onPress -> navigateToActionDataEditor()));

    // Level 1 Navigation Buttons
    this.navigationLevelOne =
        this.addRenderableWidget(
            new ActionsButton(
                this.homeButton.x + this.homeButton.getWidth(),
                this.topPos + 3,
                140,
                "Actions",
                onPress -> navigateToActionDataEditor()));

    // Level 2 Navigation Buttons
    int actionDataEntryPosition = this.actionDataSet.getPosition(this.actionDataEntry);
    this.navigationLevelTwo =
        this.addRenderableWidget(
            new ActionButton(
                this.navigationLevelOne.x + this.navigationLevelOne.getWidth(),
                this.topPos + 3,
                140,
                actionDataEntryPosition == -1
                    ? "Action: New"
                    : "Action: " + this.actionDataSet.getPosition(this.actionDataEntry),
                onPress -> navigateToActionDataEditor()));
    this.navigationLevelTwo.active = false;

    // Action Data Type Button
    this.actionDataTypeButton =
        this.addRenderableWidget(
            new SpinButton<>(
                this.leftPos + 120,
                this.contentTop + 5,
                150,
                16,
                Arrays.stream(ActionDataType.values())
                    .filter(type -> type != ActionDataType.NONE)
                    .sorted()
                    .collect(Collectors.toCollection(LinkedHashSet::new)),
                this.actionDataType,
                this::changeActionDataType));

    // Save Button
    this.saveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.leftPos + 25,
                this.bottomPos - 35,
                85,
                "save",
                onPress -> {
                  this.saveActionDataEntry();
                  this.navigateToActionDataEditor();
                }));

    // Delete Button
    this.deleteButton =
        this.addRenderableWidget(
            new DeleteButton(
                this.saveButton.x + this.saveButton.getWidth() + 10,
                this.bottomPos - 35,
                85,
                onPress -> this.deleteActionDataEntry()));

    // Chancel Button
    this.cancelButton =
        this.addRenderableWidget(
            new CancelButton(
                this.deleteButton.x + this.deleteButton.getWidth() + 10,
                this.bottomPos - 35,
                85,
                "cancel",
                onPress -> navigateToActionDataEditor()));

    // Handle edit options based on action entry type
    switch (this.actionDataType) {
      case CLOSE_DIALOG:
        this.actionEntryWidget =
            new CloseDialogEntry(this.actionDataEntry, this.actionDataSet, this);
        break;
      case COMMAND:
        this.actionEntryWidget =
            new CommandActionEntry(this.actionDataEntry, this.actionDataSet, this);
        break;
      case INTERACT_BLOCK:
        this.actionEntryWidget =
            new InteractBlockEntry(this.actionDataEntry, this.actionDataSet, this);
        break;
      case OPEN_NAMED_DIALOG:
        this.actionEntryWidget =
            new OpenNamedDialogEntry(this.actionDataEntry, this.actionDataSet, this);
        break;
      case OPEN_TRADING_SCREEN:
        this.actionEntryWidget =
            new OpenTradingScreenEntry(this.actionDataEntry, this.actionDataSet, this);
        break;
      default:
        this.actionEntryWidget = null;
        log.error("Unsupported action data type {}!", this.actionDataType);
    }

    // Initialize action entry widget
    int editorLeft = this.leftPos + 10;
    int editorTop = this.contentTop + 25;
    if (this.actionEntryWidget != null) {
      this.actionEntryWidget.init(editorLeft, editorTop);
    }
  }

  public <W extends GuiEventListener & Widget & NarratableEntry> W addActionEntryWidget(W widget) {
    return this.addRenderableWidget(widget);
  }

  public Font getFont() {
    return this.font;
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    this.renderBackground(poseStack);
    super.render(poseStack, x, y, partialTicks);
    int editorLeft = this.leftPos + 10;
    int editorTop = this.contentTop + 25;

    Text.drawString(
        poseStack,
        this.font,
        "Action Data Type:",
        this.leftPos + 10,
        this.topPos + 30,
        Constants.FONT_COLOR_BLACK);

    if (this.actionEntryWidget != null) {
      this.actionEntryWidget.render(poseStack, editorLeft, editorTop);
    }
  }

  @Override
  public void updateTick() {
    super.updateTick();

    if (this.saveButton != null) {
      this.saveButton.active =
          (this.actionDataType != this.actionDataEntry.getType()
                  && this.actionDataEntry.getType() != ActionDataType.NONE)
              || (this.actionEntryWidget != null && this.actionEntryWidget.hasChanged());
    }

    if (this.deleteButton != null) {
      this.deleteButton.active =
          this.actionDataSet != null
              && this.actionDataEntry != null
              && this.actionDataEntry != ActionDataEntry.EMPTY
              && this.actionDataEntryId != null
              && this.actionDataEntryId != Constants.EMPTY_UUID;
    }
  }
}
