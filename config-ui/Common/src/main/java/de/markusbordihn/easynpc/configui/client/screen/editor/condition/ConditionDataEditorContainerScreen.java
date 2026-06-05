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

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.EditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.components.ActionButton;
import de.markusbordihn.easynpc.configui.client.screen.components.ActionsButton;
import de.markusbordihn.easynpc.configui.client.screen.components.AddButton;
import de.markusbordihn.easynpc.configui.client.screen.components.DialogButton;
import de.markusbordihn.easynpc.configui.data.editor.EditorType;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.UUID;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class ConditionDataEditorContainerScreen<T extends EditorMenu> extends EditorScreen<T> {

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
  private final boolean actionContext;
  protected Button homeButton;
  protected Button contextButton;
  protected Button conditionsButton;
  protected Button newConditionDataEntryButton;
  ConditionDataList conditionDataList;

  public ConditionDataEditorContainerScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.actionContext = isActionContext();
    this.conditionDataSet = loadConditionDataSet();
  }

  private boolean isActionContext() {
    UUID actionDataEntryId = this.getActionDataEntryUUID();
    return actionDataEntryId != null && !Constants.EMPTY_UUID.equals(actionDataEntryId);
  }

  private ConditionDataSet loadConditionDataSet() {
    if (this.actionContext) {
      ActionDataEntry entry = findActionDataEntry();
      if (entry != null) {
        return entry.conditionDataSet();
      }

      log.error("No valid action data entry found for condition editor!");
      return new ConditionDataSet();
    }

    DialogDataEntry dialogData = this.getDialogData();
    if (dialogData != null && dialogData.getConditions() != null) {
      ConditionDataSet set = new ConditionDataSet();
      for (ConditionDataEntry entry : dialogData.getConditions()) {
        set.add(entry);
      }
      return set;
    }

    log.error("No valid condition data set found!");
    return new ConditionDataSet();
  }

  private ActionDataEntry findActionDataEntry() {
    UUID actionDataEntryId = this.getActionDataEntryUUID();
    if (actionDataEntryId == null) {
      return null;
    }

    ActionDataSet actionDataSet = getActionDataSet();
    if (actionDataSet == null) {
      return null;
    }

    return actionDataSet.getEntryOrDefault(actionDataEntryId);
  }

  private ActionDataSet getActionDataSet() {
    EditorType formerEditorType = this.getAdditionalScreenData().getEditorType();
    ActionEventType actionEventType = this.getAdditionalScreenData().getActionEventType();

    if (formerEditorType == EditorType.TRADING_OFFER_ACTION) {
      return this.getAdditionalScreenData().getTradingOfferActionDataSet();
    } else if (formerEditorType == EditorType.DIALOG_BUTTON) {
      return this.getDialogButtonData() != null ? this.getDialogButtonData().actionDataSet() : null;
    } else if (actionEventType != null && actionEventType != ActionEventType.NONE) {
      return this.getAdditionalScreenData().getActionEventSet().getActionEvents(actionEventType);
    }

    return null;
  }

  private void updateConditionDataSet() {
    if (this.actionContext) {
      ActionDataEntry currentEntry = findActionDataEntry();
      if (currentEntry == null) {
        return;
      }

      ActionDataEntry updatedEntry = currentEntry.withConditionDataSet(this.conditionDataSet);
      ActionDataSet actionDataSet = getActionDataSet();
      if (actionDataSet != null) {
        actionDataSet.put(this.getActionDataEntryUUID(), updatedEntry);
      }

      EditorType formerEditorType = this.getAdditionalScreenData().getEditorType();
      ActionEventType actionEventType = this.getAdditionalScreenData().getActionEventType();
      ConfigurationType configurationType = this.getAdditionalScreenData().getConfigurationType();

      if (formerEditorType == EditorType.TRADING_OFFER_ACTION) {
        NetworkMessageHandlerManager.getServerHandler()
            .changeTradingOfferAction(this.getEasyNPCUUID(), this.getPageIndex(), actionDataSet);
      } else if (formerEditorType == EditorType.DIALOG_BUTTON) {
        NetworkMessageHandlerManager.getServerHandler()
            .saveDialogButton(
                this.getEasyNPCUUID(),
                this.getDialogUUID(),
                this.getDialogButtonUUID(),
                this.getDialogButtonData().withActionDataSet(actionDataSet));
      } else if (actionEventType != null && actionEventType != ActionEventType.NONE) {
        NetworkMessageHandlerManager.getServerHandler()
            .actionEventChange(this.getEasyNPCUUID(), actionEventType, actionDataSet);
      }
      return;
    }

    DialogDataEntry dialogData = this.getDialogData();
    dialogData.setConditions(this.conditionDataSet.getConditions());
    NetworkMessageHandlerManager.getServerHandler()
        .saveDialog(this.getEasyNPCUUID(), this.getDialogUUID(), dialogData);
  }

  private void handleBackNavigation() {
    if (this.actionContext) {
      EditorType formerEditorType = this.getAdditionalScreenData().getEditorType();
      ActionEventType actionEventType = this.getAdditionalScreenData().getActionEventType();
      ConfigurationType configurationType = this.getAdditionalScreenData().getConfigurationType();

      if (formerEditorType == EditorType.TRADING_OFFER_ACTION) {
        NetworkMessageHandlerManager.getServerHandler()
            .openTradingOfferActionEntryEditor(
                this.getEasyNPCUUID(),
                this.getPageIndex(),
                configurationType,
                findActionDataEntry() != null ? findActionDataEntry() : new ActionDataEntry());
      } else if (formerEditorType == EditorType.DIALOG_BUTTON) {
        NetworkMessageHandlerManager.getServerHandler()
            .openActionDataEntryEditor(
                this.getEasyNPCUUID(),
                formerEditorType,
                this.getDialogUUID(),
                this.getDialogButtonUUID(),
                findActionDataEntry() != null ? findActionDataEntry() : new ActionDataEntry());
      } else if (actionEventType != null && actionEventType != ActionEventType.NONE) {
        NetworkMessageHandlerManager.getServerHandler()
            .openActionDataEntryEditor(
                this.getEasyNPCUUID(),
                actionEventType,
                configurationType,
                findActionDataEntry() != null ? findActionDataEntry() : new ActionDataEntry());
      }
      return;
    }

    NetworkMessageHandlerManager.getServerHandler()
        .openDialogEditor(this.getEasyNPCUUID(), this.getDialogUUID());
  }

  private void handleNewConditionDataEntry() {
    if (this.actionContext) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionConditionDataEntryEditor(
              this.getEasyNPCUUID(),
              this.getActionDataEntryUUID(),
              this.getAdditionalScreenData().getActionEventType(),
              this.getAdditionalScreenData().getConfigurationType(),
              this.getAdditionalScreenData().getEditorType(),
              this.getDialogUUID(),
              this.getDialogButtonUUID(),
              this.getPageIndex(),
              new ConditionDataEntry(ConditionType.SCOREBOARD));
      return;
    }

    NetworkMessageHandlerManager.getServerHandler()
        .openConditionDataEntryEditor(
            this.getEasyNPCUUID(),
            this.getDialogUUID(),
            new ConditionDataEntry(ConditionType.SCOREBOARD));
  }

  private void handleEditConditionDataEntry(ConditionDataEntry conditionDataEntry) {
    if (this.actionContext) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionConditionDataEntryEditor(
              this.getEasyNPCUUID(),
              this.getActionDataEntryUUID(),
              this.getAdditionalScreenData().getActionEventType(),
              this.getAdditionalScreenData().getConfigurationType(),
              this.getAdditionalScreenData().getEditorType(),
              this.getDialogUUID(),
              this.getDialogButtonUUID(),
              this.getPageIndex(),
              conditionDataEntry);
      return;
    }

    NetworkMessageHandlerManager.getServerHandler()
        .openConditionDataEntryEditor(
            this.getEasyNPCUUID(), this.getDialogUUID(), conditionDataEntry);
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

  private void navigateToConditionDataEditor() {
    if (this.actionContext) {
      NetworkMessageHandlerManager.getServerHandler()
          .openActionConditionDataEditor(
              this.getEasyNPCUUID(),
              this.getActionDataEntryUUID(),
              this.getAdditionalScreenData().getActionEventType(),
              this.getAdditionalScreenData().getConfigurationType(),
              this.getAdditionalScreenData().getEditorType(),
              this.getDialogUUID(),
              this.getDialogButtonUUID(),
              this.getPageIndex());
      return;
    }

    NetworkMessageHandlerManager.getServerHandler()
        .openConditionDataEditor(this.getEasyNPCUUID(), this.getDialogUUID());
  }

  @Override
  public void init() {
    super.init();

    this.homeButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + HOME_BUTTON_X_OFFSET,
                this.topPos + HOME_BUTTON_Y_OFFSET,
                HOME_BUTTON_WIDTH,
                HOME_BUTTON_HEIGHT,
                "<",
                onPress -> handleBackNavigation()));

    if (this.actionContext) {
      this.contextButton =
          this.addRenderableWidget(
              new ActionsButton(
                  this.homeButton.getX() + this.homeButton.getWidth(),
                  this.topPos + HOME_BUTTON_Y_OFFSET,
                  NAVIGATION_BUTTON_WIDTH,
                  "Actions",
                  onPress -> handleBackNavigation()));
    } else {
      this.contextButton =
          this.addRenderableWidget(
              new DialogButton(
                  this.homeButton.getX() + this.homeButton.getWidth(),
                  this.topPos + HOME_BUTTON_Y_OFFSET,
                  NAVIGATION_BUTTON_WIDTH,
                  this.getDialogData() != null ? this.getDialogData().getName(21) : "Dialog",
                  onPress ->
                      NetworkMessageHandlerManager.getServerHandler()
                          .openDialogEditor(this.getEasyNPCUUID(), this.getDialogUUID())));
    }

    this.conditionsButton =
        this.addRenderableWidget(
            new ActionButton(
                this.contextButton.getX() + this.contextButton.getWidth(),
                this.topPos + HOME_BUTTON_Y_OFFSET,
                NAVIGATION_BUTTON_WIDTH,
                "Conditions",
                onPress -> {}));
    this.conditionsButton.active = false;

    this.newConditionDataEntryButton =
        this.addRenderableWidget(
            new AddButton(
                this.leftPos + ADD_BUTTON_X_OFFSET,
                this.topPos + FOOTER_Y_OFFSET + 5,
                ADD_BUTTON_WIDTH,
                "condition.add",
                onPress -> handleNewConditionDataEntry()));

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

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    super.extractRenderState(guiGraphics, x, y, partialTicks);

    guiGraphics.fill(
        this.leftPos + LIST_X_OFFSET,
        this.topPos + HEADER_Y_OFFSET + 5,
        this.leftPos + LIST_TOTAL_WIDTH,
        this.topPos + LIST_Y_END,
        COLOR_LIST_BACKGROUND);

    if (this.conditionDataList != null) {
      this.conditionDataList.extractRenderState(guiGraphics, x, y, partialTicks);
    }

    renderHeader(guiGraphics);
    renderFooter(guiGraphics);

    if (this.newConditionDataEntryButton != null) {
      this.newConditionDataEntryButton.extractRenderState(guiGraphics, x, y, partialTicks);
    }

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.info.and_logic",
        this.leftPos + LIST_X_OFFSET + 5,
        this.topPos + LIST_Y_END + 5,
        Constants.FONT_COLOR_DARK_RED);
  }

  private void renderHeader(GuiGraphicsExtractor guiGraphics) {
    guiGraphics.fill(
        this.leftPos + LIST_X_OFFSET,
        this.topPos + HEADER_Y_OFFSET,
        this.leftPos + LIST_TOTAL_WIDTH,
        this.topPos + HEADER_Y_OFFSET + HEADER_HEIGHT,
        COLOR_HEADER_BACKGROUND);

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

  private void renderFooter(GuiGraphicsExtractor guiGraphics) {
    guiGraphics.fill(
        this.leftPos + LIST_X_OFFSET,
        this.topPos + LIST_Y_END,
        this.leftPos + LIST_TOTAL_WIDTH,
        this.topPos + LIST_Y_END + FOOTER_HEIGHT,
        COLOR_FOOTER_BACKGROUND);
  }
}
