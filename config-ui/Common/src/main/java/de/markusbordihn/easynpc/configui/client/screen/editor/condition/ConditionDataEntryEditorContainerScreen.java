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

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.EditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.components.ActionButton;
import de.markusbordihn.easynpc.configui.client.screen.components.ActionsButton;
import de.markusbordihn.easynpc.configui.client.screen.components.CancelButton;
import de.markusbordihn.easynpc.configui.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.configui.client.screen.components.DialogButton;
import de.markusbordihn.easynpc.configui.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.AdvancementConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.ConditionEntryWidget;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.ExecutionLimitConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.ExperienceLevelConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.GamemodeConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.HasItemConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.PlayerHealthConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.ScoreboardConditionEntry;
import de.markusbordihn.easynpc.configui.data.editor.EditorType;
import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.UUID;
import java.util.stream.Collectors;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.Renderable;
import net.minecraft.client.gui.components.events.GuiEventListener;
import net.minecraft.client.gui.narration.NarratableEntry;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class ConditionDataEntryEditorContainerScreen<T extends EditorMenu> extends EditorScreen<T> {

  private final ConditionDataEntry conditionDataEntry;
  private final ConditionDataSet conditionDataSet;
  private final UUID conditionDataEntryId;
  private final boolean actionContext;
  protected Button homeButton;
  protected Button contextButton;
  protected Button conditionsButton;
  protected Button saveButton;
  protected Button cancelButton;
  protected Button deleteButton;
  protected Button conditionTypeButton;
  protected int contentTop;
  private ConditionEntryWidget conditionEntryWidget;
  private ConditionType conditionType;
  private boolean isNewEntry;

  public ConditionDataEntryEditorContainerScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.actionContext = isActionContext();
    this.conditionDataSet = loadConditionDataSet();
    this.conditionDataEntryId = this.getConditionDataEntryUUID();
    this.conditionDataEntry = loadConditionDataEntry();
    this.conditionType =
        this.conditionDataEntry.conditionType() != ConditionType.NONE
            ? this.conditionDataEntry.conditionType()
            : ConditionType.SCOREBOARD;
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

      return new ConditionDataSet();
    }

    DialogDataEntry dialogData = this.getDialogData();
    if (dialogData != null && dialogData.getConditions() != null) {
      return new ConditionDataSet(dialogData.getConditions());
    }

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

  private ConditionDataEntry loadConditionDataEntry() {
    UUID entryId = this.getConditionDataEntryUUID();
    if (entryId != null && this.conditionDataSet.hasCondition(entryId)) {
      return this.conditionDataSet.getCondition(entryId);
    }

    return new ConditionDataEntry(ConditionType.SCOREBOARD);
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

  protected void changeConditionType(SpinButton<?> spinButton) {
    this.conditionType = (ConditionType) spinButton.get();
    this.clearWidgets();
    init();
  }

  private void saveConditionDataEntry() {
    if (this.conditionDataSet == null) {
      return;
    }

    ConditionDataEntry newEntry =
        this.conditionEntryWidget != null
            ? this.conditionEntryWidget.getConditionDataEntry()
            : new ConditionDataEntry(this.conditionType);
    this.conditionDataSet.put(this.conditionDataEntryId, newEntry);

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

      AdditionalScreenData screenData = this.getAdditionalScreenData();
      EditorType formerEditorType = screenData.getEditorType();
      ActionEventType actionEventType = screenData.getActionEventType();

      if (formerEditorType == EditorType.TRADING_OFFER_ACTION) {
        NetworkMessageHandlerManager.getServerHandler()
            .changeTradingOfferAction(this.getEasyNPCUUID(), this.getPageIndex(), actionDataSet);
      } else if (formerEditorType == EditorType.DIALOG_BUTTON) {
        if (this.getDialogButtonData() != null) {
          NetworkMessageHandlerManager.getServerHandler()
              .saveDialogButton(
                  this.getEasyNPCUUID(),
                  this.getDialogUUID(),
                  this.getDialogButtonUUID(),
                  this.getDialogButtonData().withActionDataSet(actionDataSet));
        }
      } else if (actionEventType != null && actionEventType != ActionEventType.NONE) {
        NetworkMessageHandlerManager.getServerHandler()
            .actionEventChange(this.getEasyNPCUUID(), actionEventType, actionDataSet);
      }
      return;
    }

    DialogDataEntry dialogData = this.getDialogData();
    if (dialogData == null) {
      return;
    }

    dialogData.setConditions(this.conditionDataSet.getConditions());
    NetworkMessageHandlerManager.getServerHandler()
        .saveDialog(this.getEasyNPCUUID(), this.getDialogUUID(), dialogData);
  }

  private void deleteConditionDataEntry() {
    if (this.minecraft == null
        || this.conditionDataSet == null
        || this.conditionDataEntryId == null) {
      return;
    }

    this.minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed) {
                this.conditionDataSet.remove(this.conditionDataEntryId);
                saveConditionDataEntry();
                this.navigateToConditionDataEditor();
              } else {
                this.minecraft.setScreen(this);
              }
            },
            TextComponent.getTranslatedConfigText("removeConditionDataEntry.deleteQuestion"),
            TextComponent.getTranslatedConfigText(
                "removeConditionDataEntry.deleteWarning", this.conditionType.name()),
            TextComponent.getTranslatedConfigText("removeConditionDataEntry.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  @Override
  public void init() {
    super.init();

    this.contentTop = this.topPos + 20;

    this.homeButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + 7,
                this.topPos + 7,
                10,
                16,
                "<",
                onPress -> this.navigateToConditionDataEditor()));

    if (this.actionContext) {
      this.contextButton =
          this.addRenderableWidget(
              new ActionsButton(
                  this.homeButton.getX() + this.homeButton.getWidth(),
                  this.topPos + 7,
                  140,
                  "Actions",
                  onPress -> this.navigateToConditionDataEditor()));
    } else {
      this.contextButton =
          this.addRenderableWidget(
              new DialogButton(
                  this.homeButton.getX() + this.homeButton.getWidth(),
                  this.topPos + 7,
                  140,
                  this.getDialogData() != null ? this.getDialogData().getName(21) : "Dialog",
                  onPress ->
                      NetworkMessageHandlerManager.getServerHandler()
                          .openDialogEditor(this.getEasyNPCUUID(), this.getDialogUUID())));
    }

    this.conditionsButton =
        this.addRenderableWidget(
            new ActionButton(
                this.contextButton.getX() + this.contextButton.getWidth(),
                this.topPos + 7,
                140,
                "Conditions",
                onPress -> this.navigateToConditionDataEditor()));

    this.conditionTypeButton =
        this.addRenderableWidget(
            new SpinButton<>(
                this.leftPos + 133,
                this.contentTop + 5,
                160,
                16,
                Arrays.stream(ConditionType.values())
                    .filter(type -> type != ConditionType.NONE)
                    .sorted()
                    .collect(Collectors.toCollection(LinkedHashSet::new)),
                this.conditionType,
                this::changeConditionType));

    this.isNewEntry = !this.conditionDataSet.hasCondition(this.conditionDataEntryId);

    this.saveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.leftPos + 25,
                this.bottomPos - 35,
                85,
                this.isNewEntry ? "add" : "save",
                onPress -> {
                  this.saveConditionDataEntry();
                  this.navigateToConditionDataEditor();
                }));

    this.deleteButton =
        this.addRenderableWidget(
            new DeleteButton(
                this.saveButton.getX() + this.saveButton.getWidth() + 10,
                this.bottomPos - 35,
                85,
                onPress -> this.deleteConditionDataEntry()));
    this.deleteButton.active = !this.isNewEntry;

    this.cancelButton =
        this.addRenderableWidget(
            new CancelButton(
                this.deleteButton.getX() + this.deleteButton.getWidth() + 10,
                this.bottomPos - 35,
                85,
                "cancel",
                onPress -> this.navigateToConditionDataEditor()));

    switch (this.conditionType) {
      case SCOREBOARD:
        this.conditionEntryWidget =
            new ScoreboardConditionEntry(this.conditionDataEntry, this.conditionDataSet, this);
        break;
      case EXECUTION_LIMIT:
        this.conditionEntryWidget =
            new ExecutionLimitConditionEntry(this.conditionDataEntry, this.conditionDataSet, this);
        break;
      case HAS_ITEM_IN_INVENTORY:
      case HAS_ITEM_IN_HAND:
        this.conditionEntryWidget =
            new HasItemConditionEntry(
                this.conditionDataEntry, this.conditionDataSet, this, this.conditionType);
        break;
      case ADVANCEMENT:
        this.conditionEntryWidget =
            new AdvancementConditionEntry(this.conditionDataEntry, this.conditionDataSet, this);
        break;
      case EXPERIENCE_LEVEL:
        this.conditionEntryWidget =
            new ExperienceLevelConditionEntry(this.conditionDataEntry, this.conditionDataSet, this);
        break;
      case PLAYER_HEALTH:
        this.conditionEntryWidget =
            new PlayerHealthConditionEntry(this.conditionDataEntry, this.conditionDataSet, this);
        break;
      case PLAYER_TAG:
        this.conditionEntryWidget =
            new HasItemConditionEntry(
                this.conditionDataEntry, this.conditionDataSet, this, ConditionType.PLAYER_TAG);
        break;
      case TEAM:
        this.conditionEntryWidget =
            new HasItemConditionEntry(
                this.conditionDataEntry, this.conditionDataSet, this, ConditionType.TEAM);
        break;
      case GAMEMODE:
        this.conditionEntryWidget =
            new GamemodeConditionEntry(this.conditionDataEntry, this.conditionDataSet, this);
        break;
      case FALLBACK:
        this.conditionEntryWidget = null;
        break;
      default:
        this.conditionEntryWidget = null;
        log.error("Unsupported condition type {}!", this.conditionType);
    }

    if (this.conditionEntryWidget != null) {
      this.conditionEntryWidget.init(this.leftPos + 10, this.contentTop + 55);
    }
  }

  public <W extends GuiEventListener & Renderable & NarratableEntry> W addConditionEntryWidget(
      W widget) {
    return this.addRenderableWidget(widget);
  }

  public boolean isNewEntry() {
    return this.isNewEntry;
  }

  public Font getFont() {
    return this.font;
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    super.extractRenderState(guiGraphics, x, y, partialTicks);

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.type",
        this.leftPos + 10,
        this.topPos + 30,
        Constants.FONT_COLOR_BLACK);

    if (this.conditionType == ConditionType.FALLBACK) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "condition.fallback.info",
          this.leftPos + 10,
          this.topPos + 50,
          Constants.FONT_COLOR_DEFAULT);
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "condition.hint.fallback",
          this.leftPos + 10,
          this.topPos + 62,
          Constants.FONT_COLOR_DEFAULT);
    } else {
      String helpTextKey =
          this.actionContext ? "condition.help_text.action" : "condition.help_text.dialog";
      Text.drawConfigString(
          guiGraphics,
          this.font,
          helpTextKey,
          this.leftPos + 10,
          this.topPos + 50,
          Constants.FONT_COLOR_DEFAULT);
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "condition.hint." + this.conditionType.name().toLowerCase(),
          this.leftPos + 10,
          this.topPos + 62,
          Constants.FONT_COLOR_DEFAULT);
    }

    if (this.conditionEntryWidget != null) {
      this.conditionEntryWidget.render(guiGraphics, this.leftPos + 10, this.contentTop + 55);
    }
  }
}
