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
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.EntityHealthConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.ExecutionLimitConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.ExperienceLevelConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.GamemodeConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.HasItemConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.NpcHealthConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.PlayerHealthConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.ScoreboardConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.TimeOfDayConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.WeatherConditionEntry;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionType;
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
  private final ConditionEditorContext context;
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
    this.context = ConditionEditorContext.resolve(this);
    this.conditionDataSet = this.context.loadConditionDataSet();
    this.conditionDataEntryId = this.getConditionDataEntryUUID();
    this.conditionDataEntry = loadConditionDataEntry();
    this.conditionType =
        this.conditionDataEntry.conditionType() != ConditionType.NONE
            ? this.conditionDataEntry.conditionType()
            : ConditionType.SCOREBOARD;
  }

  private ConditionDataEntry loadConditionDataEntry() {
    UUID entryId = this.getConditionDataEntryUUID();
    if (entryId != null && this.conditionDataSet.hasCondition(entryId)) {
      return this.conditionDataSet.getCondition(entryId);
    }

    return new ConditionDataEntry(ConditionType.SCOREBOARD);
  }

  private void navigateToConditionDataEditor() {
    this.context.openConditionListEditor();
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
    this.context.saveConditionDataSet(this.conditionDataSet);
  }

  private void deleteConditionDataEntry() {
    if (this.minecraft == null
        || this.conditionDataSet == null
        || this.conditionDataEntryId == null) {
      return;
    }

    this.minecraft.setScreenAndShow(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed) {
                this.conditionDataSet.remove(this.conditionDataEntryId);
                saveConditionDataEntry();
                this.navigateToConditionDataEditor();
              } else {
                this.minecraft.setScreenAndShow(this);
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

    int contextButtonX = this.homeButton.getX() + this.homeButton.getWidth();
    if (this.context.isActionContext()) {
      this.contextButton =
          this.addRenderableWidget(
              new ActionsButton(
                  contextButtonX,
                  this.topPos + 7,
                  140,
                  this.context.breadcrumbLabel(),
                  onPress -> this.context.openParentEditor()));
    } else {
      this.contextButton =
          this.addRenderableWidget(
              new DialogButton(
                  contextButtonX,
                  this.topPos + 7,
                  140,
                  this.context.breadcrumbLabel(),
                  onPress -> this.context.openParentEditor()));
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
      case NPC_HEALTH:
        this.conditionEntryWidget =
            new NpcHealthConditionEntry(this.conditionDataEntry, this.conditionDataSet, this);
        break;
      case ENTITY_HEALTH:
        this.conditionEntryWidget =
            new EntityHealthConditionEntry(this.conditionDataEntry, this.conditionDataSet, this);
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
      case TIME_OF_DAY:
        this.conditionEntryWidget =
            new TimeOfDayConditionEntry(this.conditionDataEntry, this.conditionDataSet, this);
        break;
      case WEATHER:
        this.conditionEntryWidget =
            new WeatherConditionEntry(this.conditionDataEntry, this.conditionDataSet, this);
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

  public UUID getExecutionLimitTargetUUID() {
    return this.context.executionLimitTargetUUID();
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
      Text.drawConfigString(
          guiGraphics,
          this.font,
          this.context.helpTextKey(),
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
