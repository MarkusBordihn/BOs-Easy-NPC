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

import de.markusbordihn.easynpc.client.screen.components.CancelButton;
import de.markusbordihn.easynpc.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.EditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.components.DialogButton;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.ConditionEntryWidget;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.ExecutionLimitConditionEntry;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry.ScoreboardConditionEntry;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
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
import net.minecraft.client.gui.GuiGraphics;
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
  protected Button homeButton;
  protected Button dialogButton;
  protected Button conditionsButton;
  protected Button saveButton;
  protected Button cancelButton;
  protected Button deleteButton;
  protected Button conditionTypeButton;
  protected int contentTop;
  private ConditionEntryWidget conditionEntryWidget;
  private ConditionType conditionType;

  public ConditionDataEntryEditorContainerScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.conditionDataSet = getConditionDataSet();

    // Condition Data Entry
    this.conditionDataEntryId = this.getConditionDataEntryUUID();
    this.conditionDataEntry = this.getConditionDataEntry();
    this.conditionType =
        this.conditionDataEntry.conditionType() != ConditionType.NONE
            ? this.conditionDataEntry.conditionType()
            : ConditionType.SCOREBOARD;
  }

  private ConditionDataSet getConditionDataSet() {
    DialogDataEntry dialogData = this.getDialogData();
    if (dialogData != null && dialogData.getConditions() != null) {
      return new ConditionDataSet(dialogData.getConditions());
    }
    log.error("No valid condition data set found!");
    return new ConditionDataSet();
  }

  private ConditionDataEntry getConditionDataEntry() {
    if (this.conditionDataSet != null
        && this.conditionDataEntryId != null
        && this.conditionDataSet.hasCondition(this.conditionDataEntryId)) {
      return this.conditionDataSet.getCondition(this.conditionDataEntryId);
    }
    return new ConditionDataEntry(ConditionType.SCOREBOARD);
  }

  private void navigateToConditionDataEditor() {
    NetworkMessageHandlerManager.getServerHandler()
        .openConditionDataEditor(this.getEasyNPCUUID(), this.getDialogUUID());
  }

  protected void changeConditionType(SpinButton<?> spinButton) {
    log.info("Change condition type to {}", spinButton.get());
    this.conditionType = (ConditionType) spinButton.get();
    this.clearWidgets();
    init();
  }

  private void saveConditionDataEntry() {
    if (this.conditionDataSet == null) {
      return;
    }

    // Update or create new condition data entry
    ConditionDataEntry newConditionDataEntry =
        conditionEntryWidget != null
            ? conditionEntryWidget.getConditionDataEntry()
            : new ConditionDataEntry(this.conditionType);
    this.conditionDataSet.put(this.conditionDataEntryId, newConditionDataEntry);

    // Save updated condition data set
    DialogDataEntry dialogData = this.getDialogData();
    dialogData.setConditions(this.conditionDataSet.getConditions());
    NetworkMessageHandlerManager.getServerHandler()
        .saveDialog(this.getEasyNPCUUID(), this.getDialogUUID(), dialogData);
  }

  private void deleteConditionDataEntry() {
    if (this.minecraft == null
        || this.conditionDataSet == null
        || this.conditionDataEntryId == null
        || this.conditionDataEntryId == Constants.EMPTY_UUID) {
      return;
    }
    this.minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed) {
                this.conditionDataSet.remove(this.conditionDataEntryId);
                DialogDataEntry dialogData = this.getDialogData();
                dialogData.setConditions(this.conditionDataSet.getConditions());
                NetworkMessageHandlerManager.getServerHandler()
                    .saveDialog(this.getEasyNPCUUID(), this.getDialogUUID(), dialogData);
                this.navigateToConditionDataEditor();
              } else {
                this.minecraft.setScreen(this);
              }
            },
            TextComponent.getTranslatedConfigText("removeConditionDataEntry.deleteQuestion"),
            TextComponent.getTranslatedConfigText(
                "removeConditionDataEntry.deleteWarning", "SCOREBOARD"),
            TextComponent.getTranslatedConfigText("removeConditionDataEntry.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  @Override
  public void init() {
    super.init();

    this.contentTop = this.topPos + 20;

    // Home Button
    this.homeButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + 7,
                this.topPos + 7,
                10,
                16,
                "<",
                onPress -> this.navigateToConditionDataEditor()));

    // Dialog Button
    this.dialogButton =
        this.addRenderableWidget(
            new DialogButton(
                this.homeButton.getX() + this.homeButton.getWidth(),
                this.topPos + 7,
                140,
                this.getDialogData().getName(21),
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openDialogEditor(this.getEasyNPCUUID(), this.getDialogUUID())));

    // Conditions Button
    this.conditionsButton =
        this.addRenderableWidget(
            new DialogButton(
                this.dialogButton.getX() + this.dialogButton.getWidth(),
                this.topPos + 7,
                140,
                "Conditions",
                onPress -> this.navigateToConditionDataEditor()));

    // Condition Type SpinButton
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

    // Save Button
    this.saveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.leftPos + 25,
                this.bottomPos - 35,
                85,
                "save",
                onPress -> {
                  this.saveConditionDataEntry();
                  this.navigateToConditionDataEditor();
                }));

    // Delete Button
    this.deleteButton =
        this.addRenderableWidget(
            new DeleteButton(
                this.saveButton.getX() + this.saveButton.getWidth() + 10,
                this.bottomPos - 35,
                85,
                onPress -> this.deleteConditionDataEntry()));

    // Cancel Button
    this.cancelButton =
        this.addRenderableWidget(
            new CancelButton(
                this.deleteButton.getX() + this.deleteButton.getWidth() + 10,
                this.bottomPos - 35,
                85,
                "cancel",
                onPress -> this.navigateToConditionDataEditor()));

    // Handle edit options based on condition type
    switch (this.conditionType) {
      case SCOREBOARD:
        this.conditionEntryWidget =
            new ScoreboardConditionEntry(this.conditionDataEntry, this.conditionDataSet, this);
        break;
      case EXECUTION_LIMIT:
        this.conditionEntryWidget =
            new ExecutionLimitConditionEntry(this.conditionDataEntry, this.conditionDataSet, this);
        break;
      default:
        this.conditionEntryWidget = null;
        log.error("Unsupported condition type {}!", this.conditionType);
    }

    // Initialize condition entry widget
    if (this.conditionEntryWidget != null) {
      this.conditionEntryWidget.init(this.leftPos + 10, this.contentTop + 50);
    }
  }

  public <W extends GuiEventListener & Renderable & NarratableEntry> W addConditionEntryWidget(
      W widget) {
    return this.addRenderableWidget(widget);
  }

  public Font getFont() {
    return this.font;
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Condition type label
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.type",
        this.leftPos + 10,
        this.topPos + 30,
        Constants.FONT_COLOR_BLACK);

    // Help text
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.help_text",
        this.leftPos + 10,
        this.topPos + 50,
        Constants.FONT_COLOR_GRAY);

    if (this.conditionEntryWidget != null) {
      this.conditionEntryWidget.render(guiGraphics, this.leftPos + 10, this.contentTop + 50);
    }
  }
}
