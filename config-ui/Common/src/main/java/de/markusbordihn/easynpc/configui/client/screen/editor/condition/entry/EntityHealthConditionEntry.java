/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry;

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.ConditionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import net.minecraft.client.gui.GuiGraphics;

public class EntityHealthConditionEntry extends ConditionEntryWidget {

  private SpinButton<ConditionOperationType> operationTypeButton;
  private TextField valueTextField;
  private TextField uuidTextField;

  public EntityHealthConditionEntry(
      ConditionDataEntry conditionDataEntry,
      ConditionDataSet conditionDataSet,
      ConditionDataEntryEditorContainerScreen<?> screen) {
    super(conditionDataEntry, conditionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasData = hasConditionData(ConditionType.ENTITY_HEALTH);
    this.operationTypeButton =
        this.addComparisonOperationButton(
            editorLeft + 110, editorTop, hasData, ConditionOperationType.LESS_THAN_OR_EQUALS);
    this.valueTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font,
                editorLeft + 110,
                editorTop + 25,
                180,
                hasData ? String.valueOf(this.conditionDataEntry.value()) : "50",
                3));
    this.uuidTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font,
                editorLeft + 110,
                editorTop + 50,
                180,
                hasData ? this.conditionDataEntry.name() : "",
                36));
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.entity_health.operation",
        editorLeft,
        editorTop + 4,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.entity_health.value",
        editorLeft,
        editorTop + 29,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.entity_health.uuid",
        editorLeft,
        editorTop + 54,
        Constants.FONT_COLOR_BLACK);
  }

  @Override
  public ConditionDataEntry getConditionDataEntry() {
    int value = 50;
    if (this.valueTextField != null) {
      try {
        value = Math.max(0, Math.min(100, Integer.parseInt(this.valueTextField.getValue())));
      } catch (NumberFormatException ignored) {
      }
    }
    return new ConditionDataEntry(
        ConditionType.ENTITY_HEALTH,
        this.operationTypeButton != null
            ? this.operationTypeButton.get()
            : ConditionOperationType.LESS_THAN_OR_EQUALS,
        this.uuidTextField != null ? this.uuidTextField.getValue().trim() : "",
        value);
  }
}
