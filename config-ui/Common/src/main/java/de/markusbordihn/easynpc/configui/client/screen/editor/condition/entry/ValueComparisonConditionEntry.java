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
import java.util.Locale;
import net.minecraft.client.gui.GuiGraphics;

public class ValueComparisonConditionEntry extends ConditionEntryWidget {

  private final ConditionType conditionType;
  private SpinButton<ConditionOperationType> operationTypeButton;
  private TextField valueTextField;
  private TextField uuidTextField;

  public ValueComparisonConditionEntry(
      ConditionDataEntry conditionDataEntry,
      ConditionDataSet conditionDataSet,
      ConditionDataEntryEditorContainerScreen<?> screen,
      ConditionType conditionType) {
    super(conditionDataEntry, conditionDataSet, screen);
    this.conditionType = conditionType;
  }

  private boolean isPercentage() {
    return this.conditionType != ConditionType.EXPERIENCE_LEVEL;
  }

  private int getDefaultValue() {
    return this.isPercentage() ? 50 : 1;
  }

  private ConditionOperationType getDefaultOperationType() {
    return switch (this.conditionType) {
      case NPC_HEALTH, ENTITY_HEALTH -> ConditionOperationType.LESS_THAN_OR_EQUALS;
      default -> ConditionOperationType.GREATER_THAN_OR_EQUALS;
    };
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasData = this.hasConditionData(this.conditionType);
    this.operationTypeButton =
        this.addComparisonOperationButton(
            editorLeft + 110, editorTop, hasData, this.getDefaultOperationType());
    this.valueTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font,
                editorLeft + 110,
                editorTop + 25,
                180,
                String.valueOf(hasData ? this.conditionDataEntry.value() : this.getDefaultValue()),
                this.isPercentage() ? 3 : 6));
    if (this.conditionType == ConditionType.ENTITY_HEALTH) {
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
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    String keyPrefix = "condition." + this.conditionType.name().toLowerCase(Locale.ROOT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        keyPrefix + ".operation",
        editorLeft,
        editorTop + 4,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        keyPrefix + ".value",
        editorLeft,
        editorTop + 29,
        Constants.FONT_COLOR_BLACK);
    if (this.uuidTextField != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          keyPrefix + ".uuid",
          editorLeft,
          editorTop + 54,
          Constants.FONT_COLOR_BLACK);
    }
  }

  @Override
  public ConditionDataEntry getConditionDataEntry() {
    int value = this.getDefaultValue();
    if (this.valueTextField != null) {
      try {
        int parsedValue = Integer.parseInt(this.valueTextField.getValue());
        value = this.isPercentage() ? Math.max(0, Math.min(100, parsedValue)) : parsedValue;
      } catch (NumberFormatException ignored) {
      }
    }
    return new ConditionDataEntry(
        this.conditionType,
        this.operationTypeButton != null
            ? this.operationTypeButton.get()
            : this.getDefaultOperationType(),
        this.uuidTextField != null ? this.uuidTextField.getValue().trim() : "",
        value);
  }
}
