/*
 * Copyright 2026 Markus Bordihn
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

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.ConditionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import net.minecraft.client.gui.GuiGraphicsExtractor;

public class ChanceConditionEntry extends ConditionEntryWidget {

  private static final int DEFAULT_PERCENTAGE = 50;
  private TextField valueTextField;

  public ChanceConditionEntry(
      ConditionDataEntry conditionDataEntry,
      ConditionDataSet conditionDataSet,
      ConditionDataEntryEditorContainerScreen<?> screen) {
    super(conditionDataEntry, conditionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasData = hasConditionData(ConditionType.CHANCE);
    this.valueTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font,
                editorLeft + 110,
                editorTop,
                180,
                hasData
                    ? String.valueOf(this.conditionDataEntry.value())
                    : String.valueOf(DEFAULT_PERCENTAGE),
                3));
  }

  @Override
  public void render(GuiGraphicsExtractor guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.chance.value",
        editorLeft,
        editorTop + 4,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.chance.hint",
        editorLeft,
        editorTop + 29,
        Constants.FONT_COLOR_GRAY);
  }

  @Override
  public ConditionDataEntry getConditionDataEntry() {
    int value = DEFAULT_PERCENTAGE;
    if (this.valueTextField != null) {
      try {
        value = Integer.parseInt(this.valueTextField.getValue());
      } catch (NumberFormatException ignored) {
        // Keep the default percentage.
      }
    }

    return new ConditionDataEntry(
        ConditionType.CHANCE,
        ConditionOperationType.NONE,
        "",
        Math.max(
            ConditionDataEntry.MIN_CHANCE_PERCENTAGE,
            Math.min(ConditionDataEntry.MAX_CHANCE_PERCENTAGE, value)));
  }
}
