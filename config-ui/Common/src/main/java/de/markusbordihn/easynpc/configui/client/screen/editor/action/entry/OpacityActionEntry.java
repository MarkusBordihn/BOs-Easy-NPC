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

package de.markusbordihn.easynpc.configui.client.screen.editor.action.entry;

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.utils.ValueUtils;
import net.minecraft.client.gui.GuiGraphics;

public class OpacityActionEntry extends ActionEntryWidget {

  private TextField opacityTextField;

  public OpacityActionEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    this.opacityTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft, editorTop + 20, 80, 16));
    this.opacityTextField.setMaxLength(3);
    this.opacityTextField.setFilter(ValueUtils::isNumericValue);
    this.opacityTextField.setValue(
        this.hasActionData(ActionDataType.SET_OPACITY)
            ? this.actionDataEntry.command()
            : String.valueOf(DisplayAttributeType.DEFAULT_OPACITY));
  }

  private String getOpacity() {
    return this.opacityTextField != null ? this.opacityTextField.getValue().trim() : "";
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.opacity",
        editorLeft + 2,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.opacity.hint",
        editorLeft + 2,
        editorTop + 42,
        Constants.FONT_COLOR_GRAY);
  }

  @Override
  public boolean isValid() {
    return ValueUtils.isNumericValue(
        this.getOpacity(), DisplayAttributeType.MIN_OPACITY, DisplayAttributeType.MAX_OPACITY);
  }

  @Override
  public boolean hasChanged() {
    return !this.hasActionData(ActionDataType.SET_OPACITY)
        || !this.getOpacity().equals(this.actionDataEntry.command());
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    return new ActionDataEntry(ActionDataType.SET_OPACITY, this.getOpacity());
  }
}
