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
import de.markusbordihn.easynpc.data.action.WaitDuration;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.gui.GuiGraphics;

public class WaitActionEntry extends ActionEntryWidget {

  private static final int HINT_TOP = 42;
  private static final int HINT_WIDTH = 285;
  private static final int HINT_SPACING = 3;

  private TextField waitDurationTextField;

  public WaitActionEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    this.waitDurationTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft, editorTop + 20, 80, 16));
    this.waitDurationTextField.setMaxLength(16);
    this.waitDurationTextField.setValue(
        this.hasActionData(ActionDataType.WAIT) ? this.actionDataEntry.command() : "");
  }

  private String getWaitDuration() {
    return this.waitDurationTextField != null ? this.waitDurationTextField.getValue().trim() : "";
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.wait",
        editorLeft + 2,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigStringWrapped(
        guiGraphics,
        this.font,
        "action.wait.hint",
        editorLeft + 2,
        editorTop + HINT_TOP,
        HINT_WIDTH,
        Constants.FONT_COLOR_GRAY);
    int hintHeight =
        this.font
                .split(TextComponent.getTranslatedConfigText("action.wait.hint"), HINT_WIDTH)
                .size()
            * this.font.lineHeight;
    Text.drawConfigStringWrapped(
        guiGraphics,
        this.font,
        "action.wait.chain.hint",
        editorLeft + 2,
        editorTop + HINT_TOP + hintHeight + HINT_SPACING,
        HINT_WIDTH,
        Constants.FONT_COLOR_GRAY);
  }

  @Override
  public boolean isValid() {
    return WaitDuration.parse(this.getWaitDuration()).isValid();
  }

  @Override
  public boolean hasChanged() {
    return !this.hasActionData(ActionDataType.WAIT)
        || !this.getWaitDuration().equals(this.actionDataEntry.command());
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    return new ActionDataEntry(ActionDataType.WAIT, this.getWaitDuration());
  }
}
