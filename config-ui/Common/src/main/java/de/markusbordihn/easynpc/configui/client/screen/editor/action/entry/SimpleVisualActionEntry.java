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
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import net.minecraft.client.gui.GuiGraphics;

public class SimpleVisualActionEntry extends ActionEntryWidget {

  private final ActionDataType actionDataType;

  public SimpleVisualActionEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen,
      ActionDataType actionDataType) {
    super(actionDataEntry, actionDataSet, screen);
    this.actionDataType = actionDataType;
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    String labelKey =
        switch (this.actionDataType) {
          case CLOSE_DIALOG -> "action.close_dialog";
          case OPEN_DEFAULT_DIALOG -> "action.open_default_dialog";
          case OPEN_TRADING_SCREEN -> "action.open_trading_screen";
          case OPEN_INVENTORY_SCREEN -> "action.open_inventory_screen";
          default -> this.actionDataType.getId();
        };
    Text.drawConfigString(
        guiGraphics,
        this.font,
        labelKey,
        editorLeft + 2,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    return new ActionDataEntry(this.actionDataType);
  }

  @Override
  public boolean hasChanged() {
    return !this.hasActionData(this.actionDataType);
  }
}
