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

package de.markusbordihn.easynpc.configui.client.screen.preset;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.ObjectSelectionList;

public class PresetList extends ObjectSelectionList<PresetListEntry> {

  private static final int ROW_MARGIN = 3;
  private static final int SCROLLBAR_WIDTH = 6;

  private int listLeft;

  public PresetList(Minecraft minecraft, int width, int height, int y0, int y1, int itemHeight) {
    super(minecraft, width, height, y0, y1, itemHeight);
    this.setRenderHeader(false, 0);
    this.setRenderBackground(false);
    this.setRenderTopAndBottom(false);
  }

  @Override
  public int addEntry(PresetListEntry presetListEntry) {
    return super.addEntry(presetListEntry);
  }

  @Override
  public void setLeftPos(int left) {
    this.listLeft = left;
    super.setLeftPos(left);
  }

  @Override
  public int getRowLeft() {
    return this.listLeft + ROW_MARGIN;
  }

  @Override
  public int getRowWidth() {
    return this.width - SCROLLBAR_WIDTH - ROW_MARGIN * 2;
  }

  @Override
  protected int getScrollbarPosition() {
    return this.listLeft + this.width - SCROLLBAR_WIDTH;
  }
}
