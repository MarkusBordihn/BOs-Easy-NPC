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

import de.markusbordihn.easynpc.configui.client.screen.editor.condition.ConditionDataListEntry.OnEdit;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.ConditionDataListEntry.OnRemove;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.ObjectSelectionList;

class ConditionDataList extends ObjectSelectionList<ConditionDataListEntry> {

  public ConditionDataList(
      ConditionDataSet conditionDataSet,
      Minecraft minecraft,
      int width,
      int height,
      int left,
      int top,
      int bottom,
      int entryHeight,
      OnEdit onEdit,
      OnRemove onRemove) {
    super(minecraft, width, height, top, entryHeight);
    this.setRenderHeader(false, 0);

    // Add entries
    int topPos = top + 4;
    if (conditionDataSet != null) {
      for (ConditionDataEntry conditionDataEntry : conditionDataSet.getConditions()) {
        this.addEntry(
            new ConditionDataListEntry(
                minecraft, conditionDataEntry, left, topPos, onEdit, onRemove));
        topPos += entryHeight;
      }
    }
  }

  @Override
  protected void renderSelection(
      GuiGraphics guiGraphics, int unused1, int unused2, int unused3, int unused4, int unused5) {
    // Nothing to render
  }

  @Override
  protected void renderListSeparators(GuiGraphics guiGraphics) {
    // Do not render list separators.
  }

  @Override
  protected void renderListBackground(GuiGraphics guiGraphics) {
    // Do not render list background.
  }
}
