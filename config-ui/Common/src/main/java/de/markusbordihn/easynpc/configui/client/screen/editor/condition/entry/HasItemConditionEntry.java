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

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.ConditionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import net.minecraft.client.gui.GuiGraphics;

public class HasItemConditionEntry extends ConditionEntryWidget {

  private final ConditionType conditionType;
  private TextField itemNameTextField;

  public HasItemConditionEntry(
      ConditionDataEntry conditionDataEntry,
      ConditionDataSet conditionDataSet,
      ConditionDataEntryEditorContainerScreen<?> screen,
      ConditionType conditionType) {
    super(conditionDataEntry, conditionDataSet, screen);
    this.conditionType = conditionType;
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasData = hasConditionData(this.conditionType);
    this.itemNameTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font,
                editorLeft + 110,
                editorTop,
                180,
                hasData ? this.conditionDataEntry.name() : getExampleValue(),
                128));
  }

  private String getExampleValue() {
    return switch (this.conditionType) {
      case PLAYER_TAG -> "vip_player";
      case TEAM -> "red_team";
      default -> "minecraft:diamond";
    };
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    String labelKey =
        switch (this.conditionType) {
          case PLAYER_TAG -> "condition.player_tag.name";
          case TEAM -> "condition.team.name";
          default -> "condition.has_item.name";
        };
    Text.drawConfigString(
        guiGraphics, this.font, labelKey, editorLeft, editorTop + 4, Constants.FONT_COLOR_BLACK);
  }

  @Override
  public ConditionDataEntry getConditionDataEntry() {
    return new ConditionDataEntry(
        this.conditionType,
        ConditionOperationType.NONE,
        this.itemNameTextField != null ? this.itemNameTextField.getValue().trim() : "",
        0);
  }
}
