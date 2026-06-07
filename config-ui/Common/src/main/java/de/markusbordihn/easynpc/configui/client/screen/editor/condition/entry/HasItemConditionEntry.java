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
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.ConditionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.HandItemType;
import java.util.LinkedHashSet;
import net.minecraft.client.gui.GuiGraphicsExtractor;

public class HasItemConditionEntry extends ConditionEntryWidget {

  private final ConditionType conditionType;
  private TextField itemNameTextField;
  private SpinButton<ConditionOperationType> operationTypeButton;
  private Checkbox mainHandCheckbox;
  private Checkbox offHandCheckbox;

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

    if (this.conditionType == ConditionType.HAS_ITEM_IN_HAND) {
      ConditionOperationType operationType =
          hasData ? this.conditionDataEntry.operationType() : ConditionOperationType.EQUALS;
      boolean mainHandSelected = true;
      boolean offHandSelected = true;
      if (hasData && this.conditionDataEntry.subType() instanceof HandItemType handItemType) {
        mainHandSelected =
            handItemType == HandItemType.MAIN_HAND || handItemType == HandItemType.BOTH;
        offHandSelected =
            handItemType == HandItemType.OFF_HAND || handItemType == HandItemType.BOTH;
      }

      LinkedHashSet<ConditionOperationType> operationTypes = new LinkedHashSet<>();
      operationTypes.add(ConditionOperationType.EQUALS);
      operationTypes.add(ConditionOperationType.NOT_EQUALS);

      this.operationTypeButton =
          this.screen.addConditionEntryWidget(
              new SpinButton<>(
                  editorLeft,
                  editorTop + 20,
                  125,
                  16,
                  operationTypes,
                  operationType,
                  button -> {}));

      this.itemNameTextField =
          this.screen.addConditionEntryWidget(
              new TextField(
                  this.font,
                  editorLeft + 130,
                  editorTop + 20,
                  155,
                  hasData ? this.conditionDataEntry.name() : "minecraft:diamond",
                  128));

      this.mainHandCheckbox =
          this.screen.addConditionEntryWidget(
              new Checkbox(
                  editorLeft,
                  editorTop + 40,
                  "config.condition.has_item_in_hand.main_hand",
                  mainHandSelected));

      this.offHandCheckbox =
          this.screen.addConditionEntryWidget(
              new Checkbox(
                  editorLeft + 155,
                  editorTop + 40,
                  "config.condition.has_item_in_hand.off_hand",
                  offHandSelected));
    } else {
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
  }

  private String getExampleValue() {
    return switch (this.conditionType) {
      case PLAYER_TAG -> "vip_player";
      case TEAM -> "red_team";
      default -> "minecraft:diamond";
    };
  }

  @Override
  public void render(GuiGraphicsExtractor guiGraphics, int editorLeft, int editorTop) {
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
    if (this.conditionType == ConditionType.HAS_ITEM_IN_HAND) {
      boolean mainSelected = this.mainHandCheckbox != null && this.mainHandCheckbox.selected();
      boolean offSelected = this.offHandCheckbox != null && this.offHandCheckbox.selected();
      HandItemType handItemType;
      if (mainSelected && offSelected) {
        handItemType = HandItemType.BOTH;
      } else if (mainSelected) {
        handItemType = HandItemType.MAIN_HAND;
      } else if (offSelected) {
        handItemType = HandItemType.OFF_HAND;
      } else {
        handItemType = HandItemType.BOTH;
      }
      ConditionOperationType operationType =
          this.operationTypeButton != null
              ? this.operationTypeButton.get()
              : ConditionOperationType.EQUALS;

      return new ConditionDataEntry(
          this.conditionType,
          handItemType,
          operationType,
          this.itemNameTextField != null ? this.itemNameTextField.getValue().trim() : "",
          0);
    }

    return new ConditionDataEntry(
        this.conditionType,
        ConditionOperationType.NONE,
        this.itemNameTextField != null ? this.itemNameTextField.getValue().trim() : "",
        0);
  }
}
