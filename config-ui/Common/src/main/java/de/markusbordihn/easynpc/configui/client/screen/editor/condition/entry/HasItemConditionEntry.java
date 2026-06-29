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
import net.minecraft.client.gui.GuiGraphics;

public class HasItemConditionEntry extends ConditionEntryWidget {

  private final ConditionType conditionType;
  private TextField itemNameTextField;
  private TextField customDataTextField;
  private TextField quantityTextField;
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

    if (!isItemCondition()) {
      this.itemNameTextField =
          this.screen.addConditionEntryWidget(
              new TextField(
                  this.font,
                  editorLeft + 110,
                  editorTop,
                  180,
                  hasData ? this.conditionDataEntry.name() : getExampleValue(),
                  128));
      return;
    }

    ConditionOperationType operationType =
        hasData ? this.conditionDataEntry.operationType() : ConditionOperationType.EQUALS;
    if (operationType == null || operationType == ConditionOperationType.NONE) {
      operationType = ConditionOperationType.EQUALS;
    }

    LinkedHashSet<ConditionOperationType> operationTypes = new LinkedHashSet<>();
    operationTypes.add(ConditionOperationType.EQUALS);
    operationTypes.add(ConditionOperationType.NOT_EQUALS);

    this.operationTypeButton =
        this.screen.addConditionEntryWidget(
            new SpinButton<>(
                editorLeft, editorTop + 20, 125, 16, operationTypes, operationType, button -> {}));

    this.itemNameTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font,
                editorLeft + 130,
                editorTop + 20,
                155,
                hasData ? this.conditionDataEntry.name() : "minecraft:diamond",
                128));

    this.customDataTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font,
                editorLeft + 130,
                editorTop + 40,
                155,
                hasData ? getStoredCustomData() : "",
                1024));

    this.quantityTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font, editorLeft + 130, editorTop + 60, 60, getQuantityValue(hasData), 10));

    if (this.conditionType == ConditionType.HAS_ITEM_IN_HAND) {
      boolean mainHandSelected = true;
      boolean offHandSelected = true;
      if (hasData && this.conditionDataEntry.subType() instanceof HandItemType handItemType) {
        mainHandSelected =
            handItemType == HandItemType.MAIN_HAND || handItemType == HandItemType.BOTH;
        offHandSelected =
            handItemType == HandItemType.OFF_HAND || handItemType == HandItemType.BOTH;
      }

      this.mainHandCheckbox =
          this.screen.addConditionEntryWidget(
              new Checkbox(
                  editorLeft,
                  editorTop + 84,
                  "config.condition.has_item_in_hand.main_hand",
                  mainHandSelected));
      this.offHandCheckbox =
          this.screen.addConditionEntryWidget(
              new Checkbox(
                  editorLeft + 155,
                  editorTop + 84,
                  "config.condition.has_item_in_hand.off_hand",
                  offHandSelected));
    }
  }

  private boolean isItemCondition() {
    return this.conditionType == ConditionType.HAS_ITEM_IN_HAND
        || this.conditionType == ConditionType.HAS_ITEM_IN_INVENTORY;
  }

  private String getQuantityValue(boolean hasData) {
    int value = hasData ? this.conditionDataEntry.value() : 0;
    return String.valueOf(Math.max(1, value));
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

    if (isItemCondition()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "condition.has_item.quantity",
          editorLeft,
          editorTop + 64,
          Constants.FONT_COLOR_BLACK);
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "condition.has_item.custom_data",
          editorLeft,
          editorTop + 44,
          Constants.FONT_COLOR_BLACK);
      int hintTop = this.conditionType == ConditionType.HAS_ITEM_IN_HAND ? 104 : 84;
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "condition.has_item.custom_data_hint",
          editorLeft,
          editorTop + hintTop,
          Constants.FONT_COLOR_DEFAULT);
    }
  }

  @Override
  public ConditionDataEntry getConditionDataEntry() {
    if (this.conditionType == ConditionType.HAS_ITEM_IN_HAND) {
      return new ConditionDataEntry(
              this.conditionType,
              getHandItemType(),
              getOperationType(),
              getItemName(),
              getQuantity())
          .withCustomData(getCustomData());
    }

    if (this.conditionType == ConditionType.HAS_ITEM_IN_INVENTORY) {
      return new ConditionDataEntry(
              this.conditionType, getOperationType(), getItemName(), getQuantity())
          .withCustomData(getCustomData());
    }

    return new ConditionDataEntry(
        this.conditionType, ConditionOperationType.NONE, getItemName(), 0);
  }

  private ConditionOperationType getOperationType() {
    return this.operationTypeButton != null
        ? this.operationTypeButton.get()
        : ConditionOperationType.EQUALS;
  }

  private HandItemType getHandItemType() {
    boolean mainSelected = this.mainHandCheckbox != null && this.mainHandCheckbox.selected();
    boolean offSelected = this.offHandCheckbox != null && this.offHandCheckbox.selected();
    if (mainSelected && offSelected) {
      return HandItemType.BOTH;
    } else if (mainSelected) {
      return HandItemType.MAIN_HAND;
    } else if (offSelected) {
      return HandItemType.OFF_HAND;
    }
    return HandItemType.BOTH;
  }

  private String getItemName() {
    return this.itemNameTextField != null ? this.itemNameTextField.getValue().trim() : "";
  }

  private int getQuantity() {
    if (this.quantityTextField == null) {
      return 0;
    }
    int quantity;
    try {
      quantity = Integer.parseInt(this.quantityTextField.getValue().trim());
    } catch (NumberFormatException ignored) {
      return 0;
    }
    return quantity > 1 ? quantity : 0;
  }

  private String getCustomData() {
    return this.customDataTextField != null ? this.customDataTextField.getValue().trim() : "";
  }

  private String getStoredCustomData() {
    return this.conditionDataEntry.customData();
  }
}
