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

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.ConditionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.RelationshipType;
import de.markusbordihn.easynpc.data.faction.FactionNameValidator;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.stream.Collectors;
import net.minecraft.client.gui.GuiGraphics;

public class RelationshipConditionEntry extends ConditionEntryWidget {

  private SpinButton<RelationshipType> relationshipTypeButton;
  private TextField factionNameTextField;

  public RelationshipConditionEntry(
      ConditionDataEntry conditionDataEntry,
      ConditionDataSet conditionDataSet,
      ConditionDataEntryEditorContainerScreen<?> screen) {
    super(conditionDataEntry, conditionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    RelationshipType relationshipType = RelationshipType.OWNER;
    if (hasConditionData(ConditionType.RELATIONSHIP)
        && this.conditionDataEntry.subType() instanceof RelationshipType entryRelationshipType) {
      relationshipType = entryRelationshipType;
    }

    this.relationshipTypeButton =
        this.screen.addConditionEntryWidget(
            new SpinButton<>(
                editorLeft + 110,
                editorTop,
                180,
                16,
                Arrays.stream(RelationshipType.values())
                    .collect(Collectors.toCollection(LinkedHashSet::new)),
                relationshipType,
                button -> this.updateFactionNameVisibility()));

    this.factionNameTextField =
        this.screen.addConditionEntryWidget(
            new TextField(this.font, editorLeft + 110, editorTop + 24, 180, 16));
    this.factionNameTextField.setFilter(FactionNameValidator::isValidInput);
    this.factionNameTextField.setValue(
        hasConditionData(ConditionType.RELATIONSHIP) ? this.conditionDataEntry.name() : "");
    this.updateFactionNameVisibility();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.relationship.type",
        editorLeft,
        editorTop + 4,
        Constants.FONT_COLOR_BLACK);

    if (this.factionNameTextField == null || !this.factionNameTextField.visible) {
      return;
    }

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.relationship.faction",
        editorLeft,
        editorTop + 28,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.relationship.faction.hint",
        editorLeft,
        editorTop + 44,
        Constants.FONT_COLOR_GRAY);
  }

  @Override
  public ConditionDataEntry getConditionDataEntry() {
    RelationshipType relationshipType = this.getRelationshipType();
    return new ConditionDataEntry(
        ConditionType.RELATIONSHIP,
        relationshipType,
        ConditionOperationType.NONE,
        relationshipType.isFactionRelation() ? this.factionNameTextField.getValue() : "",
        0);
  }

  private RelationshipType getRelationshipType() {
    return this.relationshipTypeButton != null
        ? this.relationshipTypeButton.get()
        : RelationshipType.OWNER;
  }

  private void updateFactionNameVisibility() {
    if (this.factionNameTextField != null) {
      this.factionNameTextField.visible = this.getRelationshipType().isFactionRelation();
    }
  }
}
