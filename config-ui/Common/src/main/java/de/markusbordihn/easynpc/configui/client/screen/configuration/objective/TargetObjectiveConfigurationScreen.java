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

package de.markusbordihn.easynpc.configui.client.screen.configuration.objective;

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.HelpIcon;
import de.markusbordihn.easynpc.configui.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.configui.client.screen.components.WarningIcon;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveGroup;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.Function;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class TargetObjectiveConfigurationScreen<T extends ConfigurationMenu>
    extends ObjectiveConfigurationScreen<T> {

  protected Checkbox attackHostileFactionsCheckbox;
  protected Checkbox attackPlayerByNameCheckbox;
  protected Checkbox attackEntityByTeamCheckbox;
  protected Checkbox attackEntityByTagCheckbox;
  protected Checkbox attackEntityByUUIDCheckbox;
  protected WarningIcon missingAttackObjectiveWarning;

  public TargetObjectiveConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.targetObjectiveButton.active = false;

    int objectiveEntriesTop = this.contentTopPos + 10;
    int objectiveEntriesFirstColumn = this.contentLeftPos + 5;
    int objectiveEntriesSecondColumn = this.contentLeftPos + 145;

    // Attack hostile factions.
    this.attackHostileFactionsCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.ATTACK_HOSTILE_FACTIONS));

    // Faction targeting explanation and warning icon for enabled target objectives without any
    // attack objective. Both icons are placed at the right edge to avoid long checkbox labels.
    this.addRenderableWidget(
        new HelpIcon(
            this.contentLeftPos + 286, objectiveEntriesTop, "attack_hostile_factions.tooltip"));
    this.missingAttackObjectiveWarning =
        this.addRenderableWidget(
            new WarningIcon(
                this.contentLeftPos + 302,
                objectiveEntriesTop,
                TextComponent.getTranslatedConfigText("warning.no_attack_objective")));
    this.missingAttackObjectiveWarning.visible = false;

    // Target conditions with input fields.
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES + 10;
    this.attackPlayerByNameCheckbox =
        this.addObjectiveTargetRow(
            objectiveEntriesFirstColumn,
            objectiveEntriesSecondColumn,
            objectiveEntriesTop,
            ObjectiveType.ATTACK_PLAYER_BY_NAME,
            ObjectiveDataEntry::getTargetPlayerName,
            ObjectiveDataEntry::setTargetPlayerName);

    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.attackEntityByTeamCheckbox =
        this.addObjectiveTargetRow(
            objectiveEntriesFirstColumn,
            objectiveEntriesSecondColumn,
            objectiveEntriesTop,
            ObjectiveType.ATTACK_ENTITY_BY_TEAM,
            ObjectiveDataEntry::getTargetTeamName,
            ObjectiveDataEntry::setTargetTeamName);

    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.attackEntityByTagCheckbox =
        this.addObjectiveTargetRow(
            objectiveEntriesFirstColumn,
            objectiveEntriesSecondColumn,
            objectiveEntriesTop,
            ObjectiveType.ATTACK_ENTITY_BY_TAG,
            ObjectiveDataEntry::getTargetEntityTag,
            ObjectiveDataEntry::setTargetEntityTag);

    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.attackEntityByUUIDCheckbox =
        this.addObjectiveTargetRow(
            objectiveEntriesFirstColumn,
            objectiveEntriesSecondColumn,
            objectiveEntriesTop,
            ObjectiveType.ATTACK_ENTITY_BY_UUID,
            objectiveDataEntry ->
                objectiveDataEntry.getTargetEntityUUID() != null
                    ? objectiveDataEntry.getTargetEntityUUID().toString()
                    : "",
            (objectiveDataEntry, value) -> {
              try {
                objectiveDataEntry.setTargetEntityUUID(
                    value != null && !value.isEmpty() ? UUID.fromString(value) : null);
              } catch (IllegalArgumentException exception) {
                log.error("Unable to parse UUID {} for {}", value, this.getEasyNPCUUID());
              }
            });
  }

  private boolean hasAnyAttackObjective() {
    for (ObjectiveType attackObjectiveType : ObjectiveGroup.ATTACK_TYPE) {
      if (this.objectiveDataSet.hasObjective(attackObjectiveType)) {
        return true;
      }
    }
    return false;
  }

  private boolean hasAnyTargetObjectiveSelected() {
    return (this.attackHostileFactionsCheckbox != null
            && this.attackHostileFactionsCheckbox.selected())
        || (this.attackPlayerByNameCheckbox != null && this.attackPlayerByNameCheckbox.selected())
        || (this.attackEntityByTeamCheckbox != null && this.attackEntityByTeamCheckbox.selected())
        || (this.attackEntityByTagCheckbox != null && this.attackEntityByTagCheckbox.selected())
        || (this.attackEntityByUUIDCheckbox != null && this.attackEntityByUUIDCheckbox.selected());
  }

  private Checkbox addObjectiveTargetRow(
      int checkboxLeft,
      int textFieldLeft,
      int top,
      ObjectiveType objectiveType,
      Function<ObjectiveDataEntry, String> targetValueGetter,
      BiConsumer<ObjectiveDataEntry, String> targetValueSetter) {
    String savedValue = "";
    if (this.objectiveDataSet.hasObjective(objectiveType)) {
      String targetValue =
          targetValueGetter.apply(this.objectiveDataSet.getObjective(objectiveType));
      if (targetValue != null) {
        savedValue = targetValue;
      }
    }
    String[] savedValueHolder = {savedValue};

    TextField targetValueField = new TextField(this.font, textFieldLeft, top, 125);
    targetValueField.setMaxLength(36);
    targetValueField.setEditable(this.objectiveDataSet.hasObjective(objectiveType));
    targetValueField.setValue(savedValue);

    Checkbox targetCheckbox =
        new Checkbox(
            checkboxLeft,
            top,
            objectiveType.getObjectiveName(),
            this.objectiveDataSet.hasObjective(objectiveType),
            checkbox -> {
              ObjectiveDataEntry objectiveDataEntry =
                  new ObjectiveDataEntry(objectiveType, objectiveType.getDefaultPriority());
              targetValueSetter.accept(objectiveDataEntry, targetValueField.getValue());
              targetValueField.setEditable(checkbox.selected());
              if (!checkbox.selected()) {
                NetworkMessageHandlerManager.getServerHandler()
                    .removeObjective(this.getEasyNPCUUID(), objectiveDataEntry);
              } else if (!targetValueField.getValue().isEmpty()) {
                NetworkMessageHandlerManager.getServerHandler()
                    .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
              }
            });

    SaveButton targetValueSaveButton =
        new SaveButton(
            targetValueField.getX() + targetValueField.getWidth() + 5,
            top - 1,
            onPress -> {
              ObjectiveDataEntry objectiveDataEntry =
                  new ObjectiveDataEntry(objectiveType, objectiveType.getDefaultPriority());
              targetValueSetter.accept(objectiveDataEntry, targetValueField.getValue());
              NetworkMessageHandlerManager.getServerHandler()
                  .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
              savedValueHolder[0] = targetValueField.getValue();
            });
    targetValueField.setResponder(
        value ->
            targetValueSaveButton.active =
                targetCheckbox.selected() && value != null && !value.equals(savedValueHolder[0]));
    targetValueSaveButton.active = false;

    this.addRenderableWidget(targetCheckbox);
    this.addRenderableWidget(targetValueField);
    this.addRenderableWidget(targetValueSaveButton);
    return targetCheckbox;
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    if (this.missingAttackObjectiveWarning != null) {
      this.missingAttackObjectiveWarning.visible =
          this.hasAnyTargetObjectiveSelected() && !this.hasAnyAttackObjective();
    }
    super.extractRenderState(guiGraphics, x, y, partialTicks);
  }

  @Override
  protected void renderBg(
      GuiGraphicsExtractor guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    if (this.attackPlayerByNameCheckbox != null) {
      int y = this.attackPlayerByNameCheckbox.getY() - 3;
      guiGraphics.fillGradient(
          this.contentLeftPos + 5, y, this.contentLeftPos + 300, y + 1, 0x60808080, 0x60808080);
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "target_conditions",
          this.contentLeftPos + 115,
          y - 8,
          0xFF808080);
    }
  }
}
