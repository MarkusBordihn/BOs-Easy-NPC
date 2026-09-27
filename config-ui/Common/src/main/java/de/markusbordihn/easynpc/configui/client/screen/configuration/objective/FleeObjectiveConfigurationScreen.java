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

package de.markusbordihn.easynpc.configui.client.screen.configuration.objective;

import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class FleeObjectiveConfigurationScreen<T extends ConfigurationMenu>
    extends ObjectiveConfigurationScreen<T> {

  protected Checkbox fleeSunCheckbox;
  protected Checkbox fleeCreeperCheckbox;
  protected Checkbox fleeMonsterCheckbox;
  protected Checkbox fleeMobCheckbox;
  protected Checkbox fleePlayerCheckbox;
  protected Checkbox fleeVillagerCheckbox;

  public FleeObjectiveConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    this.fleeObjectiveButton.active = false;

    int objectiveEntriesTop = this.contentTopPos + 5;
    int objectiveEntriesFirstColumn = this.contentLeftPos + 5;
    int objectiveEntriesSecondColumn = this.contentLeftPos + 145;

    this.fleeSunCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.FLEE_SUN, 1.0D));

    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.fleeCreeperCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.FLEE_CREEPER));

    this.fleeMonsterCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesSecondColumn, objectiveEntriesTop, ObjectiveType.FLEE_MONSTER));

    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.fleeMobCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.FLEE_MOB));

    this.fleePlayerCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesSecondColumn, objectiveEntriesTop, ObjectiveType.FLEE_PLAYER));

    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.fleeVillagerCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.FLEE_VILLAGER));
  }
}
