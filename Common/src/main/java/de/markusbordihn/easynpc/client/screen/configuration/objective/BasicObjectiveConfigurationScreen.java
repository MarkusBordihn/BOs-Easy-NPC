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

package de.markusbordihn.easynpc.client.screen.configuration.objective;

import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class BasicObjectiveConfigurationScreen<T extends ConfigurationMenu>
    extends ObjectiveConfigurationScreen<T> {
  protected Checkbox moveBackToHomeCheckbox;
  protected Checkbox strollRandomAroundCheckbox;
  protected Checkbox waterAvoidingRandomStrollCheckbox;
  protected Checkbox moveBackToVillageCheckbox;
  protected Checkbox moveThroughVillageCheckbox;
  protected Checkbox randomStrollInVillageCheckbox;
  protected Checkbox randomStrollAroundHomeCheckbox;
  protected Checkbox randomSwimmingCheckbox;
  protected Checkbox panicCheckbox;
  protected Checkbox avoidSunCheckbox;
  protected Checkbox fleeSunCheckbox;

  public BasicObjectiveConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.basicObjectiveButton.active = false;

    int objectiveEntriesTop = this.contentTopPos + 5;
    int objectiveEntriesFirstColumn = this.contentLeftPos + 5;
    int objectiveEntriesSecondColumn = this.contentLeftPos + 145;

    // Stroll Random Around
    this.strollRandomAroundCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.RANDOM_STROLL,
                0.8D));

    // Water Avoiding Random Stroll
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.waterAvoidingRandomStrollCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.WATER_AVOIDING_RANDOM_STROLL));

    // Move Through Village
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.moveThroughVillageCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.MOVE_THROUGH_VILLAGE));

    // Move Back To Village
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.moveBackToVillageCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.MOVE_BACK_TO_VILLAGE));

    // Move Back To Home
    this.moveBackToHomeCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesSecondColumn,
                objectiveEntriesTop,
                ObjectiveType.MOVE_BACK_TO_HOME));

    // Random Stroll In Village
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.randomStrollInVillageCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.RANDOM_STROLL_IN_VILLAGE));

    // Random Stroll Around Home
    this.randomStrollAroundHomeCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesSecondColumn,
                objectiveEntriesTop,
                ObjectiveType.RANDOM_STROLL_AROUND_HOME));

    // Random Swimming
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.randomSwimmingCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.RANDOM_SWIMMING));

    // Panic
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.panicCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.PANIC, 1.0D));

    // Avoid Sun
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.avoidSunCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.AVOID_SUN, 0.8D));

    // Flee Sun
    this.fleeSunCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesSecondColumn, objectiveEntriesTop, ObjectiveType.FLEE_SUN, 1.0D));
  }
}
