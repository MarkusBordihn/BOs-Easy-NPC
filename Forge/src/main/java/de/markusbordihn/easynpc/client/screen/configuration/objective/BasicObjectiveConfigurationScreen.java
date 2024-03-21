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
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.menu.configuration.objective.BasicObjectiveConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class BasicObjectiveConfigurationScreen
    extends ObjectiveConfigurationScreen<BasicObjectiveConfigurationMenu> {

  // Basic Objective Checkbox
  protected Checkbox moveBackToHomeCheckbox;
  protected Checkbox strollRandomAroundCheckbox;
  protected Checkbox waterAvoidingRandomStrollCheckbox;
  protected Checkbox moveBackToVillageCheckbox;
  protected Checkbox moveThroughVillageCheckbox;
  protected Checkbox randomStrollInVillageCheckbox;
  protected Checkbox randomSwimmingCheckbox;
  protected Checkbox panicCheckbox;
  protected Checkbox avoidSunCheckbox;
  protected Checkbox fleeSunCheckbox;

  public BasicObjectiveConfigurationScreen(
      BasicObjectiveConfigurationMenu menu, Inventory inventory, Component component) {
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
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.RANDOM_STROLL.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.RANDOM_STROLL),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.RANDOM_STROLL, 5);
                  objectiveDataEntry.setSpeedModifier(0.8F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Move Back To Home
    this.moveBackToHomeCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesSecondColumn,
                objectiveEntriesTop,
                ObjectiveType.MOVE_BACK_TO_HOME.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.MOVE_BACK_TO_HOME),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.MOVE_BACK_TO_HOME, 3);
                  objectiveDataEntry.setSpeedModifier(0.6F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Water Avoiding Random Stroll
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.waterAvoidingRandomStrollCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.WATER_AVOIDING_RANDOM_STROLL.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.WATER_AVOIDING_RANDOM_STROLL),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.WATER_AVOIDING_RANDOM_STROLL, 5);
                  objectiveDataEntry.setSpeedModifier(0.6F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Move Through Village
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.moveThroughVillageCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.MOVE_THROUGH_VILLAGE.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.MOVE_THROUGH_VILLAGE),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.MOVE_THROUGH_VILLAGE, 5);
                  objectiveDataEntry.setSpeedModifier(0.6F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Move Back To Village
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.moveBackToVillageCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.MOVE_BACK_TO_VILLAGE.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.MOVE_BACK_TO_VILLAGE),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.MOVE_BACK_TO_VILLAGE, 3);
                  objectiveDataEntry.setSpeedModifier(0.6F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Random Stroll In Village
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.randomStrollInVillageCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.RANDOM_STROLL_IN_VILLAGE.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.RANDOM_STROLL_IN_VILLAGE),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.RANDOM_STROLL_IN_VILLAGE, 2);
                  objectiveDataEntry.setSpeedModifier(0.6F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Random Swimming
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.randomSwimmingCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.RANDOM_SWIMMING.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.RANDOM_SWIMMING),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.RANDOM_SWIMMING, 4);
                  objectiveDataEntry.setSpeedModifier(0.8F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Panic
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.panicCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.PANIC.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.PANIC),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.PANIC, 1);
                  objectiveDataEntry.setSpeedModifier(1.0D);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Avoid Sun
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.avoidSunCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.AVOID_SUN.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.AVOID_SUN),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.AVOID_SUN, 2);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Flee Sun
    this.fleeSunCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesSecondColumn,
                objectiveEntriesTop,
                ObjectiveType.FLEE_SUN.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.FLEE_SUN),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FLEE_SUN, 3);
                  objectiveDataEntry.setSpeedModifier(1.0D);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));
  }
}
