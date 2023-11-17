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
import de.markusbordihn.easynpc.data.objective.ObjectiveData;
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
  protected Checkbox strollRandomAroundCheckbox;
  protected Checkbox waterAvoidingRandomStrollCheckbox;
  protected Checkbox moveBackToVillageCheckbox;
  protected Checkbox moveThroughVillageCheckbox;
  protected Checkbox randomStrollInVillageCheckbox;
  protected Checkbox randomSwimmingCheckbox;

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

    // Stroll Random Around
    this.strollRandomAroundCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.RANDOM_STROLL.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.RANDOM_STROLL),
                checkbox -> {
                  ObjectiveData objectiveData = new ObjectiveData(ObjectiveType.RANDOM_STROLL, 5);
                  objectiveData.setSpeedModifier(0.8F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveData);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveData);
                  }
                }));

    // Water Avoiding Random Stroll
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.waterAvoidingRandomStrollCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.WATER_AVOIDING_RANDOM_STROLL.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.WATER_AVOIDING_RANDOM_STROLL),
                checkbox -> {
                  ObjectiveData objectiveData =
                      new ObjectiveData(ObjectiveType.WATER_AVOIDING_RANDOM_STROLL, 5);
                  objectiveData.setSpeedModifier(0.6F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveData);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveData);
                  }
                }));

    // Move Through Village
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.moveThroughVillageCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.MOVE_THROUGH_VILLAGE.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.MOVE_THROUGH_VILLAGE),
                checkbox -> {
                  ObjectiveData objectiveData =
                      new ObjectiveData(ObjectiveType.MOVE_THROUGH_VILLAGE, 5);
                  objectiveData.setSpeedModifier(0.6F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveData);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveData);
                  }
                }));

    // Move Back To Village
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.moveBackToVillageCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.MOVE_BACK_TO_VILLAGE.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.MOVE_BACK_TO_VILLAGE),
                checkbox -> {
                  ObjectiveData objectiveData =
                      new ObjectiveData(ObjectiveType.MOVE_BACK_TO_VILLAGE, 3);
                  objectiveData.setSpeedModifier(0.6F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveData);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveData);
                  }
                }));

    // Random Stroll In Village
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.randomStrollInVillageCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.RANDOM_STROLL_IN_VILLAGE.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.RANDOM_STROLL_IN_VILLAGE),
                checkbox -> {
                  ObjectiveData objectiveData =
                      new ObjectiveData(ObjectiveType.RANDOM_STROLL_IN_VILLAGE, 2);
                  objectiveData.setSpeedModifier(0.6F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveData);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveData);
                  }
                }));

    // Random Swimming
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.randomSwimmingCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.RANDOM_SWIMMING.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.RANDOM_SWIMMING),
                checkbox -> {
                  ObjectiveData objectiveData = new ObjectiveData(ObjectiveType.RANDOM_SWIMMING, 4);
                  objectiveData.setSpeedModifier(0.8F);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveData);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveData);
                  }
                }));
  }
}
