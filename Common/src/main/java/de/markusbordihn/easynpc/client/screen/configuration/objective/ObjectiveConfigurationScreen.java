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
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class ObjectiveConfigurationScreen<T extends ConfigurationMenu>
    extends ConfigurationScreen<T> {

  protected static final int SPACE_BETWEEN_ENTRIES = 20;
  protected final ObjectiveDataSet objectiveDataSet;
  protected Button basicObjectiveButton;
  protected Button followObjectiveButton;
  protected Button attackObjectiveButton;
  protected Button lookObjectiveButton;

  public ObjectiveConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.objectiveDataSet = this.getObjectiveDataSet();
  }

  protected Checkbox getObjectiveCheckbox(int left, int top, ObjectiveType objectiveType) {
    return this.getObjectiveCheckbox(left, top, objectiveType, 0.6D);
  }

  protected Checkbox getObjectiveCheckbox(
      int left, int top, ObjectiveType objectiveType, double speedModifier) {
    return new Checkbox(
        left,
        top,
        objectiveType.getObjectiveName(),
        objectiveDataSet.hasObjective(objectiveType),
        checkbox -> {
          ObjectiveDataEntry objectiveDataEntry =
              objectiveDataSet.getOrCreateObjective(objectiveType);
          objectiveDataEntry.setSpeedModifier(speedModifier);
          if (checkbox.selected()) {
            NetworkMessageHandlerManager.getServerHandler()
                .addObjective(this.getNpcUUID(), objectiveDataEntry);
          } else {
            NetworkMessageHandlerManager.getServerHandler()
                .removeObjective(this.getNpcUUID(), objectiveDataEntry);
          }
        });
  }

  @Override
  public void init() {
    super.init();

    // Objective Types
    this.basicObjectiveButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                60,
                "basic",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getNpcUUID(), ConfigurationType.BASIC_OBJECTIVE)));

    this.followObjectiveButton =
        this.addRenderableWidget(
            new TextButton(
                this.basicObjectiveButton.x + this.basicObjectiveButton.getWidth(),
                this.buttonTopPos,
                60,
                "follow",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getNpcUUID(), ConfigurationType.FOLLOW_OBJECTIVE)));

    this.attackObjectiveButton =
        this.addRenderableWidget(
            new TextButton(
                this.followObjectiveButton.x + this.followObjectiveButton.getWidth(),
                this.buttonTopPos,
                60,
                "attack",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getNpcUUID(), ConfigurationType.ATTACK_OBJECTIVE)));

    this.lookObjectiveButton =
        this.addRenderableWidget(
            new TextButton(
                this.attackObjectiveButton.x + this.attackObjectiveButton.getWidth(),
                this.buttonTopPos,
                65,
                "look",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getNpcUUID(), ConfigurationType.LOOK_OBJECTIVE)));
  }
}
