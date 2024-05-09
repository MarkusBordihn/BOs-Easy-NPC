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

import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.menu.configuration.objective.ObjectiveConfigurationMenu;
import de.markusbordihn.easynpc.network.ServerNetworkMessageHandler;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class ObjectiveConfigurationScreen<T extends ObjectiveConfigurationMenu>
    extends ConfigurationScreen<T> {

  protected static final int SPACE_BETWEEN_ENTRIES = 20;
  protected final ObjectiveDataSet objectiveDataSet;
  protected Button basicObjectiveButton;
  protected Button followObjectiveButton;
  protected Button attackObjectiveButton;
  protected Button lookObjectiveButton;

  public ObjectiveConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.objectiveDataSet = menu.getObjectiveDataSet();
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
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.BASIC_OBJECTIVE)));

    this.followObjectiveButton =
        this.addRenderableWidget(
            new TextButton(
                this.basicObjectiveButton.x + this.basicObjectiveButton.getWidth(),
                this.buttonTopPos,
                60,
                "follow",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.FOLLOW_OBJECTIVE)));

    this.attackObjectiveButton =
        this.addRenderableWidget(
            new TextButton(
                this.followObjectiveButton.x + this.followObjectiveButton.getWidth(),
                this.buttonTopPos,
                60,
                "attack",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.ATTACK_OBJECTIVE)));

    this.lookObjectiveButton =
        this.addRenderableWidget(
            new TextButton(
                this.attackObjectiveButton.x + this.attackObjectiveButton.getWidth(),
                this.buttonTopPos,
                65,
                "look",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.LOOK_OBJECTIVE)));

    // Default button stats
    this.basicObjectiveButton.active =
        this.hasPermissions(
            COMMON.basicObjectiveConfigurationEnabled.get(),
            COMMON.basicObjectiveConfigurationAllowInCreative.get(),
            COMMON.basicObjectiveConfigurationPermissionLevel.get());
    this.attackObjectiveButton.active =
        this.hasPermissions(
            COMMON.attackObjectiveConfigurationEnabled.get(),
            COMMON.attackObjectiveConfigurationAllowInCreative.get(),
            COMMON.attackObjectiveConfigurationPermissionLevel.get());
    this.followObjectiveButton.active =
        this.hasPermissions(
            COMMON.followObjectiveConfigurationEnabled.get(),
            COMMON.followObjectiveConfigurationAllowInCreative.get(),
            COMMON.followObjectiveConfigurationPermissionLevel.get());
    this.lookObjectiveButton.active =
        this.hasPermissions(
            COMMON.lookObjectiveConfigurationEnabled.get(),
            COMMON.lookObjectiveConfigurationAllowInCreative.get(),
            COMMON.lookObjectiveConfigurationPermissionLevel.get());
  }
}
