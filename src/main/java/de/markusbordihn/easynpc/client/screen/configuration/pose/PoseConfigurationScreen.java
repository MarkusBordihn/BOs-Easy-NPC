/**
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

package de.markusbordihn.easynpc.client.screen.configuration.pose;

import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;

@OnlyIn(Dist.CLIENT)
public class PoseConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  // Buttons
  protected Button defaultPoseButton;
  protected Button advancedPoseButton;
  protected Button customPoseButton;

  public PoseConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Pose Types
    int poseButtonWidth = 80;
    this.defaultPoseButton = this.addRenderableWidget(menuButton(this.buttonLeftPos,
        this.buttonTopPos, poseButtonWidth - 10, "default_pose", button -> {
          NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.DEFAULT_POSE);
        }));

    this.advancedPoseButton =
        this.addRenderableWidget(menuButton(this.buttonLeftPos + this.defaultPoseButton.getWidth(),
            this.buttonTopPos, poseButtonWidth + 20, "advanced_pose", button -> {
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.ADVANCED_POSE);
            }));

    this.customPoseButton = this
        .addRenderableWidget(menuButton(advancedPoseButton.getX() + advancedPoseButton.getWidth(),
            this.buttonTopPos, poseButtonWidth + 20, "custom_pose", button -> {
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.CUSTOM_POSE);
            }));

    // Default button stats
    this.defaultPoseButton.active =
        this.hasPermissions(COMMON.defaultPoseConfigurationEnabled.get(),
            COMMON.defaultPoseConfigurationAllowInCreative.get(),
            COMMON.defaultPoseConfigurationPermissionLevel.get());
    this.advancedPoseButton.active =
        this.hasPermissions(COMMON.advancedPoseConfigurationEnabled.get(),
            COMMON.advancedPoseConfigurationAllowInCreative.get(),
            COMMON.advancedPoseConfigurationPermissionLevel.get());
    this.customPoseButton.active = this.hasPermissions(COMMON.customPoseConfigurationEnabled.get(),
        COMMON.customPoseConfigurationAllowInCreative.get(),
        COMMON.customPoseConfigurationPermissionLevel.get());
  }

}
