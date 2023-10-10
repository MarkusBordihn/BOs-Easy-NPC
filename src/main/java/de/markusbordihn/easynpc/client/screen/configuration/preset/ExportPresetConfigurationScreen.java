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

package de.markusbordihn.easynpc.client.screen.configuration.preset;

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
public class ExportPresetConfigurationScreen<T extends ConfigurationMenu>
    extends ConfigurationScreen<T> {

  // Buttons
  protected Button customExportPresetButton;
  protected Button worldExportPresetButton;

  public ExportPresetConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Import buttons
    int buttonWidth = 92;
    this.customExportPresetButton = this.addRenderableWidget(menuButton(this.buttonLeftPos,
        this.buttonTopPos, buttonWidth + 16, "custom_preset", button -> {
          NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.CUSTOM_PRESET_EXPORT);
        }));
    this.customExportPresetButton.active = false;
    this.worldExportPresetButton = this.addRenderableWidget(
        menuButton(this.customExportPresetButton.getX() + this.customExportPresetButton.getWidth(),
            this.buttonTopPos, buttonWidth, "world_preset", button -> {
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.WORLD_PRESET_EXPORT);
            }));
    this.worldExportPresetButton.active = false;

    // Default button stats
    this.customExportPresetButton.active =
        this.hasPermissions(COMMON.customExportPresetConfigurationEnabled.get(),
            COMMON.customExportPresetConfigurationAllowInCreative.get(),
            COMMON.customExportPresetConfigurationPermissionLevel.get());
    this.worldExportPresetButton.active =
        this.hasPermissions(COMMON.worldExportPresetConfigurationEnabled.get(),
            COMMON.worldExportPresetConfigurationAllowInCreative.get(),
            COMMON.worldExportPresetConfigurationPermissionLevel.get());
  }

}
