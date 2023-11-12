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

package de.markusbordihn.easynpc.client.screen.configuration.preset;

import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class ImportPresetConfigurationScreen<T extends ConfigurationMenu>
    extends ConfigurationScreen<T> {

  // Buttons
  protected Button customImportPresetButton;
  protected Button defaultImportPresetButton;
  protected Button worldImportPresetButton;

  public ImportPresetConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Import buttons
    int buttonWidth = 92;
    this.defaultImportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                buttonWidth - 4,
                "default_preset",
                button -> {
                  NetworkMessageHandler.openConfiguration(
                      uuid, ConfigurationType.DEFAULT_PRESET_IMPORT);
                }));
    this.defaultImportPresetButton.active = false;
    this.worldImportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.defaultImportPresetButton.x + this.defaultImportPresetButton.getWidth(),
                this.buttonTopPos,
                buttonWidth - 6,
                "world_preset",
                button -> {
                  NetworkMessageHandler.openConfiguration(
                      uuid, ConfigurationType.WORLD_PRESET_IMPORT);
                }));
    this.worldImportPresetButton.active = false;
    this.customImportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.worldImportPresetButton.x + this.worldImportPresetButton.getWidth(),
                this.buttonTopPos,
                buttonWidth + 15,
                "custom_preset",
                button -> {
                  NetworkMessageHandler.openConfiguration(
                      uuid, ConfigurationType.CUSTOM_PRESET_IMPORT);
                }));
    this.customImportPresetButton.active = false;

    // Default button stats
    this.defaultImportPresetButton.active =
        this.hasPermissions(
            COMMON.defaultImportPresetConfigurationEnabled.get(),
            COMMON.defaultImportPresetConfigurationAllowInCreative.get(),
            COMMON.defaultImportPresetConfigurationPermissionLevel.get());
    this.worldImportPresetButton.active =
        this.hasPermissions(
            COMMON.worldImportPresetConfigurationEnabled.get(),
            COMMON.worldImportPresetConfigurationAllowInCreative.get(),
            COMMON.worldImportPresetConfigurationPermissionLevel.get());
    this.customImportPresetButton.active =
        this.hasPermissions(
            COMMON.customImportPresetConfigurationEnabled.get(),
            COMMON.customImportPresetConfigurationAllowInCreative.get(),
            COMMON.customImportPresetConfigurationPermissionLevel.get());
  }
}
