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

package de.markusbordihn.easynpc.configui.client.screen.configuration.preset;

import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class ExportPresetConfigurationScreen<T extends ConfigurationMenu>
    extends ConfigurationScreen<T> {

  protected Button localExportPresetButton;
  protected Button customExportPresetButton;
  protected Button worldExportPresetButton;

  public ExportPresetConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private static boolean isSinglePlayerNotLan() {
    Minecraft mc = Minecraft.getInstance();
    if (!mc.isLocalServer()) {
      return false;
    }

    var integratedServer = mc.getSingleplayerServer();
    return integratedServer == null || !integratedServer.isPublished();
  }

  @Override
  public void init() {
    super.init();

    boolean singlePlayer = isSinglePlayerNotLan();
    int buttonWidth = 92;

    // Local Export tab — always visible and accessible
    this.localExportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                singlePlayer ? buttonWidth + 16 : buttonWidth,
                "local",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(
                            getEasyNPCUUID(), ConfigurationType.LOCAL_PRESET_EXPORT)));
    if (this.isConfigurationBlockedByPermission(ConfigurationType.LOCAL_PRESET_EXPORT)) {
      this.localExportPresetButton.active = false;
      this.localExportPresetButton.setTooltip(
          Tooltip.create(TextComponent.getTranslatedConfigText("menu.tooltip.no_permission")));
    }

    // Custom Export tab — hidden in single-player (same dir as local), visible on servers
    if (!singlePlayer) {
      this.customExportPresetButton =
          this.addRenderableWidget(
              new TextButton(
                  this.localExportPresetButton.getX() + this.localExportPresetButton.getWidth(),
                  this.buttonTopPos,
                  buttonWidth,
                  "custom",
                  button ->
                      NetworkMessageHandlerManager.getServerHandler()
                          .openConfiguration(
                              getEasyNPCUUID(), ConfigurationType.CUSTOM_PRESET_EXPORT)));
      if (this.isConfigurationBlockedByPermission(ConfigurationType.CUSTOM_PRESET_EXPORT)) {
        this.customExportPresetButton.active = false;
        this.customExportPresetButton.setTooltip(
            Tooltip.create(TextComponent.getTranslatedConfigText("menu.tooltip.no_permission")));
      }
    }

    int worldButtonX =
        singlePlayer
            ? this.localExportPresetButton.getX() + this.localExportPresetButton.getWidth()
            : this.customExportPresetButton.getX() + this.customExportPresetButton.getWidth();
    this.worldExportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                worldButtonX,
                this.buttonTopPos,
                buttonWidth,
                "world_preset",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(
                            getEasyNPCUUID(), ConfigurationType.WORLD_PRESET_EXPORT)));
    if (this.isConfigurationBlockedByPermission(ConfigurationType.WORLD_PRESET_EXPORT)) {
      this.worldExportPresetButton.active = false;
      this.worldExportPresetButton.setTooltip(
          Tooltip.create(TextComponent.getTranslatedConfigText("menu.tooltip.no_permission")));
    }
  }
}
