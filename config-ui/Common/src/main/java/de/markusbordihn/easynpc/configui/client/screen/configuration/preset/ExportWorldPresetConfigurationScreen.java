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

import de.markusbordihn.easynpc.configui.client.screen.components.ExportButton;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class ExportWorldPresetConfigurationScreen<T extends ConfigurationMenu>
    extends ExportPresetConfigurationScreen<T> {

  public ExportWorldPresetConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    this.worldExportPresetButton.active = false;

    this.addPresetMetadataWidgets(
        TextComponent.getTranslatedConfigText("export_preset_world_text"), this.imageWidth - 25);

    this.exportPresetButton =
        this.addRenderableWidget(
            new ExportButton(
                this.contentLeftPos + 65,
                this.bottomPos - 40,
                150,
                20,
                "export_world_preset",
                button -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .exportWorldPreset(
                          this.getEasyNPCUUID(), this.nameBox.getValue(), this.getPresetMetadata());
                  this.exportPresetButton.active = false;
                }));
  }
}
