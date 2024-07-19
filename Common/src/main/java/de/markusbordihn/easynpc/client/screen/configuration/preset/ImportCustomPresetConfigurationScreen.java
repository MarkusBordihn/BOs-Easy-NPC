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

import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.message.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.Set;
import java.util.stream.Collectors;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

public class ImportCustomPresetConfigurationScreen<T extends ConfigurationMenu>
    extends ImportPresetConfigurationScreen<T> {

  private final Set<ResourceLocation> customPresets;

  public ImportCustomPresetConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    importPresetButtonLabel = "import_custom_preset";
    importPresetHeaderLabel = "preset_custom_for";
    this.customPresets =
        CompoundTagUtils.readResourceLocations(this.additionalScreenData.getList("CustomPresets"))
            .stream()
            .filter(
                resourceLocation ->
                    resourceLocation.getPath().contains("/" + this.getSkinModel().getName() + "/"))
            .collect(Collectors.toSet());
  }

  @Override
  public void loadPreset(ResourceLocation resourceLocation) {
    NetworkMessageHandlerManager.getServerHandler()
        .importCustomPreset(getNpcUUID(), resourceLocation);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.customImportPresetButton.active = false;

    // Update custom presets
    updatePresets(this.customPresets.stream().toList());
    this.presetSelectionList.updatePresets();
  }
}
