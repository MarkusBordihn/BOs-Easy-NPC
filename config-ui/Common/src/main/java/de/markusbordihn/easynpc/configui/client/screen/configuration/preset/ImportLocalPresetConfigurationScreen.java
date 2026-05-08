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

import de.markusbordihn.easynpc.configui.client.screen.components.ReloadButton;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.io.LocalPresetDataFiles;
import de.markusbordihn.easynpc.io.PresetFileHandler;
import java.nio.file.Path;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

public class ImportLocalPresetConfigurationScreen<T extends ConfigurationMenu>
    extends ImportPresetConfigurationScreen<T> {

  public ImportLocalPresetConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    importPresetButtonLabel = "import_local_preset";
    importPresetHeaderLabel = "preset_local_for";
  }

  private void reloadLocalPresets() {
    CustomPresetDataFiles.refreshPresetResourceLocations();
    updatePresets(LocalPresetDataFiles.getPresetResourceLocations(this.getSkinModel()).toList());
    if (this.presetSelectionList != null) {
      this.presetSelectionList.updatePresets();
    }
  }

  @Override
  public void loadPreset(ResourceLocation resourceLocation) {
    try {
      Path presetFilePath = LocalPresetDataFiles.getPresetsResourceLocationPath(resourceLocation);
      CompoundTag compoundTag = PresetFileHandler.load(presetFilePath.toFile());
      if (compoundTag == null || compoundTag.isEmpty()) {
        log.error("Failed to load local preset file {}:", resourceLocation);
        return;
      }

      NetworkMessageHandlerManager.getServerHandler()
          .importLocalPreset(getEasyNPCUUID(), compoundTag, resourceLocation);
    } catch (Exception e) {
      log.error("Failed to import local preset file {}:", resourceLocation, e);
    }
  }

  @Override
  public void init() {
    super.init();

    this.localImportPresetButton.active = false;

    this.addRenderableWidget(
        new ReloadButton(
            this.importPresetButton.getX() + this.importPresetButton.getWidth() + 4,
            this.bottomPos - 40,
            20,
            20,
            "",
            button -> reloadLocalPresets()));

    reloadLocalPresets();
  }
}
