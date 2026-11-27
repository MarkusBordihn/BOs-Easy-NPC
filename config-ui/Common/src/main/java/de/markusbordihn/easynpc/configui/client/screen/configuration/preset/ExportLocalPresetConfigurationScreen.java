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

import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.ExportButton;
import de.markusbordihn.easynpc.configui.client.screen.components.FileBrowserButton;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.preset.PresetExportFormat;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.io.File;
import net.minecraft.Util;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class ExportLocalPresetConfigurationScreen<T extends ConfigurationMenu>
    extends ExportPresetConfigurationScreen<T> {

  private static boolean lastSnbtFormatPreference = false;
  protected Button openLocalExportPresetFolder;
  private File localPresetFile;
  private Checkbox snbtCheckbox;

  public ExportLocalPresetConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    if (this.localExportPresetButton != null) {
      this.localExportPresetButton.active = false;
    }

    this.localPresetFile =
        CustomPresetDataFiles.getPresetFile(this.getSkinModel(), this.getEasyNPCUUID());
    this.addPresetMetadataWidgets(
        TextComponent.getTranslatedConfigText("export_preset_text", this.getSkinModel().getName()),
        this.imageWidth - 30);

    this.exportPresetButton =
        this.addRenderableWidget(
            new ExportButton(
                this.contentLeftPos + 5,
                this.bottomPos - 45,
                140,
                20,
                "export_local_preset",
                button -> {
                  if (this.snbtCheckbox.selected()) {
                    this.showSnbtWarning();
                  } else {
                    this.exportPreset();
                  }
                }));

    this.snbtCheckbox =
        new Checkbox(
            this.exportPresetButton.getX(),
            this.exportPresetButton.getY() + 20,
            "export_preset.use_snbt_format",
            lastSnbtFormatPreference,
            checkbox -> lastSnbtFormatPreference = checkbox.selected());
    this.addRenderableWidget(this.snbtCheckbox);

    this.openLocalExportPresetFolder =
        this.addRenderableWidget(
            new FileBrowserButton(
                this.exportPresetButton.getX() + this.exportPresetButton.getWidth() + 20,
                this.exportPresetButton.getY(),
                140,
                20,
                "open_export_folder",
                button -> Util.getPlatform().openFile(this.localPresetFile.getParentFile())));
  }

  private void showSnbtWarning() {
    if (this.minecraft != null) {
      this.minecraft.setScreen(
          new ConfirmScreen(
              confirmed -> {
                if (confirmed) {
                  this.exportPreset();
                }
                this.minecraft.setScreen(this);
              },
              TextComponent.getTranslatedConfigText("export_preset.snbt_warning_title"),
              TextComponent.getTranslatedConfigText("export_preset.snbt_warning_message"),
              TextComponent.getTranslatedConfigText("export_preset.snbt_confirm"),
              CommonComponents.GUI_CANCEL));
    }
  }

  private void exportPreset() {
    NetworkMessageHandlerManager.getServerHandler()
        .exportPreset(
            this.getEasyNPCUUID(),
            this.nameBox.getValue(),
            this.snbtCheckbox.selected() ? PresetExportFormat.SNBT : PresetExportFormat.NBT,
            this.getPresetMetadata());
  }

  @Override
  public void updateTick() {
    super.updateTick();
    this.exportPresetButton.active = this.nameBox != null && !this.nameBox.getValue().isEmpty();
  }
}
