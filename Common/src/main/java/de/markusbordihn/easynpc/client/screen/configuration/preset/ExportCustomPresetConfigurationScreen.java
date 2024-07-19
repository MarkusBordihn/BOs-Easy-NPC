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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.message.NetworkMessageHandlerManager;
import java.io.File;
import java.util.Collections;
import java.util.List;
import net.minecraft.Util;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

public class ExportCustomPresetConfigurationScreen<T extends ConfigurationMenu>
    extends ExportPresetConfigurationScreen<T> {

  protected Button exportPresetButton;
  protected Button openCustomExportPresetFolder;
  protected int numberOfTextLines = 1;
  private File customPresetFile;
  private String customPresetFileName = "";
  private EditBox nameBox;
  private List<FormattedCharSequence> textComponents = Collections.emptyList();

  public ExportCustomPresetConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void validateName() {
    String nameValue = this.nameBox.getValue();
    this.exportPresetButton.active = !nameValue.isEmpty();

    // Update filename
    customPresetFileName = nameValue;

    // Re-render filename for info text.
    this.textComponents =
        this.font.split(
            Component.translatable(
                Constants.TEXT_CONFIG_PREFIX + "export_preset_text",
                customPresetFile.getParentFile(),
                customPresetFileName),
            this.imageWidth - 25);
    this.numberOfTextLines = this.textComponents.size();
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.customExportPresetButton.active = false;

    // Preset file
    customPresetFile = CustomPresetDataFiles.getPresetFile(getSkinModel(), getNpcUUID());
    customPresetFileName = customPresetFile.getName();

    // Name Edit Box
    this.nameBox = new TextField(this.font, this.contentLeftPos + 5, this.bottomPos - 65, 270);
    this.nameBox.setMaxLength(64);
    this.nameBox.setValue(customPresetFileName);
    this.nameBox.setResponder(consumer -> this.validateName());
    this.addRenderableWidget(this.nameBox);

    // Pre-format text
    this.textComponents =
        this.font.split(
            Component.translatable(
                Constants.TEXT_CONFIG_PREFIX + "export_preset_text",
                customPresetFile.getParentFile(),
                customPresetFile.getName()),
            this.imageWidth - 25);
    this.numberOfTextLines = this.textComponents.size();

    // Open custom export preset folder button
    this.openCustomExportPresetFolder =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos + 5,
                this.bottomPos - 95,
                275,
                "open_custom_export_preset_folder",
                button -> Util.getPlatform().openFile(customPresetFile.getParentFile())));

    // Export button
    this.exportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos + 65,
                this.bottomPos - 40,
                150,
                "export",
                button -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .exportPreset(getNpcUUID(), this.nameBox.getValue());
                  exportPresetButton.active = false;
                }));
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    if (!this.textComponents.isEmpty()) {
      for (int line = 0; line < this.numberOfTextLines; ++line) {
        FormattedCharSequence formattedCharSequence = this.textComponents.get(line);
        Text.drawString(
            guiGraphics,
            this.font,
            formattedCharSequence,
            leftPos + 15,
            topPos + 45 + (line * (font.lineHeight + 2)));
      }
    }
  }
}
