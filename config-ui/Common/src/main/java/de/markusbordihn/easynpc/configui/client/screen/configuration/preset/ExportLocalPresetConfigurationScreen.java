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

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.ExportButton;
import de.markusbordihn.easynpc.configui.client.screen.components.FileBrowserButton;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.preset.PresetExportFormat;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.io.File;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import net.minecraft.Util;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

public class ExportLocalPresetConfigurationScreen<T extends ConfigurationMenu>
    extends ExportPresetConfigurationScreen<T> {

  private static final Set<String> PRESET_CATEGORIES =
      new LinkedHashSet<>(List.of("General", "Villager", "Guard", "Trader", "Quest", "Custom"));
  private static boolean lastSnbtFormatPreference = false;
  protected Button exportPresetButton;
  protected Button openLocalExportPresetFolder;
  protected int numberOfTextLines = 1;
  private File localPresetFile;
  private String localPresetFileName = "";
  private EditBox nameBox;
  private SpinButton<String> categorySpinButton;
  private EditBox versionBox;
  private EditBox descriptionBox;
  private EditBox authorBox;
  private Checkbox snbtCheckbox;
  private List<FormattedCharSequence> textComponents = Collections.emptyList();

  public ExportLocalPresetConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    if (this.localExportPresetButton != null) {
      this.localExportPresetButton.active = false;
    }

    this.localPresetFile = CustomPresetDataFiles.getPresetFile(getSkinModel(), getEasyNPCUUID());
    this.localPresetFileName =
        PresetExportFormat.removePresetExtension(this.localPresetFile.getName());

    this.textComponents =
        this.font.split(
            TextComponent.getTranslatedConfigText("export_preset_text", getSkinModel().getName()),
            this.imageWidth - 30);
    this.numberOfTextLines = this.textComponents.size();

    this.nameBox = new TextField(this.font, this.contentLeftPos + 5, this.bottomPos - 140, 300);
    this.nameBox.setMaxLength(64);
    this.nameBox.setValue(localPresetFileName);
    this.addRenderableWidget(this.nameBox);

    int metaDataYOffset = this.nameBox.getY() + 35;
    String defaultCategory = "General";
    String defaultAuthor =
        this.minecraft != null && this.minecraft.player != null
            ? this.minecraft.player.getName().getString()
            : "Unknown";

    this.categorySpinButton =
        new SpinButton<>(
            this.contentLeftPos + 5,
            metaDataYOffset,
            130,
            16,
            PRESET_CATEGORIES,
            defaultCategory,
            null);
    this.addRenderableWidget(this.categorySpinButton);

    this.versionBox = new TextField(this.font, this.contentLeftPos + 230, metaDataYOffset, 75);
    this.versionBox.setMaxLength(12);
    this.versionBox.setValue("1.0.0");
    this.addRenderableWidget(this.versionBox);

    this.descriptionBox =
        new TextField(this.font, this.contentLeftPos + 5, metaDataYOffset + 35, 180);
    this.descriptionBox.setMaxLength(128);
    this.descriptionBox.setValue("");
    this.addRenderableWidget(this.descriptionBox);

    this.authorBox = new TextField(this.font, this.contentLeftPos + 200, metaDataYOffset + 35, 105);
    this.authorBox.setMaxLength(16);
    this.authorBox.setValue(defaultAuthor);
    this.addRenderableWidget(this.authorBox);

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
                    showSnbtWarning();
                  } else {
                    exportPreset();
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
                button -> Util.getPlatform().openFile(localPresetFile.getParentFile())));
  }

  private void showSnbtWarning() {
    if (this.minecraft != null) {
      this.minecraft.setScreen(
          new ConfirmScreen(
              confirmed -> {
                if (confirmed) {
                  exportPreset();
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
    String displayName = this.nameBox.getValue();
    NetworkMessageHandlerManager.getServerHandler()
        .exportPreset(
            getEasyNPCUUID(),
            displayName,
            this.snbtCheckbox.selected() ? PresetExportFormat.SNBT : PresetExportFormat.NBT,
            PresetMetadata.createDefault(displayName, this.authorBox.getValue())
                .withCategory(this.categorySpinButton.get())
                .withVersion(this.versionBox.getValue())
                .withDescription(this.descriptionBox.getValue()));
  }

  @Override
  public void updateTick() {
    super.updateTick();
    this.exportPresetButton.active = this.nameBox != null && !this.nameBox.getValue().isEmpty();
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
            topPos + 25 + (line * (font.lineHeight + 2)));
      }
    }

    if (this.categorySpinButton != null) {
      Text.drawString(
          guiGraphics,
          this.font,
          Component.literal("Category:"),
          this.categorySpinButton.getX(),
          this.categorySpinButton.getY() - 10);
    }
    if (this.versionBox != null) {
      Text.drawString(
          guiGraphics,
          this.font,
          Component.literal("Version:"),
          this.versionBox.getX(),
          this.versionBox.getY() - 10);
    }
    if (this.authorBox != null) {
      Text.drawString(
          guiGraphics,
          this.font,
          Component.literal("Author:"),
          this.authorBox.getX(),
          this.authorBox.getY() - 10);
    }
    if (this.descriptionBox != null) {
      Text.drawString(
          guiGraphics,
          this.font,
          Component.literal("Description:"),
          this.descriptionBox.getX(),
          this.descriptionBox.getY() - 10);
    }
  }
}
