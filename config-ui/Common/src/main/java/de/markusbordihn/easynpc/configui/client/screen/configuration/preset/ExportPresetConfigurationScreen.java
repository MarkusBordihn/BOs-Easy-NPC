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
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.preset.PresetExportFormat;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.AbstractWidget;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

public class ExportPresetConfigurationScreen<T extends ConfigurationMenu>
    extends ConfigurationScreen<T> {

  private static final Set<String> PRESET_CATEGORIES =
      new LinkedHashSet<>(List.of("General", "Villager", "Guard", "Trader", "Quest", "Custom"));

  protected Button localExportPresetButton;
  protected Button customExportPresetButton;
  protected Button worldExportPresetButton;
  protected Button exportPresetButton;
  protected EditBox nameBox;
  private SpinButton<String> categorySpinButton;
  private EditBox versionBox;
  private EditBox descriptionBox;
  private EditBox authorBox;
  private List<FormattedCharSequence> textComponents = Collections.emptyList();

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

  protected void addPresetMetadataWidgets(Component descriptionText, int descriptionTextWidth) {
    this.textComponents = this.font.split(descriptionText, descriptionTextWidth);

    this.nameBox = new TextField(this.font, this.contentLeftPos + 5, this.bottomPos - 140, 300);
    this.nameBox.setMaxLength(64);
    this.nameBox.setValue(
        PresetExportFormat.removePresetExtension(
            CustomPresetDataFiles.getPresetFile(this.getSkinModel(), this.getEasyNPCUUID())
                .getName()));
    this.nameBox.setResponder(name -> this.exportPresetButton.active = !name.isEmpty());
    this.addRenderableWidget(this.nameBox);

    int metaDataYOffset = this.nameBox.getY() + 35;
    this.categorySpinButton =
        new SpinButton<>(
            this.contentLeftPos + 5, metaDataYOffset, 130, 16, PRESET_CATEGORIES, "General", null);
    this.addRenderableWidget(this.categorySpinButton);

    this.versionBox = new TextField(this.font, this.contentLeftPos + 230, metaDataYOffset, 75);
    this.versionBox.setMaxLength(12);
    this.versionBox.setValue("1.0.0");
    this.addRenderableWidget(this.versionBox);

    this.descriptionBox =
        new TextField(this.font, this.contentLeftPos + 5, metaDataYOffset + 35, 180);
    this.descriptionBox.setMaxLength(128);
    this.addRenderableWidget(this.descriptionBox);

    this.authorBox = new TextField(this.font, this.contentLeftPos + 200, metaDataYOffset + 35, 105);
    this.authorBox.setMaxLength(16);
    this.authorBox.setValue(
        this.minecraft != null && this.minecraft.player != null
            ? this.minecraft.player.getName().getString()
            : "Unknown");
    this.addRenderableWidget(this.authorBox);
  }

  protected PresetMetadata getPresetMetadata() {
    return PresetMetadata.createDefault(this.nameBox.getValue(), this.authorBox.getValue())
        .withCategory(this.categorySpinButton.get())
        .withVersion(this.versionBox.getValue())
        .withDescription(this.descriptionBox.getValue());
  }

  @Override
  public void init() {
    super.init();

    boolean singlePlayer = isSinglePlayerNotLan();
    int buttonWidth = 92;

    this.localExportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                singlePlayer ? buttonWidth + 16 : buttonWidth,
                "local",
                button -> this.openConfiguration(ConfigurationType.LOCAL_PRESET_EXPORT)));
    this.blockButtonWithoutPermission(
        this.localExportPresetButton, ConfigurationType.LOCAL_PRESET_EXPORT);

    // Hidden in single-player, where it would export into the same directory as local.
    if (!singlePlayer) {
      this.customExportPresetButton =
          this.addRenderableWidget(
              new TextButton(
                  this.localExportPresetButton.getX() + this.localExportPresetButton.getWidth(),
                  this.buttonTopPos,
                  buttonWidth,
                  "custom",
                  button -> this.openConfiguration(ConfigurationType.CUSTOM_PRESET_EXPORT)));
      this.blockButtonWithoutPermission(
          this.customExportPresetButton, ConfigurationType.CUSTOM_PRESET_EXPORT);
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
                button -> this.openConfiguration(ConfigurationType.WORLD_PRESET_EXPORT)));
    this.blockButtonWithoutPermission(
        this.worldExportPresetButton, ConfigurationType.WORLD_PRESET_EXPORT);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    for (int line = 0; line < this.textComponents.size(); ++line) {
      Text.drawString(
          guiGraphics,
          this.font,
          this.textComponents.get(line),
          this.leftPos + 15,
          this.topPos + 25 + (line * (this.font.lineHeight + 2)));
    }

    if (this.categorySpinButton == null) {
      return;
    }

    this.drawFieldLabel(guiGraphics, "Category:", this.categorySpinButton);
    this.drawFieldLabel(guiGraphics, "Version:", this.versionBox);
    this.drawFieldLabel(guiGraphics, "Author:", this.authorBox);
    this.drawFieldLabel(guiGraphics, "Description:", this.descriptionBox);
  }

  private void drawFieldLabel(GuiGraphics guiGraphics, String label, AbstractWidget widget) {
    Text.drawString(
        guiGraphics, this.font, Component.literal(label), widget.getX(), widget.getY() - 10);
  }
}
