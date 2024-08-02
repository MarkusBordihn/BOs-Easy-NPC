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
import de.markusbordihn.easynpc.client.screen.Screen;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import java.util.List;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.ObjectSelectionList;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

public class ImportPresetConfigurationScreen<T extends ConfigurationMenu>
    extends ConfigurationScreen<T> {

  protected static ResourceLocation selectedPreset;
  protected static List<ResourceLocation> presetList;
  protected Button localImportPresetButton;
  protected Button customImportPresetButton;
  protected Button defaultImportPresetButton;
  protected Button worldImportPresetButton;
  protected String importPresetButtonLabel = "import_preset";
  protected String importPresetHeaderLabel = "import_preset_header";
  protected Button importPresetButton;
  ImportPresetConfigurationScreen<T>.ImportFileSelectionList presetSelectionList;

  public ImportPresetConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  public static void updateSelectedPreset(ResourceLocation resourceLocation) {
    selectedPreset = resourceLocation;
  }

  public static void updatePresets(List<ResourceLocation> presets) {
    presetList = presets;
  }

  public static List<ResourceLocation> getPresets() {
    return presetList;
  }

  public static boolean hasNoPresets() {
    return presetList == null || presetList.isEmpty();
  }

  public void loadPresetConfirm(ResourceLocation resourceLocation) {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed && this.getEasyNPCUUID() != null) {
                this.loadPreset(resourceLocation);
                minecraft.setScreen(null);
              } else {
                minecraft.setScreen(this);
              }
            },
            Component.translatable(
                Constants.TEXT_CONFIG_PREFIX + "preset.importQuestion",
                resourceLocation
                    .getPath()
                    .substring(resourceLocation.getPath().lastIndexOf("/") + 1)),
            Component.translatable(
                Constants.TEXT_CONFIG_PREFIX + "preset.importWarning",
                getEasyNPCEntity().getDisplayName().getString()),
            Component.translatable(Constants.TEXT_CONFIG_PREFIX + "preset.importButton"),
            CommonComponents.GUI_CANCEL));
  }

  public void loadPreset(ResourceLocation resourceLocation) {
    log.error("Not implemented! Received resource location {}", resourceLocation);
  }

  public String getPresetFileName(ResourceLocation resourceLocation) {
    this.getSkinModel();
    return resourceLocation
        .getPath()
        .replace("preset/" + getSkinModel().toString().toLowerCase() + "/", "")
        .replace(Constants.NPC_NBT_SUFFIX, "");
  }

  @Override
  public void init() {
    super.init();

    int buttonWidth = 70;
    this.localImportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                buttonWidth - 4,
                "local",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(
                            this.getEasyNPCUUID(), ConfigurationType.LOCAL_PRESET_IMPORT)));

    this.defaultImportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.localImportPresetButton.getX() + this.localImportPresetButton.getWidth(),
                this.buttonTopPos,
                buttonWidth - 4,
                "default",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(
                            this.getEasyNPCUUID(), ConfigurationType.DEFAULT_PRESET_IMPORT)));

    this.worldImportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.defaultImportPresetButton.getX() + this.defaultImportPresetButton.getWidth(),
                this.buttonTopPos,
                buttonWidth - 6,
                "world_preset",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(
                            this.getEasyNPCUUID(), ConfigurationType.WORLD_PRESET_IMPORT)));

    this.customImportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.worldImportPresetButton.getX() + this.worldImportPresetButton.getWidth(),
                this.buttonTopPos,
                buttonWidth + 15,
                "custom",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(
                            this.getEasyNPCUUID(), ConfigurationType.CUSTOM_PRESET_IMPORT)));

    // Import button
    this.importPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos + 25,
                this.bottomPos - 40,
                220,
                importPresetButtonLabel,
                button -> {
                  if (selectedPreset != null) {
                    this.loadPresetConfirm(selectedPreset);
                  }
                }));

    // Preset Selection List
    this.presetSelectionList =
        new ImportPresetConfigurationScreen<T>.ImportFileSelectionList(this.minecraft);
    this.addWidget(this.presetSelectionList);
    ImportPresetConfigurationScreen.updateSelectedPreset(null);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);
    if (this.presetSelectionList != null) {
      this.presetSelectionList.render(guiGraphics, x, y, partialTicks);
    }
    if (this.importPresetButton != null) {
      this.importPresetButton.active = ImportPresetConfigurationScreen.selectedPreset != null;
    }
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    int fileListTop = this.topPos + 55;
    int fileListHeight = fileListTop + 110;
    int fileListWidth = this.leftPos + 290;

    // File Selection List
    guiGraphics.fill(
        this.contentLeftPos - 1,
        fileListTop - 1,
        fileListWidth + 1,
        fileListHeight + 1,
        0xff000000);
    guiGraphics.fill(this.contentLeftPos, fileListTop, fileListWidth, fileListHeight, 0xffaaaaaa);
  }

  class ImportFileSelectionList extends ObjectSelectionList<ImportFileSelectionList.Entry> {
    public ImportFileSelectionList(Minecraft minecraft) {
      super(
          minecraft,
          ImportPresetConfigurationScreen.this.width - 5,
          ImportPresetConfigurationScreen.this.height - 150 + 66,
          ImportPresetConfigurationScreen.this.topPos + 66,
          ImportPresetConfigurationScreen.this.height
              - 150
              - ImportPresetConfigurationScreen.this.topPos
              + 66,
          14);
      this.setRenderHeader(false, 0);
      this.setRenderBackground(false);
      this.updatePresets();
    }

    public void updatePresets() {
      this.clearEntries();

      if (ImportPresetConfigurationScreen.hasNoPresets()) {
        return;
      }

      ImportPresetConfigurationScreen.getPresets()
          .forEach(
              resourceLocation -> {
                ImportPresetConfigurationScreen<T>.ImportFileSelectionList.Entry entry =
                    new ImportPresetConfigurationScreen<T>.ImportFileSelectionList.Entry(
                        resourceLocation, getSkinModel());
                this.addEntry(entry);
              });
    }

    @Override
    protected int getScrollbarPosition() {
      return super.getScrollbarPosition() + 12;
    }

    @Override
    public int getRowWidth() {
      return super.getRowWidth() + 40;
    }

    @Override
    public boolean isFocused() {
      return ImportPresetConfigurationScreen.this.getFocused() == this;
    }

    @Override
    public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
      if (this.getItemCount() > 0) {
        super.render(guiGraphics, x, y, partialTicks);
        return;
      }

      // Display "No presets found" message.
      Text.drawConfigStringShadow(
          guiGraphics,
          ImportPresetConfigurationScreen.this.font,
          "no_presets_found",
          ImportPresetConfigurationScreen.this.contentLeftPos + 80,
          ImportPresetConfigurationScreen.this.topPos + 105,
          Constants.FONT_COLOR_WHITE);
    }

    public class Entry extends ObjectSelectionList.Entry<Entry> {
      final ResourceLocation resourceLocation;
      final SkinModel skinModel;
      final String fileName;

      public Entry(ResourceLocation resourceLocation, SkinModel skinModel) {
        this.resourceLocation = resourceLocation;
        this.skinModel = skinModel;
        this.fileName = getPresetFileName(resourceLocation);
      }

      public void render(
          GuiGraphics guiGraphics,
          int x,
          int y,
          int unused1,
          int unused2,
          int unused3,
          int unused4,
          int unused5,
          boolean unused6,
          float partialTicks) {

        // File Selection List Header
        int fileListTop = ImportPresetConfigurationScreen.this.topPos + 55;
        int fileListWidth = ImportPresetConfigurationScreen.this.leftPos + 290;
        guiGraphics.fill(
            ImportPresetConfigurationScreen.this.contentLeftPos - 1,
            fileListTop - 4,
            fileListWidth + 1,
            fileListTop + 12,
            0xff000000);
        guiGraphics.fill(
            ImportPresetConfigurationScreen.this.contentLeftPos,
            fileListTop - 3,
            fileListWidth,
            fileListTop + 11,
            0xff888888);
        Text.drawConfigStringShadowWithData(
            guiGraphics,
            ImportPresetConfigurationScreen.this.font,
            importPresetHeaderLabel,
            this.skinModel,
            ImportPresetConfigurationScreen.this.contentLeftPos + 3,
            fileListTop,
            Constants.FONT_COLOR_WHITE);

        // Display file name.
        Text.drawStringShadow(
            guiGraphics,
            ImportPresetConfigurationScreen.this.font,
            fileName,
            ImportPresetConfigurationScreen.ImportFileSelectionList.this.width / 2
                - ImportPresetConfigurationScreen.this.font.width(this.fileName) / 2,
            y + 1,
            Constants.FONT_COLOR_WHITE);
      }

      @Override
      public boolean mouseClicked(double unused1, double unused2, int button) {
        if (button == 0) {
          this.select();
          return true;
        } else {
          return false;
        }
      }

      private void select() {
        ImportPresetConfigurationScreen.ImportFileSelectionList.this.setSelected(this);
        Screen.log.debug("Selected file {}.", this.resourceLocation);

        // Set selected preset.
        ImportPresetConfigurationScreen.updateSelectedPreset(this.resourceLocation);
      }

      @Override
      public Component getNarration() {
        return Component.literal(this.resourceLocation.getPath());
      }
    }
  }
}
