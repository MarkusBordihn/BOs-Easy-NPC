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

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.ServerNetworkMessageHandler;
import java.util.List;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.ObjectSelectionList;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
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
              if (confirmed && uuid != null) {
                loadPreset(resourceLocation);
                minecraft.setScreen(null);
              } else {
                minecraft.setScreen(this);
              }
            },
            new TranslatableComponent(
                Constants.TEXT_PREFIX + "preset.importQuestion",
                resourceLocation
                    .getPath()
                    .substring(resourceLocation.getPath().lastIndexOf("/") + 1)),
            new TranslatableComponent(
                Constants.TEXT_PREFIX + "preset.importWarning",
                this.easyNPC.getEntity().getDisplayName().getString()),
            new TranslatableComponent(Constants.TEXT_PREFIX + "preset.importButton"),
            CommonComponents.GUI_CANCEL));
  }

  public void loadPreset(ResourceLocation resourceLocation) {
    // Define the preset loading action.
  }

  public String getPresetFileName(ResourceLocation resourceLocation) {
    return resourceLocation
        .getPath()
        .replace("preset/" + this.skinModel.toString().toLowerCase() + "/", "")
        .replace(Constants.NPC_NBT_SUFFIX, "");
  }

  @Override
  public void init() {
    super.init();

    // Import buttons
    int buttonWidth = 70;
    this.localImportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                buttonWidth - 4,
                "local",
                button ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.LOCAL_PRESET_IMPORT)));
    this.localImportPresetButton.active = false;
    this.defaultImportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.localImportPresetButton.x + this.localImportPresetButton.getWidth(),
                this.buttonTopPos,
                buttonWidth - 4,
                "default",
                button ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.DEFAULT_PRESET_IMPORT)));
    this.defaultImportPresetButton.active = false;
    this.worldImportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.defaultImportPresetButton.x + this.defaultImportPresetButton.getWidth(),
                this.buttonTopPos,
                buttonWidth - 6,
                "world_preset",
                button ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.WORLD_PRESET_IMPORT)));
    this.worldImportPresetButton.active = false;
    this.customImportPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.worldImportPresetButton.x + this.worldImportPresetButton.getWidth(),
                this.buttonTopPos,
                buttonWidth + 15,
                "custom",
                button ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.CUSTOM_PRESET_IMPORT)));
    this.customImportPresetButton.active = false;

    // Default button stats
    this.localImportPresetButton.active =
        this.hasPermissions(
                COMMON.localImportPresetConfigurationEnabled.get(),
                COMMON.localImportPresetConfigurationAllowInCreative.get(),
                COMMON.localImportPresetConfigurationPermissionLevel.get())
            && CustomPresetDataFiles.hasPresetFiles(this.skinModel);
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

    // Preset Selection List
    this.presetSelectionList =
        new ImportPresetConfigurationScreen<T>.ImportFileSelectionList(this.minecraft);
    this.addWidget(this.presetSelectionList);
    ImportPresetConfigurationScreen.updateSelectedPreset(null);

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
    this.importPresetButton.active = false;
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);
    this.presetSelectionList.render(poseStack, x, y, partialTicks);
    this.importPresetButton.active = ImportPresetConfigurationScreen.selectedPreset != null;
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    int fileListTop = this.topPos + 55;
    int fileListHeight = fileListTop + 110;
    int fileListWidth = this.leftPos + 290;

    // File Selection List
    fill(
        poseStack,
        this.contentLeftPos - 1,
        fileListTop - 1,
        fileListWidth + 1,
        fileListHeight + 1,
        0xff000000);
    fill(poseStack, this.contentLeftPos, fileListTop, fileListWidth, fileListHeight, 0xffaaaaaa);
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
      this.setRenderTopAndBottom(false);
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
                        resourceLocation, skinModel);
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
    protected boolean isFocused() {
      return ImportPresetConfigurationScreen.this.getFocused() == this;
    }

    @Override
    public void render(PoseStack poseStack, int x, int y, float partialTicks) {
      if (this.getItemCount() > 0) {
        super.render(poseStack, x, y, partialTicks);
        return;
      }

      // Display "No presets found" message.
      Text.drawConfigStringShadow(
          poseStack,
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
          PoseStack poseStack,
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
        fill(
            poseStack,
            ImportPresetConfigurationScreen.this.contentLeftPos - 1,
            fileListTop - 4,
            fileListWidth + 1,
            fileListTop + 12,
            0xff000000);
        fill(
            poseStack,
            ImportPresetConfigurationScreen.this.contentLeftPos,
            fileListTop - 3,
            fileListWidth,
            fileListTop + 11,
            0xff888888);
        Text.drawConfigStringShadowWithData(
            poseStack,
            ImportPresetConfigurationScreen.this.font,
            importPresetHeaderLabel,
            this.skinModel,
            ImportPresetConfigurationScreen.this.contentLeftPos + 3,
            fileListTop,
            Constants.FONT_COLOR_WHITE);

        // Display file name.
        Text.drawStringShadow(
            poseStack,
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
        log.debug("Selected file {}.", this.resourceLocation);

        // Set selected preset.
        ImportPresetConfigurationScreen.updateSelectedPreset(this.resourceLocation);
      }

      public Component getNarration() {
        return new TextComponent(this.resourceLocation.getPath());
      }
    }
  }
}
