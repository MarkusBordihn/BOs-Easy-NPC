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
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.menu.configuration.preset.DefaultImportPresetConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import java.io.IOException;
import java.io.InputStream;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.ObjectSelectionList;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.packs.resources.Resource;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class ImportDefaultPresetConfigurationScreen
    extends ImportPresetConfigurationScreen<DefaultImportPresetConfigurationMenu> {

  // Cache
  protected static ResourceLocation selectedPreset;
  // Buttons
  protected Button importPresetButton;
  // Preset Selection List
  private ImportDefaultPresetConfigurationScreen.ImportFileSelectionList presetSelectionList;

  public ImportDefaultPresetConfigurationScreen(
      DefaultImportPresetConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  public static void updateSelectedPreset(ResourceLocation resourceLocation) {
    selectedPreset = resourceLocation;
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
    // Load resource file as input stream.
    InputStream inputStream;
    try {
      Resource resource =
          Minecraft.getInstance().getResourceManager().getResource(resourceLocation);
      inputStream = resource.getInputStream();
    } catch (IOException exception) {
      log.error("Failed to load resource {}", resourceLocation, exception);
      return;
    }

    // Read NBT data from file.
    CompoundTag compoundTag;
    try {
      compoundTag = NbtIo.readCompressed(inputStream);
    } catch (IOException exception) {
      log.error("Failed to read NBT data from {}", resourceLocation, exception);
      return;
    }
    if (compoundTag == null) {
      log.error("Received empty preset {}", resourceLocation);
      return;
    }

    // Remove UUID from NBT data, if present.
    if (compoundTag.contains("UUID")) {
      compoundTag.remove("UUID");
    }

    // Remove position from NBT data, if present.
    if (compoundTag.contains("Pos")) {
      compoundTag.remove("Pos");
    }

    NetworkMessageHandler.importPreset(uuid, compoundTag);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.defaultImportPresetButton.active = false;

    // File Selection List
    this.presetSelectionList =
        new ImportDefaultPresetConfigurationScreen.ImportFileSelectionList(this.minecraft);
    this.addWidget(this.presetSelectionList);
    ImportDefaultPresetConfigurationScreen.updateSelectedPreset(null);

    // Import button
    this.importPresetButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos + 25,
                this.bottomPos - 40,
                220,
                "import_default_preset",
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
    this.importPresetButton.active = ImportDefaultPresetConfigurationScreen.selectedPreset != null;
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

  @OnlyIn(Dist.CLIENT)
  class ImportFileSelectionList
      extends ObjectSelectionList<
          ImportDefaultPresetConfigurationScreen.ImportFileSelectionList.Entry> {
    public ImportFileSelectionList(Minecraft minecraft) {
      super(
          minecraft,
          ImportDefaultPresetConfigurationScreen.this.width - 5,
          ImportDefaultPresetConfigurationScreen.this.height - 150 + 66,
          ImportDefaultPresetConfigurationScreen.this.topPos + 66,
          ImportDefaultPresetConfigurationScreen.this.height
              - 150
              - ImportDefaultPresetConfigurationScreen.this.topPos
              + 66,
          14);
      this.setRenderHeader(false, 0);
      this.setRenderBackground(false);
      this.setRenderTopAndBottom(false);

      // Read relevant preset files.
      Minecraft.getInstance()
          .getResourceManager()
          .listResources(
              "preset", resourceLocation -> resourceLocation.endsWith(Constants.NPC_NBT_SUFFIX))
          .forEach(
              resourceLocation -> {
                if (!resourceLocation
                    .getPath()
                    .toLowerCase()
                    .startsWith("preset/" + skinModel.toString().toLowerCase() + "/")) {
                  log.warn(
                      "Skipping preset file {} as it does not match the current skin model {}",
                      resourceLocation,
                      skinModel.toString().toLowerCase());
                  return;
                }
                ImportDefaultPresetConfigurationScreen.ImportFileSelectionList.Entry entry =
                    new ImportDefaultPresetConfigurationScreen.ImportFileSelectionList.Entry(
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
      return ImportDefaultPresetConfigurationScreen.this.getFocused() == this;
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
          ImportDefaultPresetConfigurationScreen.this.font,
          "no_presets_found",
          ImportDefaultPresetConfigurationScreen.this.contentLeftPos + 80,
          ImportDefaultPresetConfigurationScreen.this.topPos + 105,
          Constants.FONT_COLOR_WHITE);
    }

    @OnlyIn(Dist.CLIENT)
    public class Entry
        extends ObjectSelectionList.Entry<
            ImportDefaultPresetConfigurationScreen.ImportFileSelectionList.Entry> {
      final ResourceLocation resourceLocation;
      final SkinModel skinModel;
      final String fileName;

      public Entry(ResourceLocation resourceLocation, SkinModel skinModel) {
        this.resourceLocation = resourceLocation;
        this.skinModel = skinModel;
        this.fileName =
            this.resourceLocation.getNamespace()
                + ':'
                + this.resourceLocation
                    .getPath()
                    .replace("preset/" + this.skinModel.toString().toLowerCase() + "/", "")
                    .replace(Constants.NPC_NBT_SUFFIX, "");
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
        int fileListTop = ImportDefaultPresetConfigurationScreen.this.topPos + 55;
        int fileListWidth = ImportDefaultPresetConfigurationScreen.this.leftPos + 290;
        fill(
            poseStack,
            ImportDefaultPresetConfigurationScreen.this.contentLeftPos - 1,
            fileListTop - 4,
            fileListWidth + 1,
            fileListTop + 12,
            0xff000000);
        fill(
            poseStack,
            ImportDefaultPresetConfigurationScreen.this.contentLeftPos,
            fileListTop - 3,
            fileListWidth,
            fileListTop + 11,
            0xff888888);
        Text.drawConfigStringShadowWithData(
            poseStack,
            ImportDefaultPresetConfigurationScreen.this.font,
            "preset_default_for",
            this.skinModel,
            ImportDefaultPresetConfigurationScreen.this.contentLeftPos + 3,
            fileListTop,
            Constants.FONT_COLOR_WHITE);

        // Display file name.
        Text.drawStringShadow(
            poseStack,
            ImportDefaultPresetConfigurationScreen.this.font,
            fileName,
            ImportFileSelectionList.this.width / 2
                - ImportDefaultPresetConfigurationScreen.this.font.width(this.fileName) / 2,
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
        ImportFileSelectionList.this.setSelected(this);

        // Set selected preset.
        ImportDefaultPresetConfigurationScreen.updateSelectedPreset(this.resourceLocation);
      }

      public Component getNarration() {
        return new TextComponent(this.resourceLocation.getPath());
      }
    }
  }
}
