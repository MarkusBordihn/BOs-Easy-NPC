/**
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

import java.util.List;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.ObjectSelectionList;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import com.mojang.blaze3d.vertex.PoseStack;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import de.markusbordihn.easynpc.menu.configuration.preset.WorldImportPresetConfigurationMenu;

@OnlyIn(Dist.CLIENT)
public class ImportWorldPresetConfigurationScreen
    extends ImportPresetConfigurationScreen<WorldImportPresetConfigurationMenu> {

  // Buttons
  protected Button importPresetButton;

  // Preset Selection List
  private ImportWorldPresetConfigurationScreen.ImportFileSelectionList presetSelectionList;

  // Cache
  protected static ResourceLocation selectedPreset;
  protected static List<ResourceLocation> worldPresets;

  public ImportWorldPresetConfigurationScreen(WorldImportPresetConfigurationMenu menu,
      Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  public static void updateSelectedPreset(ResourceLocation resourceLocation) {
    selectedPreset = resourceLocation;
  }

  public static void updateWorldPresets(List<ResourceLocation> presets) {
    worldPresets = presets;
  }

  public void loadPresetConfirm(ResourceLocation resourceLocation) {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(new ConfirmScreen(confirmed -> {
      if (confirmed && uuid != null) {
        loadPreset(resourceLocation);
        minecraft.setScreen((Screen) null);
      } else {
        minecraft.setScreen(this);
      }
    }, Component.translatable(Constants.TEXT_PREFIX + "preset.importQuestion",
        resourceLocation.getPath().substring(resourceLocation.getPath().lastIndexOf("/") + 1)),
        Component.translatable(Constants.TEXT_PREFIX + "preset.importWarning",
            this.entity.getDisplayName().getString()),
        Component.translatable(Constants.TEXT_PREFIX + "preset.importButton"),
        CommonComponents.GUI_CANCEL));
  }

  public void loadPreset(ResourceLocation resourceLocation) {
    NetworkMessageHandler.importWorldPreset(uuid, resourceLocation);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.worldImportPresetButton.active = false;

    // World Preset Files
    log.info("Found {} world preset files.", this.menu.getWorldPresets().size());
    ImportWorldPresetConfigurationScreen.updateWorldPresets(this.menu.getWorldPresets());

    // File Selection List
    this.presetSelectionList =
        new ImportWorldPresetConfigurationScreen.ImportFileSelectionList(this.minecraft);
    this.addWidget(this.presetSelectionList);
    ImportWorldPresetConfigurationScreen.updateSelectedPreset(null);

    // Import button
    this.importPresetButton = this.addRenderableWidget(menuButton(this.buttonLeftPos + 25,
        this.bottomPos - 40, 220, "import_world_preset", button -> {
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
    this.importPresetButton.active = ImportWorldPresetConfigurationScreen.selectedPreset != null;
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    int fileListTop = this.topPos + 55;
    int fileListHeight = fileListTop + 110;
    int fileListWidth = this.leftPos + 290;

    // File Selection List
    fill(poseStack, this.contentLeftPos - 1, fileListTop - 1, fileListWidth + 1, fileListHeight + 1,
        0xff000000);
    fill(poseStack, this.contentLeftPos, fileListTop, fileListWidth, fileListHeight, 0xffaaaaaa);

    // File Selection List Header
    fill(poseStack, this.contentLeftPos - 1, fileListTop - 4, fileListWidth + 1, fileListTop + 12,
        0xff000000);
    fill(poseStack, this.contentLeftPos, fileListTop - 3, fileListWidth, fileListTop + 11,
        0xff888888);
    this.font.drawShadow(poseStack,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + "preset_world_for", this.skinModel),
        this.contentLeftPos + 3f, fileListTop, 16777215);
  }

  @OnlyIn(Dist.CLIENT)
  class ImportFileSelectionList extends
      ObjectSelectionList<ImportWorldPresetConfigurationScreen.ImportFileSelectionList.Entry> {
    public ImportFileSelectionList(Minecraft minecraft) {
      super(minecraft, ImportWorldPresetConfigurationScreen.this.width - 5,
          ImportWorldPresetConfigurationScreen.this.height - 150 + 66,
          ImportWorldPresetConfigurationScreen.this.topPos + 66,
          ImportWorldPresetConfigurationScreen.this.height - 150
              - ImportWorldPresetConfigurationScreen.this.topPos + 66,
          14);
      this.setRenderHeader(false, 0);
      this.setRenderBackground(false);
      this.setRenderTopAndBottom(false);

      // Read relevant preset files.
      ImportWorldPresetConfigurationScreen.worldPresets.forEach(resourceLocation -> {
        if (!resourceLocation.getPath()
            .startsWith("preset/" + skinModel.toString().toLowerCase() + "/")) {
          log.warn("Skipping preset file {} as it does not match the current skin model {}",
              resourceLocation, skinModel.toString().toLowerCase());
          return;
        }
        ImportWorldPresetConfigurationScreen.ImportFileSelectionList.Entry entry =
            new ImportWorldPresetConfigurationScreen.ImportFileSelectionList.Entry(resourceLocation,
                skinModel);
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
      return ImportWorldPresetConfigurationScreen.this.getFocused() == this;
    }

    @Override
    public void render(PoseStack poseStack, int x, int y, float partialTicks) {
      if (this.getItemCount() > 0) {
        super.render(poseStack, x, y, partialTicks);
        return;
      }

      // Display "No presets found" message.
      ImportWorldPresetConfigurationScreen.this.font.drawShadow(poseStack,
          Component.translatable(Constants.TEXT_CONFIG_PREFIX + "no_presets_found"),
          ImportWorldPresetConfigurationScreen.this.contentLeftPos + 80f,
          ImportWorldPresetConfigurationScreen.this.topPos + 105f, 16777215);
    }

    @OnlyIn(Dist.CLIENT)
    public class Entry extends
        ObjectSelectionList.Entry<ImportWorldPresetConfigurationScreen.ImportFileSelectionList.Entry> {
      final ResourceLocation resourceLocation;
      final SkinModel skinModel;
      final String fileName;

      public Entry(ResourceLocation resourceLocation, SkinModel skinModel) {
        this.resourceLocation = resourceLocation;
        this.skinModel = skinModel;
        this.fileName = this.resourceLocation.getNamespace() + ':'
            + this.resourceLocation.getPath()
                .replace("preset/" + this.skinModel.toString().toLowerCase() + "/", "")
                .replace(Constants.NPC_NBT_SUFFIX, "");
      }

      public void render(PoseStack poseStack, int x, int y, int unused1, int unused2, int unused3,
          int unused4, int unused5, boolean unused6, float partialTicks) {
        // Display file name.
        ImportWorldPresetConfigurationScreen.this.font.drawShadow(poseStack, fileName,
            ImportFileSelectionList.this.width / 2f
                - ImportWorldPresetConfigurationScreen.this.font.width(this.fileName) / 2f,
            y + 1f, 16777215, true);
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
        log.info("Selected file {}.", this.resourceLocation);

        // Set selected preset.
        ImportWorldPresetConfigurationScreen.updateSelectedPreset(this.resourceLocation);
      }

      public Component getNarration() {
        return Component.literal(this.resourceLocation.getPath());
      }
    }
  }

}
