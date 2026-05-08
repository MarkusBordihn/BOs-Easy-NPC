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

package de.markusbordihn.easynpc.configui.client.screen.preset;

import de.markusbordihn.easynpc.client.screen.components.DrawBorder;
import de.markusbordihn.easynpc.client.screen.components.DrawBox;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.io.ClientDefaultPresetDataFiles;
import de.markusbordihn.easynpc.io.LocalPresetDataFiles;
import de.markusbordihn.easynpc.security.PresetFeaturePreview;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.ObjectSelectionList;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.entity.Entity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetListEntry extends ObjectSelectionList.Entry<PresetListEntry> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final float TEXT_SCALE = 0.75f;
  private static final int PREVIEW_BOX_SIZE = 24;

  private final ResourceLocation preset;
  private final PresetMetadata metadata;
  private final PresetType presetType;
  private final PresetBrowserScreen screen;
  private PresetData presetData;
  private PresetFeaturePreview securityPreview;
  private EasyNPC<?> previewNPC;

  public PresetListEntry(
      ResourceLocation preset,
      PresetMetadata metadata,
      PresetType presetType,
      PresetBrowserScreen screen) {
    this.preset = preset;
    this.metadata = metadata;
    this.presetType = presetType;
    this.screen = screen;
    loadPresetData();
  }

  private void loadPresetData() {
    try {
      // LOCAL presets can be loaded directly from client files
      if (this.presetType == PresetType.LOCAL) {
        this.presetData = LocalPresetDataFiles.loadPresetData(this.preset);

        if (this.presetData != null && this.presetData.hasValidData()) {
          this.securityPreview = this.screen.createSecurityPreview(this.presetData);
          loadPreviewNPC();
        } else {
          log.warn("Invalid PresetData for LOCAL preset: {}", this.preset);
        }
        return;
      }

      // DEFAULT presets can be loaded directly from client JAR
      if (this.presetType == PresetType.DEFAULT) {
        this.presetData = ClientDefaultPresetDataFiles.loadDefaultPresetData(this.preset);

        if (this.presetData != null && this.presetData.hasValidData()) {
          this.securityPreview = this.screen.createSecurityPreview(this.presetData);
          loadPreviewNPC();
        } else {
          log.warn("Invalid PresetData for DEFAULT preset: {}", this.preset);
        }
        return;
      }

      // For other preset types (CUSTOM, DATA, WORLD), try to get server instance
      MinecraftServer server = Minecraft.getInstance().getSingleplayerServer();

      if (server != null) {
        this.presetData = PresetHandler.loadPreset(this.preset, this.presetType, server);
        if (this.presetData != null && this.presetData.hasValidData()) {
          this.securityPreview = this.screen.createSecurityPreview(this.presetData);
          loadPreviewNPC();
        } else {
          log.warn("Invalid PresetData for: {}", this.preset);
        }
      } else {
        CompoundTag syncedTag = this.screen.getPresetDataFromSync(this.preset, this.presetType);
        if (syncedTag != null) {
          this.presetData = PresetData.fromCompoundTag(this.preset, this.presetType, syncedTag);
          if (this.presetData != null && this.presetData.hasValidData()) {
            this.securityPreview = this.screen.createSecurityPreview(this.presetData);
            loadPreviewNPC();
          } else {
            log.warn("Invalid synced PresetData for: {}", this.preset);
          }
        } else {
          log.debug(
              "Preset preview not available on dedicated server for: {} (spawning will still work)",
              this.preset);
        }
      }
    } catch (Exception e) {
      log.error("Failed to load preset data for {}: {}", this.preset, e.getMessage());
    }
  }

  private void loadPreviewNPC() {
    try {
      Minecraft minecraft = Minecraft.getInstance();
      if (minecraft.level == null || this.presetData == null) {
        return;
      }

      Entity entity = this.presetData.entityType().create(minecraft.level);
      if (entity instanceof EasyNPC<?> easyNPC) {
        entity.load(this.presetData.data());
        this.previewNPC = easyNPC;
      } else if (entity != null) {
        log.warn("Preset {} is not an EasyNPC entity", this.preset);
      }
    } catch (Exception e) {
      log.error("Failed to create preview NPC for {}: {}", this.preset, e.getMessage());
    }
  }

  public void cleanup() {
    if (this.previewNPC != null && this.previewNPC.getEntity() != null) {
      this.previewNPC.getEntity().discard();
      this.previewNPC = null;
    }
  }

  public ResourceLocation getPreset() {
    return preset;
  }

  public PresetMetadata getMetadata() {
    return metadata;
  }

  public PresetType getPresetType() {
    return presetType;
  }

  public PresetData getPresetData() {
    return presetData;
  }

  public PresetFeaturePreview getSecurityPreview() {
    return this.securityPreview;
  }

  public EasyNPC<?> getPreviewNPC() {
    return previewNPC;
  }

  @Override
  public void render(
      GuiGraphics guiGraphics,
      int index,
      int top,
      int left,
      int width,
      int height,
      int mouseX,
      int mouseY,
      boolean isMouseOver,
      float partialTicks) {

    guiGraphics.fill(
        left,
        top,
        left + width,
        top + height,
        this.screen.isSelected(this.preset) ? 0x80FFFFFF : isMouseOver ? 0x40FFFFFF : 0);

    int previewBoxX = left + 1;
    int previewBoxY = top + 1;

    renderPreviewBox(guiGraphics, previewBoxX, previewBoxY, mouseX, mouseY);
    renderPresetInfo(guiGraphics, left, top);
  }

  private void renderPreviewBox(GuiGraphics guiGraphics, int x, int y, int mouseX, int mouseY) {
    DrawBox.draw(guiGraphics, x, y, PREVIEW_BOX_SIZE, PREVIEW_BOX_SIZE);
    DrawBorder.draw(guiGraphics, x, y, PREVIEW_BOX_SIZE, PREVIEW_BOX_SIZE);

    if (this.previewNPC != null) {
      EntityConfigScreenRenderer.renderEntity(
          guiGraphics,
          this.previewNPC,
          EntityRenderConfig.guiScaled(x + 12, y + PREVIEW_BOX_SIZE - 12, 10, 0.0f, 0.0f),
          mouseX,
          mouseY);
    }
  }

  private void renderPresetInfo(GuiGraphics guiGraphics, int left, int top) {
    guiGraphics.pose().pushPose();
    guiGraphics.pose().scale(TEXT_SCALE, TEXT_SCALE, 1.0f);

    int scaledX = (int) ((left + 26) / TEXT_SCALE);
    int scaledY = (int) ((top + 2) / TEXT_SCALE);
    int lineHeight = (int) (10 / TEXT_SCALE);

    Text.drawString(
        guiGraphics,
        this.screen.getFont(),
        LocalPresetDataFiles.getPresetDisplayName(this.preset, this.metadata),
        scaledX,
        scaledY,
        0x000000);

    Text.drawString(
        guiGraphics,
        this.screen.getFont(),
        this.presetType.name() + " - " + this.metadata.category(),
        scaledX,
        scaledY + lineHeight,
        0x3F3F3F);

    Text.drawString(
        guiGraphics,
        this.screen.getFont(),
        this.metadata.version(),
        scaledX,
        scaledY + lineHeight * 2,
        0x3F3F3F);

    guiGraphics.pose().popPose();
  }

  @Override
  public Component getNarration() {
    return Component.literal(LocalPresetDataFiles.getPresetDisplayName(this.preset, this.metadata));
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    this.screen.selectEntry(this);
    return true;
  }
}
