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

package de.markusbordihn.easynpc.configui.client.screen.configuration.skin;

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.render.EntityRenderOverrides;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

public class AdvancedSkinConfigurationScreen<T extends ConfigurationMenu>
    extends SkinConfigurationScreen<T> {

  private static final int PREVIEW_SCALING = 45;
  protected Checkbox disableSkinCheckbox;
  private Button applyTextureLocationButton = null;
  private Button clearTextureLocationButton = null;
  private EditBox textureLocationBox;
  private ResourceLocation previewTexture;
  private String errorMessage = "";

  public AdvancedSkinConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private static SkinDataEntry restoreSkinDataEntry(
      SkinDataEntry formerSkinDataEntry, VariantDataCapable<?> variantData) {
    return switch (formerSkinDataEntry.type()) {
      case CUSTOM ->
          SkinDataEntry.createCustomSkin(
              formerSkinDataEntry.uuid(), formerSkinDataEntry.disableLayers());
      case RESOURCE_LOCATION ->
          SkinDataEntry.createResourceLocationSkin(formerSkinDataEntry.texture());
      case DEFAULT -> SkinDataEntry.createDefaultSkin(variantData.getSkinVariantType().name());
      default -> SkinDataEntry.createDefaultSkin(variantData.getDefaultSkinVariantType().name());
    };
  }

  private void applyTextureLocation() {
    ResourceLocation textureLocation =
        ResourceLocation.tryParse(this.textureLocationBox.getValue());
    if (textureLocation == null) {
      return;
    }

    NetworkMessageHandlerManager.getServerHandler()
        .setSkin(this.getEasyNPCUUID(), SkinDataEntry.createResourceLocationSkin(textureLocation));
  }

  private void validateTextureLocation() {
    String textureLocationValue = this.textureLocationBox.getValue();
    this.previewTexture = null;
    this.errorMessage = "";
    this.clearTextureLocationButton.active = !textureLocationValue.isEmpty();

    if (textureLocationValue.isEmpty()) {
      this.applyTextureLocationButton.active = false;
      return;
    }

    ResourceLocation textureLocation = ResourceLocation.tryParse(textureLocationValue);
    if (textureLocation == null) {
      this.applyTextureLocationButton.active = false;
      this.errorMessage = "invalid_resource_location";
      return;
    }

    this.applyTextureLocationButton.active = true;
    this.previewTexture = textureLocation;

    if (this.minecraft != null
        && this.minecraft.getResourceManager().getResource(textureLocation).isEmpty()) {
      this.errorMessage = "missing_resource_location";
    }
  }

  private void renderPreview(GuiGraphics guiGraphics) {
    if (this.previewTexture == null || this.getEasyNPC() == null) {
      return;
    }

    EntityConfigScreenRenderer.renderEntity(
        guiGraphics,
        this.getEasyNPC(),
        EntityRenderConfig.withOverrides(
            this.contentLeftPos + 240,
            this.contentTopPos + 185,
            PREVIEW_SCALING,
            EntityRenderOverrides.withTexture(this.previewTexture)),
        this.xMouse,
        this.yMouse);
  }

  @Override
  public void init() {
    super.init();

    this.advancedSkinButton.active = false;

    setDescriptionText("advanced_skin.text");

    SkinDataCapable<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    VariantDataCapable<?> variantData = this.getEasyNPC().getEasyNPCVariantData();
    SkinDataEntry formerSkinDataEntry = skinData.getSkinDataEntry();

    this.disableSkinCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 5,
                this.contentTopPos + 35,
                "disable_skin_checkbox",
                formerSkinDataEntry.type() == SkinType.NONE,
                checkbox ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .setSkin(
                            this.getEasyNPCUUID(),
                            checkbox.selected()
                                ? SkinDataEntry.createNoneSkin()
                                : restoreSkinDataEntry(formerSkinDataEntry, variantData))));

    this.textureLocationBox =
        new TextField(this.font, this.contentLeftPos, this.contentTopPos + 70, 180);
    this.textureLocationBox.setMaxLength(255);
    this.textureLocationBox.setResponder(consumer -> this.validateTextureLocation());
    this.addRenderableWidget(this.textureLocationBox);

    this.applyTextureLocationButton =
        this.addRenderableWidget(
            new TextButton(
                this.textureLocationBox.getX() + this.textureLocationBox.getWidth() + 2,
                this.contentTopPos + 70,
                60,
                "add",
                onPress -> this.applyTextureLocation()));
    this.applyTextureLocationButton.active = false;

    this.clearTextureLocationButton =
        this.addRenderableWidget(
            new TextButton(
                this.applyTextureLocationButton.getX()
                    + this.applyTextureLocationButton.getWidth()
                    + 1,
                this.contentTopPos + 70,
                55,
                "clear",
                onPress -> this.textureLocationBox.setValue("")));
    this.clearTextureLocationButton.active = false;

    ResourceLocation textureLocation = formerSkinDataEntry.texture();
    this.textureLocationBox.setValue(textureLocation != null ? textureLocation.toString() : "");
  }

  @Override
  protected void renderSkinSelectionBackground(GuiGraphics guiGraphics) {}

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    renderDescriptionText(guiGraphics, this.contentLeftPos + 5, this.contentTopPos + 5);

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "use_a_resource_location",
        this.contentLeftPos,
        this.contentTopPos + 60);

    if (!this.errorMessage.isEmpty()) {
      Text.drawErrorMessage(
          guiGraphics,
          this.font,
          TextComponent.getTranslatedText(this.errorMessage),
          this.contentLeftPos,
          this.contentTopPos + 93,
          190);
    } else {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "resource_location_preview",
          this.contentLeftPos,
          this.contentTopPos + 93,
          Constants.FONT_COLOR_DARK_GREEN);
    }

    this.renderPreview(guiGraphics);
  }
}
