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

import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.client.texture.TextureManager;
import de.markusbordihn.easynpc.client.texture.TextureModelKey;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.SkinSelectionButton;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.render.EntityRenderOverrides;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Set;
import java.util.UUID;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public abstract class TextureLocationSkinConfigurationScreen<T extends ConfigurationMenu>
    extends SkinConfigurationScreen<T> {

  private static final float SKIN_NAME_SCALING = 0.7f;
  private static final int ADD_SKIN_DELAY = 20;
  private final String textureSkinLocationLabel;
  private final int disableLayersCheckboxTop;
  private final int errorMessageTop;
  protected Button addTextureSettingsButton = null;
  protected EditBox textureSkinLocationBox;
  protected String formerTextureSkinLocation = "";
  protected String errorMessage = "";
  private boolean canTextureSkinLocationChange = true;
  private Button clearTextureSettingsButton = null;

  protected TextureLocationSkinConfigurationScreen(
      T menu,
      Inventory inventory,
      Component component,
      String textureSkinLocationLabel,
      int disableLayersCheckboxTop,
      int errorMessageTop) {
    super(menu, inventory, component);
    this.textureSkinLocationLabel = textureSkinLocationLabel;
    this.disableLayersCheckboxTop = disableLayersCheckboxTop;
    this.errorMessageTop = errorMessageTop;
  }

  protected static void updateNextTextureSkinLocationChange() {
    SkinConfigurationScreen.nextTextureSkinLocationChange =
        (int) Instant.now().getEpochSecond() + ADD_SKIN_DELAY;
  }

  protected abstract Set<UUID> getTextureCacheKeys(SkinModel skinModel);

  protected abstract SkinType getTextureSkinType(TextureModelKey textureModelKey);

  protected abstract SkinDataEntry createSkinDataEntry(TextureModelKey textureModelKey);

  protected abstract boolean isValidTextureSkinLocation(String textureSkinLocation);

  protected abstract void addTextureSkinLocation();

  protected void defineAdditionalSkinButtons(Button skinButton, TextureModelKey textureModelKey) {}

  private void renderSkins(GuiGraphics guiGraphics, int mouseX, int mouseY) {
    if (this.getEasyNPC() == null) {
      return;
    }

    int positionTop = 144;
    int skinPosition = 0;
    this.skinButtons = new ArrayList<>();
    SkinModel skinModel = this.getEasyNPC().getEasyNPCSkinData().getSkinModel();
    UUID[] textureKeys = this.getTextureCacheKeys(skinModel).toArray(UUID[]::new);
    this.numOfSkins = textureKeys.length;

    if (this.lastNumOfSkins != this.numOfSkins) {
      this.checkSkinNavigationButtonState();
      this.lastNumOfSkins = this.numOfSkins;
    }

    for (int i = this.skinStartIndex;
        i < this.numOfSkins && i < this.skinStartIndex + this.maxSkinsPerPage;
        i++) {
      int left = this.leftPos + 32 + (skinPosition * SKIN_PREVIEW_WIDTH);
      int top = this.topPos + 65 + positionTop;

      UUID textureKey = textureKeys[i];
      this.renderSkinEntity(guiGraphics, left, top, skinModel, textureKey);

      int topNamePos = Math.round((top - 76) / SKIN_NAME_SCALING);
      int leftNamePos = Math.round((left - 21) / SKIN_NAME_SCALING);
      int scaledMouseX = Math.round(mouseX / SKIN_NAME_SCALING);
      int scaledMouseY = Math.round(mouseY / SKIN_NAME_SCALING);
      guiGraphics.pose().pushMatrix();
      guiGraphics.pose().translate(0, 100);
      guiGraphics.pose().scale(SKIN_NAME_SCALING, SKIN_NAME_SCALING);
      Text.drawLimitedHoverString(
          guiGraphics,
          this.font,
          TextUtils.normalizeString(textureKey.toString()),
          leftNamePos,
          topNamePos,
          Constants.FONT_COLOR_DARK_GREEN,
          11,
          scaledMouseX,
          scaledMouseY);
      guiGraphics.pose().popMatrix();

      skinPosition++;
    }
  }

  private void renderSkinEntity(
      GuiGraphics guiGraphics, int x, int y, SkinModel skinModel, UUID textureUUID) {
    TextureModelKey textureModelKey = new TextureModelKey(textureUUID, skinModel);
    Button skinButton =
        new SkinSelectionButton(
            x - 24,
            y - 81,
            button ->
                NetworkMessageHandlerManager.getServerHandler()
                    .setSkin(this.getEasyNPCUUID(), this.createSkinDataEntry(textureModelKey)));
    skinButton.active = !this.getEasyNPC().getEasyNPCSkinData().getSkinUUID().equals(textureUUID);

    EntityConfigScreenRenderer.renderEntity(
        guiGraphics,
        this.getEasyNPC(),
        EntityRenderConfig.withOverrides(
            x + 4,
            y - 35,
            30,
            EntityRenderOverrides.withSkin(this.getTextureSkinType(textureModelKey), textureUUID)),
        this.xMouse,
        this.yMouse);

    this.skinButtons.add(skinButton);
    this.defineAdditionalSkinButtons(skinButton, textureModelKey);
  }

  private void clearTextureSkinLocation() {
    if (!this.textureSkinLocationBox.getValue().isEmpty()) {
      this.textureSkinLocationBox.setValue("");
    }
  }

  private void validateTextureSkinLocation() {
    String textureSkinLocationValue = this.textureSkinLocationBox.getValue();
    if (!this.canTextureSkinLocationChange) {
      this.addTextureSettingsButton.active = false;
      this.clearTextureSettingsButton.active = true;
      return;
    }

    this.addTextureSettingsButton.active =
        !textureSkinLocationValue.isEmpty()
            && this.isValidTextureSkinLocation(textureSkinLocationValue);
    this.clearTextureSettingsButton.active = !textureSkinLocationValue.isEmpty();
  }

  @Override
  public void init() {
    super.init();

    SkinDataCapable<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    this.numOfSkins = this.getTextureCacheKeys(skinData.getSkinModel()).size();

    this.textureSkinLocationBox =
        new TextField(this.font, this.contentLeftPos, this.contentTopPos + 50, 180);
    this.textureSkinLocationBox.setMaxLength(255);
    this.textureSkinLocationBox.setValue("");
    this.textureSkinLocationBox.setResponder(consumer -> this.validateTextureSkinLocation());
    this.addRenderableWidget(this.textureSkinLocationBox);

    this.addTextureSettingsButton =
        this.addRenderableWidget(
            new TextButton(
                this.textureSkinLocationBox.getX() + this.textureSkinLocationBox.getWidth() + 2,
                this.contentTopPos + 50,
                65,
                "add",
                onPress -> this.addTextureSkinLocation()));
    this.addTextureSettingsButton.active = false;

    this.clearTextureSettingsButton =
        this.addRenderableWidget(
            new TextButton(
                this.addTextureSettingsButton.getX() + this.addTextureSettingsButton.getWidth() + 1,
                this.contentTopPos + 50,
                55,
                "clear",
                onPress -> this.clearTextureSkinLocation()));
    this.clearTextureSettingsButton.active = false;

    this.addRenderableWidget(
        new Checkbox(
            this.contentLeftPos + 55,
            this.contentTopPos + this.disableLayersCheckboxTop,
            "disable_skin_layers",
            skinData.getSkinDataEntry().disableLayers(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .setSkin(
                        this.getEasyNPCUUID(),
                        skinData.getSkinDataEntry().withDisableLayers(checkbox.selected()))));

    this.defineProfessionButtons(this.contentLeftPos + 185, this.contentTopPos + 85);

    this.defineSkinNavigationButtons();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    this.renderDescriptionText(guiGraphics, this.contentLeftPos + 5, this.contentTopPos + 5);

    if (this.addTextureSettingsButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          this.textureSkinLocationLabel,
          this.contentLeftPos,
          this.addTextureSettingsButton.getY() - 10);
    }

    this.canTextureSkinLocationChange =
        Instant.now().getEpochSecond() >= SkinConfigurationScreen.nextTextureSkinLocationChange;

    if (!this.canTextureSkinLocationChange) {
      guiGraphics.pose().pushMatrix();
      guiGraphics.pose().translate(0, 100);
      Graphics.blit(
          guiGraphics,
          Constants.TEXTURE_CONFIGURATION,
          this.leftPos + 176,
          this.contentTopPos + 53,
          82,
          1,
          8,
          10);

      if (!TextureManager.hasLastErrorMessage() && this.errorMessage.isEmpty()) {
        Text.drawConfigString(
            guiGraphics,
            this.font,
            "processing_url_skin",
            this.leftPos + 55,
            this.contentTopPos + 73,
            Constants.FONT_COLOR_RED);
      }
      guiGraphics.pose().popMatrix();
    }

    if (!this.errorMessage.isEmpty()) {
      Text.drawErrorMessage(
          guiGraphics,
          this.font,
          TextComponent.getTranslatedText(this.errorMessage),
          this.leftPos + 10,
          this.contentTopPos + this.errorMessageTop,
          this.imageWidth - 14);
    } else if (TextureManager.hasLastErrorMessage()) {
      Text.drawErrorMessage(
          guiGraphics,
          this.font,
          TextureManager.getLastErrorMessage(),
          this.leftPos + 10,
          this.contentTopPos + this.errorMessageTop,
          this.imageWidth - 14);
    }

    this.renderSkins(guiGraphics, x, y);
  }
}
