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

package de.markusbordihn.easynpc.client.screen.configuration.skin;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.client.screen.components.SkinSelectionButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.client.texture.TextureManager;
import de.markusbordihn.easynpc.client.texture.TextureModelKey;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.message.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import de.markusbordihn.easynpc.utils.TextUtils;
import de.markusbordihn.easynpc.validator.UrlValidator;
import java.util.ArrayList;
import java.util.Set;
import java.util.UUID;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class UrlSkinConfigurationScreen<T extends ConfigurationMenu>
    extends SkinConfigurationScreen<T> {

  private static final float SKIN_NAME_SCALING = 0.7f;
  private static final int ADD_SKIN_DELAY = 20;
  protected Button addTextureSettingsButton = null;
  protected EditBox textureSkinLocationBox;
  private boolean canTextureSkinLocationChange = true;
  private Button clearTextureSettingsButton = null;
  private String formerTextureSkinLocation = "";

  public UrlSkinConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private static void updateNextTextureSkinLocationChange() {
    UrlSkinConfigurationScreen.nextTextureSkinLocationChange =
        (int) java.time.Instant.now().getEpochSecond() + ADD_SKIN_DELAY;
  }

  private void renderSkins(GuiGraphics guiGraphics) {
    if (this.getEasyNPC() == null) {
      return;
    }

    int positionTop = 144;
    int skinPosition = 0;
    skinButtons = new ArrayList<>();
    SkinData<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    SkinModel skinModel = skinData.getSkinModel();
    Set<UUID> textures = RemoteTextureManager.getTextureCacheKeys(skinModel);
    this.numOfSkins = textures.size();
    Object[] textureKeys = textures.toArray();

    // Check Skin buttons state, if number of skins changed.
    if (this.lastNumOfSkins != this.numOfSkins) {
      checkSkinNavigationButtonState();
      this.lastNumOfSkins = this.numOfSkins;
    }

    for (int i = skinStartIndex; i < this.numOfSkins && i < skinStartIndex + maxSkinsPerPage; i++) {
      int left = this.leftPos + 32 + (skinPosition * SKIN_PREVIEW_WIDTH);
      int top = this.topPos + 65 + positionTop;

      // Render Skins
      UUID textureKey = (UUID) textureKeys[i];
      this.renderSkinEntity(guiGraphics, left, top, skinModel, textureKey);

      // Render skin name
      int topNamePos = Math.round((top - 76) / SKIN_NAME_SCALING);
      int leftNamePos = Math.round((left - 21) / SKIN_NAME_SCALING);
      guiGraphics.pose().pushPose();
      guiGraphics.pose().translate(0, 0, 100);
      guiGraphics.pose().scale(SKIN_NAME_SCALING, SKIN_NAME_SCALING, SKIN_NAME_SCALING);
      String variantName = TextUtils.normalizeString(textureKey.toString(), 11);
      Text.drawString(
          guiGraphics,
          this.font,
          variantName,
          leftNamePos,
          topNamePos,
          Constants.FONT_COLOR_DARK_GREEN);
      guiGraphics.pose().popPose();

      skinPosition++;
    }
  }

  private void renderSkinEntity(
      GuiGraphics guiGraphics, int x, int y, SkinModel skinModel, UUID textureUUID) {
    // Skin details
    TextureModelKey textureModelKey = new TextureModelKey(textureUUID, skinModel);
    SkinType skinType = RemoteTextureManager.getTextureSkinType(textureModelKey);

    // Create dynamically button for each skin variant and profession.
    Button skinButton =
        new SkinSelectionButton(
            x - 24,
            y - 81,
            button -> {
              String skinURL = RemoteTextureManager.getTextureSkinURL(textureModelKey);
              NetworkMessageHandlerManager.getServerHandler()
                  .setSkin(this.getNpcUUID(), "", skinURL, textureUUID, skinType, "");
            });

    // Disable button for active skin.
    SkinData<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    UUID skinUUID = skinData.getSkinUUID();
    skinButton.active = !skinUUID.equals(textureUUID);

    // Render skin entity with variant and profession.
    ScreenHelper.renderEntityPlayerSkin(
        guiGraphics,
        x + 4,
        y,
        x - this.xMouse,
        y - 40 - this.yMouse,
        this.getEasyNPC(),
        textureUUID,
        skinType);

    skinButtons.add(skinButton);
  }

  private void clearTextureSkinLocation() {
    if (!this.textureSkinLocationBox.getValue().isEmpty()) {
      this.textureSkinLocationBox.setValue("");
    }
  }

  private void addTextureSkinLocation() {
    String textureSkinLocationValue = this.textureSkinLocationBox.getValue();
    if (!textureSkinLocationValue.equals(this.formerTextureSkinLocation)
        && (textureSkinLocationValue.isEmpty()
            || UrlValidator.isValidUrl(textureSkinLocationValue))) {

      // Validate url and send message to server.
      if (UrlValidator.isValidUrl(textureSkinLocationValue)) {
        log.debug("Setting remote user texture to {}", textureSkinLocationValue);
        TextureManager.clearLastErrorMessage();
        NetworkMessageHandlerManager.getServerHandler()
            .setRemoteSkin(this.getNpcUUID(), textureSkinLocationValue);
      }

      this.addTextureSettingsButton.active = false;
      this.formerTextureSkinLocation = textureSkinLocationValue;
      updateNextTextureSkinLocationChange();
    }
  }

  private void validateTextureSkinLocation() {
    String textureSkinLocationValue = this.textureSkinLocationBox.getValue();

    // Additional check to make sure that the server is not spammed with requests.
    if (!this.canTextureSkinLocationChange) {
      this.addTextureSettingsButton.active = false;
      this.clearTextureSettingsButton.active = true;
      return;
    }

    // Validate url
    this.addTextureSettingsButton.active =
        !textureSkinLocationValue.isEmpty() && UrlValidator.isValidUrl(textureSkinLocationValue);

    // Clear button
    this.clearTextureSettingsButton.active = !textureSkinLocationValue.isEmpty();
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.urlSkinButton.active = false;

    // Description text
    setDescriptionText("url_skin.text");

    // Entity specific information.
    SkinData<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    SkinModel skinModel = skinData.getSkinModel();
    this.numOfSkins = RemoteTextureManager.getTextureCacheKeys(skinModel).size();

    // Texture Skin Location
    this.textureSkinLocationBox =
        new TextField(this.font, this.contentLeftPos, this.contentTopPos + 50, 180);
    this.textureSkinLocationBox.setMaxLength(255);
    this.textureSkinLocationBox.setValue("");
    this.textureSkinLocationBox.setResponder(consumer -> this.validateTextureSkinLocation());
    this.addRenderableWidget(this.textureSkinLocationBox);

    // Add Button
    this.addTextureSettingsButton =
        this.addRenderableWidget(
            new TextButton(
                this.textureSkinLocationBox.getX() + this.textureSkinLocationBox.getWidth() + 2,
                this.contentTopPos + 50,
                65,
                "add",
                onPress -> this.addTextureSkinLocation()));
    this.addTextureSettingsButton.active = false;

    // Clear Texture Buttons
    this.clearTextureSettingsButton =
        this.addRenderableWidget(
            new TextButton(
                this.addTextureSettingsButton.getX() + this.addTextureSettingsButton.getWidth() + 1,
                this.contentTopPos + 50,
                55,
                "clear",
                onPress -> this.clearTextureSkinLocation()));
    this.clearTextureSettingsButton.active = false;

    // Skin Navigation Buttons
    defineSkinNavigationButtons();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Description text
    renderDescriptionText(guiGraphics, this.contentLeftPos + 5, this.contentTopPos + 5);

    if (addTextureSettingsButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "use_a_skin_url",
          this.contentLeftPos,
          addTextureSettingsButton.getY() - 10);
    }

    // Reload protection
    this.canTextureSkinLocationChange =
        java.time.Instant.now().getEpochSecond()
            >= UrlSkinConfigurationScreen.nextTextureSkinLocationChange;

    // Render Status Symbol and text, if needed.
    if (!this.canTextureSkinLocationChange) {
      guiGraphics.pose().translate(0, 0, 100);
      Graphics.blit(
          guiGraphics,
          Constants.TEXTURE_CONFIGURATION,
          this.leftPos + 176,
          this.contentTopPos + 53,
          82,
          1,
          8,
          10);

      if (!TextureManager.hasLastErrorMessage()) {
        Text.drawConfigString(
            guiGraphics,
            this.font,
            "processing_url_skin",
            this.leftPos + 55,
            this.contentTopPos + 80);
      }
    }

    // Show error messages, if any.
    if (TextureManager.hasLastErrorMessage()) {
      Text.drawErrorMessage(
          guiGraphics,
          this.font,
          TextureManager.getLastErrorMessage(),
          this.leftPos + 10,
          this.contentTopPos + 71,
          this.imageWidth - 14);
    }

    // Skins
    this.renderSkins(guiGraphics);
  }
}
