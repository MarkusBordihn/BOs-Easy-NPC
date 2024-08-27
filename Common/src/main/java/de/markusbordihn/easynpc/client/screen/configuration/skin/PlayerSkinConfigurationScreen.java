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

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.client.screen.components.SkinSelectionButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.client.texture.TextureManager;
import de.markusbordihn.easynpc.client.texture.TextureModelKey;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import de.markusbordihn.easynpc.utils.PlayersUtils;
import de.markusbordihn.easynpc.utils.TextUtils;
import de.markusbordihn.easynpc.validator.NameValidator;
import java.util.ArrayList;
import java.util.Set;
import java.util.UUID;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.player.Inventory;

public class PlayerSkinConfigurationScreen<T extends ConfigurationMenu>
    extends SkinConfigurationScreen<T> {

  private static final float SKIN_NAME_SCALING = 0.7f;
  private static final int ADD_SKIN_DELAY = 20;
  protected Button addTextureSettingsButton = null;
  protected EditBox textureSkinLocationBox;
  private boolean canTextureSkinLocationChange = true;
  private Button clearTextureSettingsButton = null;
  private String formerTextureSkinLocation = "";
  private String errorMessage = "";

  public PlayerSkinConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private static void updateNextTextureSkinLocationChange() {
    PlayerSkinConfigurationScreen.nextTextureSkinLocationChange =
        (int) java.time.Instant.now().getEpochSecond() + ADD_SKIN_DELAY;
  }

  private void renderSkins(PoseStack poseStack) {
    if (this.getEasyNPC() == null) {
      return;
    }

    int positionTop = 144;
    int skinPosition = 0;
    skinButtons = new ArrayList<>();
    SkinData<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    SkinModel skinModel = skinData.getSkinModel();
    Set<UUID> textures = PlayerTextureManager.getTextureCacheKeys(skinModel);
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
      this.renderSkinEntity(left, top, skinModel, textureKey);

      // Render skin name
      int topNamePos = Math.round((top - 76) / SKIN_NAME_SCALING);
      int leftNamePos = Math.round((left - 21) / SKIN_NAME_SCALING);
      poseStack.pushPose();
      poseStack.translate(0, 0, 100);
      poseStack.scale(SKIN_NAME_SCALING, SKIN_NAME_SCALING, SKIN_NAME_SCALING);
      String variantName = TextUtils.normalizeString(textureKey.toString(), 11);
      Text.drawString(
          poseStack,
          this.font,
          variantName,
          leftNamePos,
          topNamePos,
          Constants.FONT_COLOR_DARK_GREEN);
      poseStack.popPose();

      skinPosition++;
    }
  }

  private void renderSkinEntity(int x, int y, SkinModel skinModel, UUID textureUUID) {
    // Skin details
    TextureModelKey textureModelKey = new TextureModelKey(textureUUID, skinModel);
    SkinType skinType = PlayerTextureManager.getTextureSkinType(textureModelKey);

    // Create dynamically button for each skin variant and profession.
    Button skinButton =
        new SkinSelectionButton(
            x - 24,
            y - 81,
            button ->
                NetworkMessageHandlerManager.getServerHandler()
                    .setPlayerSkin(this.getEasyNPCUUID(), "", textureUUID));

    // Disable button for active skin.
    SkinData<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    UUID skinUUID = skinData.getSkinUUID();
    skinButton.active = !skinUUID.equals(textureUUID);

    // Render skin entity with variant and profession.
    ScreenHelper.renderEntityCustomSkin(
        x + 4, y, x - this.xMouse, y - 40 - this.yMouse, this.getEasyNPC(), textureUUID, skinType);

    skinButtons.add(skinButton);
  }

  private void clearTextureSkinLocation() {
    if (!this.textureSkinLocationBox.getValue().isEmpty()) {
      this.textureSkinLocationBox.setValue("");
    }
  }

  private void addTextureSkinLocation() {
    String textureSkinLocationValue = this.textureSkinLocationBox.getValue();
    if (!textureSkinLocationValue.isEmpty()
        && !textureSkinLocationValue.equals(this.formerTextureSkinLocation)) {

      // Validate player name
      if (!NameValidator.isValidPlayerName(textureSkinLocationValue)) {
        this.errorMessage = "invalid_player_name";
        return;
      }

      // Validate player UUID
      UUID playerUUID = PlayersUtils.getUserUUID(textureSkinLocationValue);
      if (playerUUID == null) {
        this.errorMessage = "invalid_player_uuid";
        return;
      }

      // Send texture skin location to server.
      log.debug("Setting player texture to {} with UUID {}", textureSkinLocationValue, playerUUID);
      TextureManager.clearLastErrorMessage();
      this.errorMessage = "";
      NetworkMessageHandlerManager.getServerHandler()
          .setPlayerSkin(this.getEasyNPCUUID(), textureSkinLocationValue, playerUUID);

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

    // Validate player name.
    this.addTextureSettingsButton.active =
        !textureSkinLocationValue.isEmpty()
            && NameValidator.isValidPlayerName(textureSkinLocationValue);

    // Clear button
    this.clearTextureSettingsButton.active = !textureSkinLocationValue.isEmpty();
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.playerSkinButton.active = false;

    // Description text
    setDescriptionText("player_skin.text");

    // Entity specific information.
    SkinData<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    SkinModel skinModel = skinData.getSkinModel();
    this.numOfSkins = PlayerTextureManager.getTextureCacheKeys(skinModel).size();

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
                this.textureSkinLocationBox.x + this.textureSkinLocationBox.getWidth() + 2,
                this.contentTopPos + 50,
                65,
                "add",
                onPress -> this.addTextureSkinLocation()));
    this.addTextureSettingsButton.active = false;

    // Clear Texture Buttons
    this.clearTextureSettingsButton =
        this.addRenderableWidget(
            new TextButton(
                this.addTextureSettingsButton.x + this.addTextureSettingsButton.getWidth() + 1,
                this.contentTopPos + 50,
                55,
                "clear",
                onPress -> this.clearTextureSkinLocation()));
    this.clearTextureSettingsButton.active = false;

    // Skin Navigation Buttons
    defineSkinNavigationButtons();
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Description text
    renderDescriptionText(poseStack, this.contentLeftPos + 5, this.contentTopPos + 5);

    if (addTextureSettingsButton != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "use_a_player_name",
          this.contentLeftPos,
          addTextureSettingsButton.y - 10);
    }

    // Reload protection
    this.canTextureSkinLocationChange =
        java.time.Instant.now().getEpochSecond()
            >= PlayerSkinConfigurationScreen.nextTextureSkinLocationChange;

    // Render Status Symbol and text, if needed.
    if (!this.canTextureSkinLocationChange) {
      poseStack.translate(0, 0, 100);
      Graphics.blit(
          poseStack,
          Constants.TEXTURE_CONFIGURATION,
          this.leftPos + 176,
          this.contentTopPos + 53,
          82,
          1,
          8,
          10);

      if (!TextureManager.hasLastErrorMessage() && this.errorMessage.isEmpty()) {
        Text.drawConfigString(
            poseStack,
            this.font,
            "processing_url_skin",
            this.leftPos + 55,
            this.contentTopPos + 80);
      }
    }

    // Show error messages, if any.
    if (this.errorMessage != null && !this.errorMessage.isEmpty()) {
      Text.drawErrorMessage(
          poseStack,
          this.font,
          new TranslatableComponent(Constants.TEXT_PREFIX + this.errorMessage),
          this.leftPos + 10,
          this.contentTopPos + 71,
          this.imageWidth - 14);
    } else if (TextureManager.hasLastErrorMessage()) {
      Text.drawErrorMessage(
          poseStack,
          this.font,
          TextureManager.getLastErrorMessage(),
          this.leftPos + 10,
          this.contentTopPos + 71,
          this.imageWidth - 14);
    }

    // Skins
    this.renderSkins(poseStack);
  }
}
