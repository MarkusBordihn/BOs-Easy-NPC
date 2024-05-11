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

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
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
import de.markusbordihn.easynpc.menu.configuration.skin.PlayerSkinConfigurationMenu;
import de.markusbordihn.easynpc.network.ServerNetworkMessageHandler;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import de.markusbordihn.easynpc.utils.TextUtils;
import de.markusbordihn.easynpc.validator.NameValidator;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

public class PlayerSkinConfigurationScreen
    extends SkinConfigurationScreen<PlayerSkinConfigurationMenu> {

  private static final float SKIN_NAME_SCALING = 0.7f;
  private static final int ADD_SKIN_DELAY = 20;
  protected static int nextTextureSkinLocationChange =
      (int) java.time.Instant.now().getEpochSecond();
  protected final SkinData<?> skinData;
  protected Button addTextureSettingsButton = null;
  protected EditBox textureSkinLocationBox;
  protected int lastNumOfSkins = 0;
  private Button clearTextureSettingsButton = null;
  private String formerTextureSkinLocation = "";
  private boolean canTextureSkinLocationChange = true;

  public PlayerSkinConfigurationScreen(
      PlayerSkinConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.skinData = this.easyNPC.getEasyNPCSkinData();
  }

  private static void updateNextTextureSkinLocationChange() {
    PlayerSkinConfigurationScreen.nextTextureSkinLocationChange =
        (int) java.time.Instant.now().getEpochSecond() + ADD_SKIN_DELAY;
  }

  private void renderSkins(PoseStack poseStack) {
    if (this.easyNPC == null) {
      return;
    }

    int positionTop = 119;
    int skinPosition = 0;
    skinButtons = new ArrayList<>();
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
      this.renderSkinEntity(poseStack, left, top, skinModel, textureKey);

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

  private void renderSkinEntity(
      PoseStack poseStack, int x, int y, SkinModel skinModel, UUID textureUUID) {
    // Skin details
    TextureModelKey textureModelKey = new TextureModelKey(textureUUID, skinModel);
    SkinType skinType = PlayerTextureManager.getTextureSkinType(textureModelKey);

    // Create dynamically button for each skin variant and profession.
    Button skinButton =
        new SkinSelectionButton(
            x - 24,
            y - 81,
            button -> ServerNetworkMessageHandler.setPlayerSkin(this.uuid, "", textureUUID));

    // Disable button for active skin.
    UUID skinUUID = this.skinData.getSkinUUID();
    skinButton.active = !skinUUID.equals(textureUUID);

    // Render skin entity with variant and profession.
    ScreenHelper.renderEntityPlayerSkin(
        x + 4, y, x - this.xMouse, y - 40 - this.yMouse, this.easyNPC, textureUUID, skinType);

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
            || NameValidator.isValidPlayerName(textureSkinLocationValue))) {

      // Validate player name and send skin change request to server.
      if (NameValidator.isValidPlayerName(textureSkinLocationValue)) {
        log.debug("Setting player user texture to {}", textureSkinLocationValue);
        TextureManager.clearLastErrorMessage();
        ServerNetworkMessageHandler.setPlayerSkin(
            this.uuid, textureSkinLocationValue, Constants.BLANK_UUID);
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

    // Clear former error messages, if any.
    TextureManager.clearLastErrorMessage();

    // Entity specific information.
    this.numOfSkins = PlayerTextureManager.getTextureCacheKeys(skinModel).size();

    // Texture Skin Location
    this.textureSkinLocationBox =
        new TextField(this.font, this.contentLeftPos, this.topPos + 50, 180);
    this.textureSkinLocationBox.setMaxLength(255);
    this.textureSkinLocationBox.setValue("");
    this.textureSkinLocationBox.setResponder(consumer -> this.validateTextureSkinLocation());
    this.addRenderableWidget(this.textureSkinLocationBox);

    // Add Button
    this.addTextureSettingsButton =
        this.addRenderableWidget(
            new TextButton(
                this.textureSkinLocationBox.x + this.textureSkinLocationBox.getWidth() + 2,
                this.topPos + 50,
                65,
                "add",
                onPress -> this.addTextureSkinLocation()));
    this.addTextureSettingsButton.active = false;

    // Clear Texture Buttons
    this.clearTextureSettingsButton =
        this.addRenderableWidget(
            new TextButton(
                this.addTextureSettingsButton.x + this.addTextureSettingsButton.getWidth() + 1,
                this.topPos + 50,
                55,
                "clear",
                onPress -> this.clearTextureSkinLocation()));
    this.clearTextureSettingsButton.active = false;

    // Skin Navigation Buttons
    int skinButtonTop = this.topPos + 187;
    int skinButtonLeft = this.contentLeftPos;
    int skinButtonRight = this.rightPos - 29;
    this.skinPreviousPageButton =
        this.addRenderableWidget(
            new TextButton(
                skinButtonLeft,
                skinButtonTop,
                20,
                new TextComponent("<<"),
                onPress -> {
                  skinStartIndex = Math.max(this.skinStartIndex - maxSkinsPerPage, 0);
                  checkSkinNavigationButtonState();
                }));
    this.skinPreviousButton =
        this.addRenderableWidget(
            new TextButton(
                skinButtonLeft + 20,
                skinButtonTop,
                20,
                new TextComponent("<"),
                onPress -> {
                  if (this.skinStartIndex > 0) {
                    skinStartIndex--;
                  }
                  checkSkinNavigationButtonState();
                }));
    this.skinNextPageButton =
        this.addRenderableWidget(
            new TextButton(
                skinButtonRight,
                skinButtonTop,
                20,
                new TextComponent(">>"),
                onPress -> {
                  if (this.skinStartIndex >= 0
                      && this.skinStartIndex + this.maxSkinsPerPage < this.numOfSkins) {
                    this.skinStartIndex = this.skinStartIndex + this.maxSkinsPerPage;
                  } else if (this.numOfSkins > this.maxSkinsPerPage) {
                    this.skinStartIndex = this.numOfSkins - this.maxSkinsPerPage;
                  } else {
                    this.skinStartIndex = this.numOfSkins;
                  }
                  checkSkinNavigationButtonState();
                }));
    this.skinNextButton =
        this.addRenderableWidget(
            new TextButton(
                skinButtonRight - 20,
                skinButtonTop,
                20,
                new TextComponent(">"),
                onPress -> {
                  if (this.skinStartIndex >= 0
                      && this.skinStartIndex < this.numOfSkins - this.maxSkinsPerPage) {
                    skinStartIndex++;
                  }
                  checkSkinNavigationButtonState();
                }));
    checkSkinNavigationButtonState();
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

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
      RenderSystem.setShader(GameRenderer::getPositionTexShader);
      RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
      RenderSystem.setShaderTexture(0, Constants.TEXTURE_CONFIGURATION);
      poseStack.translate(0, 0, 100);
      this.blit(poseStack, this.leftPos + 176, this.topPos + 53, 82, 1, 8, 10);

      if (!TextureManager.hasLastErrorMessage()) {
        Text.drawConfigString(
            poseStack, this.font, "processing_url_skin", this.leftPos + 55, this.topPos + 80);
      }
    }

    // Show error messages, if any.
    if (TextureManager.hasLastErrorMessage()) {
      List<FormattedCharSequence> textComponents =
          this.font.split(
              new TextComponent(TextureManager.getLastErrorMessage()), this.imageWidth - 14);
      int line = 0;
      for (FormattedCharSequence formattedCharSequence : textComponents) {
        Text.drawString(
            poseStack,
            this.font,
            formattedCharSequence,
            this.leftPos + 10,
            this.topPos + 71 + (line++ * (this.font.lineHeight + 2)),
            Constants.FONT_COLOR_RED);
      }
    }

    // Make sure we pass the mouse movements to the dynamically added buttons, if any.
    if (!skinButtons.isEmpty()) {
      for (Button skinButton : skinButtons) {
        skinButton.render(poseStack, x, y, partialTicks);
      }
    }

    // Skins
    this.renderSkins(poseStack);
  }
}
