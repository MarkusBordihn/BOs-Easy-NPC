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

package de.markusbordihn.easynpc.client.screen.configuration.skin;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import com.mojang.blaze3d.systems.RenderSystem;

import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.components.ImageButton;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.client.texture.TextureModelKey;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.menu.configuration.skin.PlayerSkinConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessage;
import de.markusbordihn.easynpc.utils.PlayersUtils;
import de.markusbordihn.easynpc.utils.TextUtils;

public class PlayerSkinConfigurationScreen
    extends SkinConfigurationScreen<PlayerSkinConfigurationMenu> {

  // Internal
  private Button clearTextureSettingsButton = null;
  private Button skinNextButton = null;
  private Button skinNextPageButton = null;
  private Button skinPreviousButton = null;
  private Button skinPreviousPageButton = null;
  private List<Button> skinButtons = new ArrayList<>();
  protected Button addTextureSettingsButton = null;
  protected EditBox textureSkinLocationBox;

  // Skin Preview
  private static final float SKIN_NAME_SCALING = 0.7f;
  private static final int ADD_SKIN_DELAY = 20;
  private int skinStartIndex = 0;
  private int maxSkinsPerPage = 5;

  // Cache
  private String formerTextureSkinLocation = "";
  private boolean canTextureSkinLocationChange = true;
  protected static int nextTextureSkinLocationChange =
      (int) java.time.Instant.now().getEpochSecond();
  protected int numOfSkins = 0;
  protected int lastNumOfSkins = 0;

  public PlayerSkinConfigurationScreen(PlayerSkinConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  private void renderSkins(GuiGraphics guiGraphics) {
    if (this.entity == null) {
      return;
    }

    int positionTop = 119;
    int skinPosition = 0;
    skinButtons = new ArrayList<>();
    Set<UUID> textures = PlayerTextureManager.getPlayerTextureCacheKeys(skinModel);
    this.numOfSkins = textures.size();
    Object[] textureKeys = textures.toArray();

    // Check Skin buttons state, if number of skins changed.
    if (this.lastNumOfSkins != this.numOfSkins) {
      checkSkinButtonState();
      this.lastNumOfSkins = this.numOfSkins;
    }

    for (int i = skinStartIndex; i < this.numOfSkins && i < skinStartIndex + maxSkinsPerPage; i++) {
      int left = this.leftPos + 32 + (skinPosition * skinPreviewWidth);
      int top = this.topPos + 65 + positionTop;

      // Render Skins
      UUID textureKey = (UUID) textureKeys[i];
      this.renderSkinEntity(guiGraphics, left, top, skinModel, textureKey);

      // Render skin name
      int topNamePos = Math.round((top - 76f) / SKIN_NAME_SCALING);
      int leftNamePos = Math.round((left - 21f) / SKIN_NAME_SCALING);
      guiGraphics.pose().pushPose();
      guiGraphics.pose().translate(0, 0, 100);
      guiGraphics.pose().scale(SKIN_NAME_SCALING, SKIN_NAME_SCALING, SKIN_NAME_SCALING);
      String variantName = TextUtils.normalizeString(textureKey.toString(), 11);
      guiGraphics.drawString(this.font, Component.literal(variantName), leftNamePos, topNamePos,
          Constants.FONT_COLOR_DARK_GREEN);
      guiGraphics.pose().popPose();

      skinPosition++;
    }
  }

  private void renderSkinEntity(GuiGraphics guiGraphics, int x, int y, SkinModel skinModel,
      UUID textureUUID) {
    // Skin details
    TextureModelKey textureModelKey = new TextureModelKey(textureUUID, skinModel);
    SkinType skinType = PlayerTextureManager.getPlayerTextureSkinType(textureModelKey);

    // Create dynamically button for each skin variant and profession.
    int skinButtonLeft = x - 24;
    int skinButtonTop = y - 81;
    int skinButtonHeight = 84;
    ImageButton skinButton = new ImageButton(skinButtonLeft, skinButtonTop, skinPreviewWidth,
        skinButtonHeight, 0, -84, 84, Constants.TEXTURE_CONFIGURATION, button -> {
          String skinURL = PlayerTextureManager.getPlayerTextureSkinURL(textureModelKey);
          NetworkMessage.skinChange(this.uuid, "", skinURL, textureUUID, skinType);
        });

    // Render active skin in different style.
    Optional<UUID> skinUUID = this.entity.getSkinUUID();
    if (skinUUID.isPresent() && skinUUID.get().equals(textureUUID)) {
      guiGraphics.pose().pushPose();
      guiGraphics.blit(Constants.TEXTURE_CONFIGURATION, skinButtonLeft, skinButtonTop, 0,
          skinButtonHeight, skinPreviewWidth, skinButtonHeight);
      guiGraphics.pose().popPose();
    }

    // Render skin entity with variant and profession.
    ScreenHelper.renderEntityPlayerSkin(x, y, x - this.xMouse, y - 40 - this.yMouse, this.entity,
        textureUUID, skinType);

    skinButtons.add(skinButton);
  }

  private void clearTextureSkinLocation() {
    if (!this.textureSkinLocationBox.getValue().isEmpty()) {
      this.textureSkinLocationBox.setValue("");
    }
  }

  private void addTextureSkinLocation() {
    String textureSkinLocationValue = this.textureSkinLocationBox.getValue();
    if (textureSkinLocationValue != null
        && !textureSkinLocationValue.equals(this.formerTextureSkinLocation)
        && (textureSkinLocationValue.isEmpty()
            || PlayersUtils.isValidPlayerName(textureSkinLocationValue)
            || PlayersUtils.isValidUrl(textureSkinLocationValue))) {

      if (PlayersUtils.isValidPlayerName(textureSkinLocationValue)) {
        log.debug("Settings player user texture to {}", textureSkinLocationValue);
        NetworkMessage.skinChange(this.uuid, textureSkinLocationValue, SkinType.PLAYER_SKIN);
      } else if (PlayersUtils.isValidUrl(textureSkinLocationValue)) {
        log.debug("Setting remote user texture to {}", textureSkinLocationValue);
        NetworkMessage.skinChange(this.uuid, textureSkinLocationValue,
            SkinType.INSECURE_REMOTE_URL);
      }

      this.addTextureSettingsButton.active = false;
      this.formerTextureSkinLocation = textureSkinLocationValue;
      updateNextTextureSkinLocationChange();
    }
  }

  private static void updateNextTextureSkinLocationChange() {
    PlayerSkinConfigurationScreen.nextTextureSkinLocationChange =
        (int) java.time.Instant.now().getEpochSecond() + ADD_SKIN_DELAY;
  }

  private void validateTextureSkinLocation() {
    String textureSkinLocationValue = this.textureSkinLocationBox.getValue();

    // Additional check to make sure that the server is not spammed with requests.
    if (!this.canTextureSkinLocationChange && textureSkinLocationValue != null) {
      this.addTextureSettingsButton.active = false;
      this.clearTextureSettingsButton.active = true;
      return;
    }

    // Validations per skin models.
    switch (skinModel) {
      case HUMANOID:
      case HUMANOID_SLIM:
        this.addTextureSettingsButton.active =
            textureSkinLocationValue != null && !textureSkinLocationValue.isEmpty()
                && (PlayersUtils.isValidPlayerName(textureSkinLocationValue)
                    || PlayersUtils.isValidUrl(textureSkinLocationValue));
        break;
      default:
        this.addTextureSettingsButton.active = PlayersUtils.isValidUrl(textureSkinLocationValue);
    }

    // Clear button
    this.clearTextureSettingsButton.active =
        textureSkinLocationValue != null && !textureSkinLocationValue.isEmpty();
  }

  private void checkSkinButtonState() {
    // Check the visible for the buttons.
    boolean skinButtonShouldBeVisible = this.numOfSkins > this.maxSkinsPerPage;
    this.skinPreviousButton.visible = skinButtonShouldBeVisible;
    this.skinNextButton.visible = skinButtonShouldBeVisible;
    this.skinPreviousPageButton.visible = skinButtonShouldBeVisible;
    this.skinNextPageButton.visible = skinButtonShouldBeVisible;

    // Enable / disable buttons depending on the current skin index.
    this.skinPreviousButton.active = this.skinStartIndex > 0;
    this.skinNextButton.active = this.skinStartIndex + this.maxSkinsPerPage < this.numOfSkins;
    this.skinPreviousPageButton.active = this.skinStartIndex - this.maxSkinsPerPage > 0;
    this.skinNextPageButton.active =
        this.skinStartIndex + 1 + this.maxSkinsPerPage < this.numOfSkins;
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.playerSkinButton.active = false;

    // Entity specific information.
    this.numOfSkins = PlayerTextureManager.getPlayerTextureCacheKeys(skinModel).size();

    // Texture Skin Location
    this.textureSkinLocationBox = new EditBox(this.font, this.contentLeftPos, this.topPos + 60, 160,
        20, Component.translatable("Skin Location"));
    this.textureSkinLocationBox.setMaxLength(255);
    this.textureSkinLocationBox.setValue("");
    this.textureSkinLocationBox.setResponder(consumer -> this.validateTextureSkinLocation());
    this.addRenderableWidget(this.textureSkinLocationBox);

    // Add Button
    this.addTextureSettingsButton = this.addRenderableWidget(
        menuButton(this.textureSkinLocationBox.getX() + this.textureSkinLocationBox.getWidth() + 2,
            this.topPos + 60, 65, "add", onPress -> {
              this.addTextureSkinLocation();
            }));
    this.addTextureSettingsButton.active = false;

    // Clear Texture Buttons
    this.clearTextureSettingsButton = this.addRenderableWidget(menuButton(
        this.addTextureSettingsButton.getX() + this.addTextureSettingsButton.getWidth() + 1,
        this.topPos + 60, 55, "clear", onPress -> {
          this.clearTextureSkinLocation();
        }));

    // Skin Navigation Buttons
    int skinButtonTop = this.topPos + 187;
    int skinButtonLeft = this.contentLeftPos;
    int skinButtonRight = this.rightPos - 31;
    this.skinPreviousPageButton = this.addRenderableWidget(
        menuButton(skinButtonLeft, skinButtonTop, 20, Component.literal("<<"), onPress -> {
          if (this.skinStartIndex - maxSkinsPerPage > 0) {
            skinStartIndex = skinStartIndex - maxSkinsPerPage;
          } else {
            skinStartIndex = 0;
          }
          checkSkinButtonState();
        }));
    this.skinPreviousButton = this.addRenderableWidget(
        menuButton(skinButtonLeft + 20, skinButtonTop, 20, Component.literal("<"), onPress -> {
          if (this.skinStartIndex > 0) {
            skinStartIndex--;
          }
          checkSkinButtonState();
        }));
    this.skinNextPageButton = this.addRenderableWidget(
        menuButton(skinButtonRight, skinButtonTop, 20, Component.literal(">>"), onPress -> {
          if (this.skinStartIndex >= 0
              && this.skinStartIndex + this.maxSkinsPerPage < this.numOfSkins) {
            this.skinStartIndex = this.skinStartIndex + this.maxSkinsPerPage;
          } else if (this.numOfSkins > this.maxSkinsPerPage) {
            this.skinStartIndex = this.numOfSkins - this.maxSkinsPerPage;
          } else {
            this.skinStartIndex = this.numOfSkins;
          }
          checkSkinButtonState();
        }));
    this.skinNextButton = this.addRenderableWidget(
        menuButton(skinButtonRight - 20, skinButtonTop, 20, Component.literal(">"), onPress -> {
          if (this.skinStartIndex >= 0
              && this.skinStartIndex < this.numOfSkins - this.maxSkinsPerPage) {
            skinStartIndex++;
          }
          checkSkinButtonState();
        }));
    checkSkinButtonState();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    if (this.isPlayerSkinModel) {
      guiGraphics.drawString(this.font,
          Component.translatable(Constants.TEXT_CONFIG_PREFIX + "use_a_player_name"),
          this.contentLeftPos, this.topPos + 50, 4210752);
    } else {
      guiGraphics.drawString(this.font,
          Component.translatable(Constants.TEXT_CONFIG_PREFIX + "use_a_skin_url"),
          this.contentLeftPos, this.topPos + 50, 4210752);
    }

    // Reload protection
    this.canTextureSkinLocationChange = java.time.Instant.now()
        .getEpochSecond() >= PlayerSkinConfigurationScreen.nextTextureSkinLocationChange;

    // Render Status Symbol and text, if needed.
    if (!this.canTextureSkinLocationChange) {
      RenderSystem.setShader(GameRenderer::getPositionTexShader);
      RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
      RenderSystem.setShaderTexture(0, Constants.TEXTURE_CONFIGURATION);
      guiGraphics.pose().translate(0, 0, 100);
      guiGraphics.blit(Constants.TEXTURE_CONFIGURATION, this.leftPos + 155, this.topPos + 65, 82, 1,
          8, 10);

      // Show processing text.
      guiGraphics.drawString(this.font,
          Component.translatable(Constants.TEXT_CONFIG_PREFIX + "processing_skin"),
          this.leftPos + 55, this.topPos + 88, 4210752);
    }

    // Skins
    this.renderSkins(guiGraphics);

    // Make sure we pass the mouse movements to the dynamically added buttons, if any.
    if (!skinButtons.isEmpty()) {
      for (Button skinButton : skinButtons) {
        skinButton.render(guiGraphics, x, y, partialTicks);
      }
    }
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    // Skin Selection
    guiGraphics.fill(this.contentLeftPos, this.topPos + 102, this.contentLeftPos + 282,
        this.topPos + 188, 0xff000000);
    guiGraphics.fill(this.contentLeftPos + 1, this.topPos + 103, this.contentLeftPos + 281,
        this.topPos + 187, 0xffaaaaaa);
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    // Make sure we pass the mouse click to the dynamically added buttons, if any.
    if (!skinButtons.isEmpty()) {
      for (Button skinButton : skinButtons) {
        skinButton.mouseClicked(mouseX, mouseY, button);
      }
    }
    return super.mouseClicked(mouseX, mouseY, button);
  }
}
