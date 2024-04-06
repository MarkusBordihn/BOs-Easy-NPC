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
import de.markusbordihn.easynpc.client.screen.components.SkinSelectionButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.io.CustomSkinDataFiles;
import de.markusbordihn.easynpc.menu.configuration.skin.CustomSkinConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import net.minecraft.Util;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

public class CustomSkinConfigurationScreen
    extends SkinConfigurationScreen<CustomSkinConfigurationMenu> {

  private static final float SKIN_NAME_SCALING = 0.7f;
  private static final int ADD_SKIN_RELOAD_DELAY = 5;
  protected static int nextSkinReload = (int) java.time.Instant.now().getEpochSecond();
  protected final SkinData<?> skinData = this.easyNPC.getEasyNPCSkinData();
  protected Button skinFolderButton = null;
  protected Button skinReloadButton = null;
  protected int numberOfTextLines = 1;
  protected int lastNumOfSkins = 0;
  private List<FormattedCharSequence> textComponents = Collections.emptyList();

  public CustomSkinConfigurationScreen(
      CustomSkinConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void renderSkins(PoseStack poseStack) {
    if (this.easyNPC == null) {
      return;
    }

    int positionTop = 119;
    int skinPosition = 0;
    skinButtons = new ArrayList<>();

    Set<UUID> textures = CustomTextureManager.getCustomTextureCacheKeys(skinModel);
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
      int topNamePos = Math.round((top - 76f) / SKIN_NAME_SCALING);
      int leftNamePos = Math.round((left - 21f) / SKIN_NAME_SCALING);
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

    // Create dynamically button for each skin variant.
    Button skinButton =
        new SkinSelectionButton(
            x - 24,
            y - 81,
            button ->
                NetworkMessageHandler.skinChange(this.uuid, "", "", textureUUID, SkinType.CUSTOM));

    // Disable button for active skin.
    UUID skinUUID = this.skinData.getSkinUUID();
    skinButton.active = !(skinUUID.equals(textureUUID));

    // Render skin entity with variant and profession.
    ScreenHelper.renderEntityPlayerSkin(
        x + 4,
        y,
        x - this.xMouse,
        y - 40 - this.yMouse,
        this.easyNPC,
        textureUUID,
        SkinType.CUSTOM);

    skinButtons.add(skinButton);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.customSkinButton.active = false;

    // Entity specific information.
    this.numOfSkins = CustomTextureManager.getCustomTextureCacheKeys(skinModel).size();

    // Skin Navigation Buttons
    int skinButtonTop = this.topPos + 187;
    int skinButtonLeft = this.contentLeftPos;
    int skinButtonRight = this.rightPos - 31;
    this.skinPreviousPageButton =
        this.addRenderableWidget(
            new TextButton(
                skinButtonLeft,
                skinButtonTop,
                20,
                "<<",
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
                "<",
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
                ">>",
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
                ">",
                onPress -> {
                  if (this.skinStartIndex >= 0
                      && this.skinStartIndex < this.numOfSkins - this.maxSkinsPerPage) {
                    skinStartIndex++;
                  }
                  checkSkinNavigationButtonState();
                }));
    checkSkinNavigationButtonState();

    // Open Skin Folder Button
    Path skinModelFolder = CustomSkinDataFiles.getCustomSkinDataFolder(skinModel);
    if (skinModelFolder != null) {
      this.skinFolderButton =
          this.addRenderableWidget(
              new TextButton(
                  this.contentLeftPos + 10,
                  skinButtonTop - 114,
                  263,
                  "open_textures_folder",
                  skinModel.toString(),
                  onPress -> Util.getPlatform().openFile(skinModelFolder.toFile())));
    }

    // Skin Reload Button
    this.skinReloadButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos + 60,
                skinButtonTop,
                160,
                "reload_textures",
                onPress -> {
                  CustomSkinDataFiles.refreshRegisterTextureFiles();
                  CustomSkinConfigurationScreen.nextSkinReload =
                      (int) java.time.Instant.now().getEpochSecond() + ADD_SKIN_RELOAD_DELAY;
                }));

    // Pre-format text
    this.textComponents =
        this.font.split(
            Component.translatable(Constants.TEXT_CONFIG_PREFIX + "custom_skin_text"),
            this.imageWidth - 20);
    this.numberOfTextLines = this.textComponents.size();
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Information text
    if (!this.textComponents.isEmpty()) {
      for (int line = 0; line < this.numberOfTextLines; ++line) {
        FormattedCharSequence formattedCharSequence = this.textComponents.get(line);
        Text.drawString(
            poseStack,
            this.font,
            formattedCharSequence,
            leftPos + 10,
            topPos + 45 + (line * (font.lineHeight + 2)));
      }
    }

    // Throttle the skin reload button.
    boolean canSkinReload =
        java.time.Instant.now().getEpochSecond() >= CustomSkinConfigurationScreen.nextSkinReload;
    if (!canSkinReload) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "skin_reloading",
          leftPos + 55,
          topPos + 215,
          Constants.FONT_COLOR_RED);
    }
    this.skinReloadButton.active = canSkinReload;

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
