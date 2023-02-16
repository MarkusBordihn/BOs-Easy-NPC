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

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.ImageButton;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.menu.configuration.skin.DefaultSkinConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkHandler;
import de.markusbordihn.easynpc.skin.SkinType;
import de.markusbordihn.easynpc.utils.TextUtils;

@OnlyIn(Dist.CLIENT)
public class DefaultSkinConfigurationScreen
    extends SkinConfigurationScreen<DefaultSkinConfigurationMenu> {

  // Internal
  private Button skinPreviousButton = null;
  private Button skinNextButton = null;
  private Button skinPreviousPageButton = null;
  private Button skinNextPageButton = null;
  private List<Button> skinButtons = new ArrayList<>();

  // Skin Preview
  private static final float SKIN_NAME_SCALING = 0.7f;
  private int skinStartIndex = 0;
  private int maxSkinsPerPage = 10;

  // Cache
  private Enum<?>[] professions;
  private Enum<?>[] variants;
  protected int numOfProfessions = 0;
  protected int numOfSkins = 0;
  protected int numOfVariants = 0;

  public DefaultSkinConfigurationScreen(DefaultSkinConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  private void renderSkins(PoseStack poseStack) {
    if (this.entity == null) {
      return;
    }

    int positionTop = 55;
    int skinPosition = 0;
    skinButtons = new ArrayList<>();
    for (int i = skinStartIndex; i < this.numOfSkins && i < skinStartIndex + maxSkinsPerPage; i++) {
      int variantIndex = this.numOfProfessions > 0 ? i / this.numOfProfessions : i;
      Enum<?> profession =
          this.numOfProfessions > 0 ? this.professions[i - (variantIndex * this.numOfProfessions)]
              : null;
      Enum<?> variant = this.variants[variantIndex];
      int left = this.leftPos + (skinPosition > 4 ? -228 : 32) + (skinPosition * 52);
      int top = this.topPos + 65 + positionTop + (skinPosition > 4 ? 84 : 0);

      // Render skin with additional variant and professions.
      this.renderSkinEntity(poseStack, left, top, variant, profession);

      // Render skin name
      float topNamePos = (top - 76f) / SKIN_NAME_SCALING;
      float leftNamePos = (left - 21f) / SKIN_NAME_SCALING;
      poseStack.pushPose();
      poseStack.translate(0, 0, 100);
      poseStack.scale(SKIN_NAME_SCALING, SKIN_NAME_SCALING, SKIN_NAME_SCALING);
      String variantName = TextUtils.normalizeString(variant.name(), 10);
      this.font.draw(poseStack, Component.literal(variantName), leftNamePos, topNamePos,
          Constants.FONT_COLOR_DARK_GREEN);
      if (profession != null) {
        String professionName = TextUtils.normalizeString(profession.name(), 10);
        this.font.draw(poseStack, Component.literal(professionName), leftNamePos, topNamePos + 10f,
            Constants.FONT_COLOR_BLACK);
      }
      poseStack.popPose();

      skinPosition++;
    }
  }

  private void renderSkinEntity(PoseStack poseStack, int x, int y, Enum<?> variant,
      Enum<?> profession) {

    // Create dynamically button for each skin variant and profession.
    int skinButtonLeft = x - 24;
    int skinButtonTop = y - 81;
    int skinButtonWidth = 52;
    int skinButtonHeight = 84;
    ImageButton skinButton = new ImageButton(skinButtonLeft, skinButtonTop, skinButtonWidth,
        skinButtonHeight, 0, -84, 84, Constants.TEXTURE_CONFIGURATION, button -> {
          NetworkHandler.variantChange(this.uuid, variant);
          if (profession != null) {
            NetworkHandler.professionChange(this.uuid, profession);
          }
          NetworkHandler.skinChange(this.uuid, SkinType.DEFAULT);
        });

    // Render active skin in different style.
    if (this.entity.getSkinType() == SkinType.DEFAULT && this.entity.getVariant().equals(variant)
        && (profession == null || this.entity.getProfession().equals(profession))) {
      poseStack.pushPose();
      RenderSystem.setShader(GameRenderer::getPositionTexShader);
      RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
      RenderSystem.setShaderTexture(0, Constants.TEXTURE_CONFIGURATION);
      this.blit(poseStack, skinButtonLeft, skinButtonTop, 0, skinButtonHeight, skinButtonWidth,
          skinButtonHeight);
      poseStack.popPose();
    }

    // Render skin entity with variant and profession.
    ScreenHelper.renderEntityDefaultSkin(x, y, x - this.xMouse, y - 40 - this.yMouse, this.entity, variant,
        profession);

    skinButtons.add(skinButton);
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
    this.customSkinButton.active = true;
    this.defaultSkinButton.active = false;
    this.playerSkinButton.active = true;

    // Entity specific information.
    this.professions = this.entity.getProfessions();
    this.variants = this.entity.getVariants();
    this.numOfProfessions = this.entity.hasProfessions() ? this.professions.length : 0;
    this.numOfVariants = this.variants.length;
    this.numOfSkins =
        numOfProfessions > 0 ? this.numOfVariants * this.numOfProfessions : this.numOfVariants;
    log.debug("Found about {} skins with {} variants and {} professions.", this.numOfSkins,
        this.numOfVariants, this.numOfProfessions);

    // Skin Navigation Buttons
    int skinButtonTop = this.topPos + 210;
    int skinButtonLeft = this.leftPos + 7;
    int skinButtonRight = this.leftPos + 249;
    this.skinPreviousPageButton = this.addRenderableWidget(new Button(skinButtonLeft, skinButtonTop,
        20, 20, Component.translatable("<<"), onPress -> {
          if (this.skinStartIndex - maxSkinsPerPage > 0) {
            skinStartIndex = skinStartIndex - maxSkinsPerPage;
          } else {
            skinStartIndex = 0;
          }
          checkSkinButtonState();
        }));
    this.skinPreviousButton = this.addRenderableWidget(new Button(skinButtonLeft + 20,
        skinButtonTop, 20, 20, Component.translatable("<"), onPress -> {
          if (this.skinStartIndex > 0) {
            skinStartIndex--;
          }
          checkSkinButtonState();
        }));
    this.skinNextPageButton = this.addRenderableWidget(new Button(skinButtonRight, skinButtonTop,
        20, 20, Component.translatable(">>"), onPress -> {
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
    this.skinNextButton = this.addRenderableWidget(new Button(skinButtonRight - 20, skinButtonTop,
        20, 20, Component.translatable(">"), onPress -> {
          if (this.skinStartIndex >= 0
              && this.skinStartIndex < this.numOfSkins - this.maxSkinsPerPage) {
            skinStartIndex++;
          }
          checkSkinButtonState();
        }));
    checkSkinButtonState();
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Skins
    this.renderSkins(poseStack);

    // Make sure we pass the mouse movements to the dynamically added buttons, if any.
    if (!skinButtons.isEmpty()) {
      for (Button skinButton : skinButtons) {
        skinButton.render(poseStack, x, y, partialTicks);
      }
    }
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    // Skin Selection
    fill(poseStack, this.leftPos + 7, this.topPos + 38, this.leftPos + 269, this.topPos + 208,
        0xff000000);
    fill(poseStack, this.leftPos + 8, this.topPos + 39, this.leftPos + 268, this.topPos + 207,
        0xffaaaaaa);
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
