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

import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.ImageButton;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.Profession;
import de.markusbordihn.easynpc.menu.configuration.skin.DefaultSkinConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessage;
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
  private Profession[] professions;
  private Enum<?>[] variants;
  protected int numOfProfessions = 0;
  protected int numOfSkins = 0;
  protected int numOfVariants = 0;

  public DefaultSkinConfigurationScreen(DefaultSkinConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  private void renderSkins(GuiGraphics guiGraphics) {
    if (this.entity == null) {
      return;
    }

    int skinPosition = 0;
    skinButtons = new ArrayList<>();
    for (int i = skinStartIndex; i < this.numOfSkins && i < skinStartIndex + maxSkinsPerPage; i++) {
      int variantIndex = this.numOfProfessions > 0 ? i / this.numOfProfessions : i;
      Profession profession =
          this.numOfProfessions > 0 ? this.professions[i - (variantIndex * this.numOfProfessions)]
              : null;
      Enum<?> variant = this.variants[variantIndex];
      int left = this.leftPos + (skinPosition > 4 ? -(skinPreviewWidth * 4) - 24 : 32)
          + (skinPosition * (skinPreviewWidth));
      int top = this.contentTopPos + 82 + (skinPosition > 4 ? 84 : 0);

      // Render skin with additional variant and professions.
      this.renderSkinEntity(guiGraphics, left, top, variant, profession);

      // Render skin name
      int topNamePos = Math.round((top - 76f) / SKIN_NAME_SCALING);
      int leftNamePos = Math.round((left - 21f) / SKIN_NAME_SCALING);
      guiGraphics.pose().pushPose();
      guiGraphics.pose().translate(0, 0, 100);
      guiGraphics.pose().scale(SKIN_NAME_SCALING, SKIN_NAME_SCALING, SKIN_NAME_SCALING);
      String variantName = TextUtils.normalizeString(variant.name(), 12);
      guiGraphics.drawString(this.font, Component.literal(variantName), leftNamePos, topNamePos,
          Constants.FONT_COLOR_DARK_GREEN);
      if (profession != null) {
        String professionName = TextUtils.normalizeString(profession.name(), 11);
        guiGraphics.drawString(this.font, Component.literal(professionName), leftNamePos,
            topNamePos + 10, Constants.FONT_COLOR_BLACK);
      }
      guiGraphics.pose().popPose();

      skinPosition++;
    }
  }

  private void renderSkinEntity(GuiGraphics guiGraphics, int x, int y, Enum<?> variant,
      Profession profession) {

    // Create dynamically button for each skin variant and profession.
    int skinButtonLeft = x - 24;
    int skinButtonTop = y - 81;
    int skinButtonHeight = 84;
    ImageButton skinButton = new ImageButton(skinButtonLeft, skinButtonTop, skinPreviewWidth,
        skinButtonHeight, 0, -84, 84, Constants.TEXTURE_CONFIGURATION, button -> {
          NetworkMessage.variantChange(this.uuid, variant);
          if (profession != null) {
            NetworkMessage.professionChange(this.uuid, profession);
          }
          NetworkMessage.skinChange(this.uuid, SkinType.DEFAULT);
        });

    // Render active skin in different style.
    if (this.entity.getSkinType() == SkinType.DEFAULT && this.entity.getVariant().equals(variant)
        && (profession == null || this.entity.getProfession().equals(profession))) {
      guiGraphics.pose().pushPose();
      guiGraphics.blit(Constants.TEXTURE_CONFIGURATION, skinButtonLeft, skinButtonTop, 0,
          skinButtonHeight, skinPreviewWidth, skinButtonHeight);
      guiGraphics.pose().popPose();
    }

    // Render skin entity with variant and profession.
    ScreenHelper.renderEntityDefaultSkin(x, y, x - this.xMouse, y - 40 - this.yMouse, this.entity,
        variant, profession);

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
    this.defaultSkinButton.active = false;

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
    int skinButtonTop = this.topPos + 212;
    int skinButtonLeft = this.contentLeftPos;
    int skinButtonRight = this.leftPos + 269;
    this.skinPreviousPageButton = this.addRenderableWidget(
        menuButton(skinButtonLeft, skinButtonTop, 20, Component.translatable("<<"), onPress -> {
          if (this.skinStartIndex - maxSkinsPerPage > 0) {
            skinStartIndex = skinStartIndex - maxSkinsPerPage;
          } else {
            skinStartIndex = 0;
          }
          checkSkinButtonState();
        }));
    this.skinPreviousButton = this.addRenderableWidget(
        menuButton(skinButtonLeft + 20, skinButtonTop, 20, Component.translatable("<"), onPress -> {
          if (this.skinStartIndex > 0) {
            skinStartIndex--;
          }
          checkSkinButtonState();
        }));
    this.skinNextPageButton = this.addRenderableWidget(
        menuButton(skinButtonRight, skinButtonTop, 20, Component.translatable(">>"), onPress -> {
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
    this.skinNextButton = this.addRenderableWidget(menuButton(skinButtonRight - 20, skinButtonTop,
        20, Component.translatable(">"), onPress -> {
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
    guiGraphics.fill(this.contentLeftPos, this.contentTopPos, this.contentLeftPos + 282,
        this.contentTopPos + 170, 0xff000000);
    guiGraphics.fill(this.contentLeftPos + 1, this.contentTopPos + 1, this.contentLeftPos + 281,
        this.contentTopPos + 169, 0xffaaaaaa);
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
