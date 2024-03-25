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
import de.markusbordihn.easynpc.client.screen.components.SkinSelectionButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.menu.configuration.skin.DefaultSkinConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.ArrayList;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class DefaultSkinConfigurationScreen
    extends SkinConfigurationScreen<DefaultSkinConfigurationMenu> {

  // Skin Preview
  private static final float SKIN_NAME_SCALING = 0.7f;
  protected final SkinData<?> skinData;
  protected final VariantData<?> variantData;
  protected final ProfessionData<?> professionData;
  protected int numOfProfessions = 0;
  protected int numOfVariants = 0;
  // Cache
  private Profession[] professions;
  private Enum<?>[] variants;

  public DefaultSkinConfigurationScreen(
      DefaultSkinConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.skinData = this.easyNPC.getEasyNPCSkinData();
    this.professionData = this.easyNPC.getEasyNPCProfessionData();
    this.variantData = this.easyNPC.getEasyNPCVariantData();
    this.maxSkinsPerPage = 10;
  }

  private void renderSkins(GuiGraphics guiGraphics) {
    if (this.easyNPC == null) {
      return;
    }

    int skinPosition = 0;
    skinButtons = new ArrayList<>();
    for (int i = skinStartIndex; i < this.numOfSkins && i < skinStartIndex + maxSkinsPerPage; i++) {
      int variantIndex = this.numOfProfessions > 0 ? i / this.numOfProfessions : i;
      Profession profession =
          this.numOfProfessions > 0
              ? this.professions[i - (variantIndex * this.numOfProfessions)]
              : null;
      Enum<?> variant = this.variants[variantIndex];
      int left =
          this.leftPos
              + (skinPosition > 4 ? -(SKIN_PREVIEW_WIDTH * 4) - 28 : 32)
              + (skinPosition * (SKIN_PREVIEW_WIDTH));
      int top = this.contentTopPos + 82 + (skinPosition > 4 ? 84 : 0);

      // Render skin with additional variant and professions.
      this.renderSkinEntity(guiGraphics, left, top, variant, profession);

      // Render skin name
      int topNamePos = Math.round((top - 76f) / SKIN_NAME_SCALING);
      int leftNamePos = Math.round((left - 21f) / SKIN_NAME_SCALING);
      guiGraphics.pose().pushPose();
      guiGraphics.pose().translate(0, 0, 100);
      guiGraphics.pose().scale(SKIN_NAME_SCALING, SKIN_NAME_SCALING, SKIN_NAME_SCALING);
      String variantName = TextUtils.normalizeString(variant.name(), 14);
      Text.drawString(
          guiGraphics,
          this.font,
          variantName,
          leftNamePos,
          topNamePos,
          Constants.FONT_COLOR_DARK_GREEN);
      if (profession != null) {
        String professionName = TextUtils.normalizeString(profession.name(), 13);
        Text.drawString(
            guiGraphics,
            this.font,
            professionName,
            leftNamePos,
            topNamePos + 10,
            Constants.FONT_COLOR_BLACK);
      }
      guiGraphics.pose().popPose();

      skinPosition++;
    }
  }

  private void renderSkinEntity(
      GuiGraphics guiGraphics, int x, int y, Enum<?> variant, Profession profession) {

    // Create dynamically button for each skin variant and profession.
    Button skinButton =
        new SkinSelectionButton(
            x - 24,
            y - 81,
            button -> {
              NetworkMessageHandler.variantChange(this.uuid, variant);
              if (profession != null) {
                NetworkMessageHandler.professionChange(this.uuid, profession);
              }
              NetworkMessageHandler.skinChange(this.uuid, SkinType.DEFAULT);
            });

    // Disable button for active skin.
    skinButton.active =
        !(this.skinData.getSkinType() == SkinType.DEFAULT
            && this.variantData.getVariant().equals(variant)
            && (profession == null || this.professionData.getProfession().equals(profession)));

    // Render skin entity with variant and profession.
    ScreenHelper.renderEntityDefaultSkin(
        x + 4, y, x - this.xMouse, y - 40 - this.yMouse, this.easyNPC, variant, profession);

    skinButtons.add(skinButton);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.defaultSkinButton.active = false;

    // Entity specific information.
    this.professions = this.professionData.getProfessions();
    this.variants = this.variantData.getVariants();
    this.numOfProfessions = this.professionData.hasProfessions() ? this.professions.length : 0;
    this.numOfVariants = this.variants.length;
    this.numOfSkins =
        numOfProfessions > 0 ? this.numOfVariants * this.numOfProfessions : this.numOfVariants;
    log.debug(
        "Found about {} skins with {} variants and {} professions.",
        this.numOfSkins,
        this.numOfVariants,
        this.numOfProfessions);

    // Skin Navigation Buttons
    int skinButtonTop = this.topPos + 212;
    int skinButtonLeft = this.contentLeftPos;
    int skinButtonRight = this.leftPos + 289;
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
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Make sure we pass the mouse movements to the dynamically added buttons, if any.
    if (!skinButtons.isEmpty()) {
      for (Button skinButton : skinButtons) {
        skinButton.render(guiGraphics, x, y, partialTicks);
      }
    }

    // Skins
    this.renderSkins(guiGraphics);
  }

  @Override
  protected void renderSkinSelectionBackground(GuiGraphics guiGraphics) {
    guiGraphics.fill(
        this.contentLeftPos,
        this.contentTopPos,
        this.contentLeftPos + 302,
        this.contentTopPos + 170,
        0xff000000);
    guiGraphics.fill(
        this.contentLeftPos + 1,
        this.contentTopPos + 1,
        this.contentLeftPos + 301,
        this.contentTopPos + 169,
        0xffaaaaaa);
  }
}
