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
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.ArrayList;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class DefaultSkinConfigurationScreen<T extends ConfigurationMenu>
    extends SkinConfigurationScreen<T> {

  private static final float SKIN_NAME_SCALING = 0.7f;
  protected int numOfProfessions = 0;
  protected int numOfVariants = 0;

  public DefaultSkinConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.maxSkinsPerPage = 10;
  }

  private void renderSkins(GuiGraphics guiGraphics) {
    if (this.getEasyNPCEntity() == null) {
      return;
    }

    int skinPosition = 0;
    skinButtons = new ArrayList<>();
    ProfessionData<?> professionData = this.getEasyNPC().getEasyNPCProfessionData();
    VariantData<?> variantData = this.getEasyNPC().getEasyNPCVariantData();
    for (int i = skinStartIndex; i < this.numOfSkins && i < skinStartIndex + maxSkinsPerPage; i++) {
      int variantIndex = this.numOfProfessions > 0 ? i / this.numOfProfessions : i;
      Profession profession =
          this.numOfProfessions > 0
              ? professionData.getProfessions()[i - (variantIndex * this.numOfProfessions)]
              : null;
      Enum<?> variant = variantData.getVariants()[variantIndex];
      int left =
          this.leftPos
              + (skinPosition > 4 ? -(SKIN_PREVIEW_WIDTH * 4) - 28 : 32)
              + (skinPosition * (SKIN_PREVIEW_WIDTH));
      int top = this.contentTopPos + 102 + (skinPosition > 4 ? 84 : 0);

      // Render skin with additional variant and professions.
      this.renderSkinEntity(left, top, variant, profession);

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

  private void renderSkinEntity(int x, int y, Enum<?> variant, Profession profession) {

    // Create dynamically button for each skin variant and profession.
    Button skinButton =
        new SkinSelectionButton(
            x - 24,
            y - 81,
            button -> {
              if (profession != null) {
                NetworkMessageHandlerManager.getServerHandler()
                    .changeProfession(this.getNpcUUID(), profession);
              }
              NetworkMessageHandlerManager.getServerHandler()
                  .setDefaultSkin(this.getNpcUUID(), variant);
            });

    // Disable button for active skin.
    SkinData<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    VariantData<?> variantData = this.getEasyNPC().getEasyNPCVariantData();
    ProfessionData<?> professionData = this.getEasyNPC().getEasyNPCProfessionData();
    skinButton.active =
        !(skinData.getSkinType() == SkinType.DEFAULT
            && variantData.getVariant().equals(variant)
            && (profession == null || professionData.getProfession().equals(profession)));

    // Render skin entity with variant and profession.
    ScreenHelper.renderEntityDefaultSkin(
        x + 4, y, x - this.xMouse, y - 40 - this.yMouse, this.getEasyNPC(), variant, profession);

    skinButtons.add(skinButton);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.defaultSkinButton.active = false;

    // Description text
    setDescriptionText("default_skin.text");

    // Entity specific information.
    VariantData<?> variantData = this.getEasyNPC().getEasyNPCVariantData();
    ProfessionData<?> professionData = this.getEasyNPC().getEasyNPCProfessionData();
    this.numOfProfessions =
        professionData.hasProfessions() ? professionData.getProfessions().length : 0;
    this.numOfVariants = variantData.getVariants().length;
    this.numOfSkins =
        numOfProfessions > 0 ? this.numOfVariants * this.numOfProfessions : this.numOfVariants;
    log.debug(
        "Found about {} skins with {} variants and {} professions.",
        this.numOfSkins,
        this.numOfVariants,
        this.numOfProfessions);

    // Skin Navigation Buttons
    defineSkinNavigationButtons();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Description text
    renderDescriptionText(guiGraphics, this.contentLeftPos + 5, this.contentTopPos + 5);

    // Skins
    this.renderSkins(guiGraphics);
  }

  @Override
  protected void renderSkinSelectionBackground(GuiGraphics guiGraphics) {
    guiGraphics.fill(
        this.contentLeftPos,
        this.contentTopPos + 20,
        this.contentLeftPos + 302,
        this.contentTopPos + 190,
        0xff000000);
    guiGraphics.fill(
        this.contentLeftPos + 1,
        this.contentTopPos + 21,
        this.contentLeftPos + 301,
        this.contentTopPos + 189,
        0xffaaaaaa);
  }
}
