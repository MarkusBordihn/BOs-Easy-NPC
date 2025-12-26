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

import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SkinSelectionButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.render.EntityRenderOverrides;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.ArrayList;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.npc.Villager;
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
    VariantDataCapable<?> variantData = this.getEasyNPC().getEasyNPCVariantData();

    for (int i = skinStartIndex; i < this.numOfSkins && i < skinStartIndex + maxSkinsPerPage; i++) {
      int variantIndex = i;
      if (variantIndex >= variantData.getSkinVariantTypes().length) {
        variantIndex = variantIndex % variantData.getSkinVariantTypes().length;
      }

      Enum<?> variant = variantData.getSkinVariantTypes()[variantIndex];
      int left =
          this.leftPos
              + (skinPosition > 4 ? -(SKIN_PREVIEW_WIDTH * 4) - 28 : 32)
              + (skinPosition * (SKIN_PREVIEW_WIDTH));
      int top = this.contentTopPos + 102 + (skinPosition > 4 ? 84 : 0);

      // Render skin entity with variant.
      this.renderSkinEntity(guiGraphics, left, top, variant, null);

      // Render skin name
      int topNamePos = Math.round((top - 76f) / SKIN_NAME_SCALING);
      int leftNamePos = Math.round((left - 21f) / SKIN_NAME_SCALING);
      guiGraphics.pose().pushPose();
      guiGraphics.pose().translate(0, 0, 100);
      guiGraphics.pose().scale(SKIN_NAME_SCALING, SKIN_NAME_SCALING, SKIN_NAME_SCALING);

      // Determine skin variant name and split into type and profession if applicable.
      String variantName = variant.name();
      if (this.getEasyNPCEntity() instanceof Villager
          && variantName.contains("_")
          && !variantName.equals("DEFAULT")) {
        String[] parts = variantName.split("_", 2);

        // Show type as title
        Text.drawString(
            guiGraphics,
            this.font,
            TextUtils.normalizeString(parts[0], 14),
            leftNamePos,
            topNamePos,
            Constants.FONT_COLOR_DARK_GREEN);

        // Show profession as subtitle
        Text.drawString(
            guiGraphics,
            this.font,
            TextUtils.normalizeString(parts[1], 13),
            leftNamePos,
            topNamePos + 10,
            Constants.FONT_COLOR_BLACK);
      } else {
        Text.drawString(
            guiGraphics,
            this.font,
            TextUtils.normalizeString(variantName, 14),
            leftNamePos,
            topNamePos,
            Constants.FONT_COLOR_DARK_GREEN);
      }

      guiGraphics.pose().popPose();

      skinPosition++;
    }
  }

  private void renderSkinEntity(
      GuiGraphics guiGraphics, int x, int y, Enum<?> variantType, Profession profession) {

    Button skinButton =
        new SkinSelectionButton(
            x - 24,
            y - 81,
            button -> {
              if (profession != null) {
                NetworkMessageHandlerManager.getServerHandler()
                    .changeProfession(this.getEasyNPCUUID(), profession);
              }
              NetworkMessageHandlerManager.getServerHandler()
                  .setSkin(
                      this.getEasyNPCUUID(), SkinDataEntry.createDefaultSkin(variantType.name()));
            });

    // Disable button for active skin.
    SkinDataCapable<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    VariantDataCapable<?> variantData = this.getEasyNPC().getEasyNPCVariantData();
    ProfessionDataCapable<?> professionData = this.getEasyNPC().getEasyNPCProfessionData();
    skinButton.active =
        !(skinData.getSkinType() == SkinType.DEFAULT
            && variantData.getSkinVariantType().equals(variantType)
            && (profession == null || professionData.getProfession().equals(profession)));

    EntityConfigScreenRenderer.renderEntity(
        guiGraphics,
        this.getEasyNPC(),
        EntityRenderConfig.withOverrides(
            x + 4, y - 30, 30, EntityRenderOverrides.withVariant(variantType, profession)),
        this.xMouse,
        this.yMouse);

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
    VariantDataCapable<?> variantData = this.getEasyNPC().getEasyNPCVariantData();
    ProfessionDataCapable<?> professionData = this.getEasyNPC().getEasyNPCProfessionData();
    this.numOfProfessions =
        professionData.hasProfessions() ? professionData.getProfessions().length : 0;
    this.numOfVariants = variantData.getSkinVariantTypes().length;
    this.numOfSkins = this.numOfVariants;

    log.debug("Found {} predefined variant combinations.", this.numOfSkins);

    // Disable Layers Checkbox
    SkinDataCapable<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    this.addRenderableWidget(
        new Checkbox(
            this.contentLeftPos + 55,
            this.contentTopPos + 192,
            "disable_skin_layers",
            skinData.getSkinDataEntry().disableLayers(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .setSkin(
                        this.getEasyNPCUUID(),
                        skinData.getSkinDataEntry().withDisableLayers(checkbox.selected()))));

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
