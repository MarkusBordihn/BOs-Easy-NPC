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
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import java.util.UUID;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class NoneSkinConfigurationScreen<T extends ConfigurationMenu>
    extends SkinConfigurationScreen<T> {

  protected Checkbox noneSkinCheckbox;

  public NoneSkinConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.noneSkinButton.active = false;

    // Description text
    setDescriptionText("disable_skin.text");

    // Skin data and variant
    SkinDataCapable<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    VariantDataCapable<?> variantData = this.getEasyNPC().getEasyNPCVariantData();

    // Former skin type and variant
    SkinType formerSkinType = skinData.getSkinType();
    UUID formerSkinUUID = skinData.getSkinUUID();
    Enum<?> formerVariant = variantData.getSkinVariantType();

    // None Dialog Checkbox
    this.noneSkinCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 100,
                this.topPos + 170,
                "disable_skin_checkbox",
                skinData.getSkinType() == SkinType.NONE,
                checkbox -> {
                  if (checkbox.selected()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .setSkin(this.getEasyNPCUUID(), SkinDataEntry.createNoneSkin());
                  } else {
                    NetworkMessageHandlerManager.getServerHandler()
                        .setSkin(
                            this.getEasyNPCUUID(),
                            formerSkinType == SkinType.DEFAULT
                                ? SkinDataEntry.createDefaultSkin(formerVariant.name())
                                : formerSkinType == SkinType.CUSTOM
                                    ? SkinDataEntry.createCustomSkin(formerSkinUUID, true)
                                    : SkinDataEntry.createDefaultSkin(
                                        variantData.getDefaultSkinVariantType().name()));
                  }
                }));
  }

  @Override
  protected void renderSkinSelectionBackground(GuiGraphics guiGraphics) {
    // Do nothing
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Description text
    renderDescriptionText(guiGraphics, this.contentLeftPos + 5, this.contentTopPos + 60);
  }
}
