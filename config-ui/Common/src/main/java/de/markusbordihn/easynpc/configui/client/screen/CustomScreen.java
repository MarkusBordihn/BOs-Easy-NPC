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

package de.markusbordihn.easynpc.configui.client.screen;

import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.data.screen.AdditionalScreenDataInterface;
import de.markusbordihn.easynpc.menu.ScreenMenuInterface;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.AbstractContainerMenu;

public class CustomScreen<
        T extends AbstractContainerMenu & ScreenMenuInterface<D>,
        D extends AdditionalScreenDataInterface>
    extends de.markusbordihn.easynpc.client.screen.Screen<T, D> {

  private static final int TITLE_HEIGHT = 19;

  protected CustomScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  protected CustomScreen(T menu, Inventory inventory, Component component, int width, int height) {
    super(menu, inventory, component, width, height);
  }

  public Font getFont() {
    return this.font;
  }

  @Override
  protected void init() {
    super.init();

    if (this.closeButton != null) {
      this.closeButton.setX(this.width - 15 - 4);
      this.closeButton.setY(TITLE_HEIGHT + 4);
    }
  }

  @Override
  protected void renderLabels(GuiGraphics guiGraphics, int mouseX, int mouseY) {
    guiGraphics.drawString(this.font, this.title, 6, 6, Constants.FONT_COLOR_BLACK, false);
  }

  @Override
  public void renderBackground(GuiGraphics guiGraphics, int x, int y, float partialTicks) {

    // Title section
    Graphics.blit(guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, 0, 0, 0, 0, 248, TITLE_HEIGHT);

    // Body section
    int startY = TITLE_HEIGHT - 2;
    Graphics.blit(guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, 0, startY, 0, 0, 245, 161);
    Graphics.blit(
        guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, 245, startY, 4, 0, this.width, 161);
    Graphics.blit(
        guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, this.width - 244, startY, 4, 0, 244, 161);
    Graphics.blit(
        guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, 0, this.height - 161, 0, 4, 245, 161);
    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_DEMO_BACKGROUND,
        245,
        this.height - 161,
        4,
        4,
        this.width,
        161);
    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_DEMO_BACKGROUND,
        this.width - 244,
        this.height - 161,
        4,
        4,
        244,
        161);

    this.renderLabels(guiGraphics, x, y);
  }
}
