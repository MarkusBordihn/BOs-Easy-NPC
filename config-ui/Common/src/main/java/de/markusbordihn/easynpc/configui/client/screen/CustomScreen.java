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
    Graphics.blitStretched(
        guiGraphics,
        Constants.TEXTURE_PRESET_BROWSER_BACKGROUND,
        0,
        0,
        this.width,
        TITLE_HEIGHT,
        1,
        1,
        333,
        19,
        512,
        512);
    Graphics.blitStretched(
        guiGraphics,
        Constants.TEXTURE_PRESET_BROWSER_BACKGROUND,
        0,
        TITLE_HEIGHT,
        this.width,
        this.height - TITLE_HEIGHT,
        1,
        20,
        333,
        244,
        512,
        512);
    this.renderLabels(guiGraphics, x, y);
  }
}
