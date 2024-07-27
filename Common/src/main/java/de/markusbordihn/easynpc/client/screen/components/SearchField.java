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

package de.markusbordihn.easynpc.client.screen.components;

import de.markusbordihn.easynpc.Constants;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.resources.ResourceLocation;

public class SearchField extends TextField {

  public static final ResourceLocation SPRITE = Constants.TEXTURE_CONFIGURATION;
  public static final int SPRITE_HEIGHT = 12;
  public static final int SPRITE_OFFSET_X = 93;
  public static final int SPRITE_OFFSET_Y = 30;
  public static final int SPRITE_WIDTH = 12;
  public static final int SPRITE_X = 1;
  public static final int SPRITE_Y = 2;

  public SearchField(Font font, int x, int y, int width, int height) {
    super(font, x + SPRITE_WIDTH, y - 1, width, height + 2);
    this.setCursorPosition(15);
    this.setHighlightPos(15);
  }

  @Override
  public void renderWidget(GuiGraphics guiGraphics, int left, int top, float partialTicks) {
    super.renderWidget(guiGraphics, left, top, partialTicks);

    // Expand Search Field Background and Border
    int stylePositionX = this.getX() - SPRITE_WIDTH;
    guiGraphics.fill(
        stylePositionX - 1,
        this.getY(),
        stylePositionX + 16,
        this.getY() + this.height,
        isFocused() ? 0xFFFFFFFF : 0xFFA0A0A0);
    guiGraphics.fill(
        stylePositionX,
        this.getY() + 1,
        stylePositionX + 16,
        this.getY() + this.height - 1,
        0xFF000000);

    // Button Sprite
    guiGraphics.blit(
        SPRITE,
        stylePositionX + SPRITE_X,
        this.getY() + SPRITE_Y + 1,
        SPRITE_OFFSET_X,
        isFocused() ? SPRITE_OFFSET_Y : SPRITE_OFFSET_Y + SPRITE_HEIGHT,
        SPRITE_WIDTH,
        SPRITE_HEIGHT);
  }
}
