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

import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.world.item.DyeColor;

public class ColorButton extends CustomButton {

  public static final int DEFAULT_WIDTH = 18;
  public static final int DEFAULT_HEIGHT = 18;
  DyeColor color = DyeColor.WHITE;

  public ColorButton(int x, int y, OnPress onPress) {
    this(x, y, DEFAULT_WIDTH, DEFAULT_HEIGHT, onPress);
  }

  public ColorButton(int x, int y, int width, int height, OnPress onPress) {
    super(x, y, width, height, onPress);
  }

  @Override
  public void onClick(double x, double y) {
    int colorIndex = this.color.getId() + 1;
    if (colorIndex >= DyeColor.values().length) {
      colorIndex = 0;
    }
    this.color = DyeColor.byId(colorIndex);
    super.onClick(x, y);
  }

  @Override
  public void renderButton(GuiGraphics guiGraphics, int left, int top, float partialTicks) {
    super.renderButton(guiGraphics, left, top, partialTicks);

    int textColor = getColorValue();
    if (textColor >= 0) {
      guiGraphics.fill(
          getX() + 2,
          getY() + 2,
          getX() + getWidth() - 2,
          getY() + getHeight() - 2,
          0xff000000 | textColor);
    }
  }

  public DyeColor getColor() {
    return this.color;
  }

  public void setColor(DyeColor color) {
    this.color = color;
  }

  public int getColorValue() {
    return this.color.getTextColor();
  }
}
