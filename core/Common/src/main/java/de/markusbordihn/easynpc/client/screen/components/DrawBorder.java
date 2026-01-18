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

public class DrawBorder {

  private static final int DEFAULT_BORDER_COLOR = 0xFF555555;

  private DrawBorder() {}

  public static void draw(GuiGraphics guiGraphics, int x, int y, int width, int height) {
    draw(guiGraphics, x, y, width, height, DEFAULT_BORDER_COLOR);
  }

  public static void draw(
      GuiGraphics guiGraphics, int x, int y, int width, int height, int borderColor) {
    guiGraphics.fill(x, y, x + width, y + 1, borderColor);
    guiGraphics.fill(x, y + height - 1, x + width, y + height, borderColor);
    guiGraphics.fill(x, y, x + 1, y + height, borderColor);
    guiGraphics.fill(x + width - 1, y, x + width, y + height, borderColor);
  }

  public static void drawVerticalSeparator(
      GuiGraphics guiGraphics, int x, int y, int height, int color) {
    guiGraphics.fill(x, y, x + 1, y + height, color);
  }
}
