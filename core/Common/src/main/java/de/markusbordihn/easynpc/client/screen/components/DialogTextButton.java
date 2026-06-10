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
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.util.Mth;

public class DialogTextButton extends TextButton {

  private static final int TEXT_PADDING = 5;
  private static final int LOCK_RESERVE = 14;
  private static final String ELLIPSIS = "...";

  private final int rightPadding;

  public DialogTextButton(
      int left, int top, int width, Component label, boolean hasCondition, OnPress onPress) {
    super(left, top, width, label, onPress);
    this.rightPadding = hasCondition ? LOCK_RESERVE : TEXT_PADDING;
  }

  public boolean isTextTruncated() {
    Component message = this.getMessage();
    if (message == null || message.getString().isEmpty()) {
      return false;
    }
    return Minecraft.getInstance().font.width(message)
        > this.width - TEXT_PADDING - this.rightPadding;
  }

  @Override
  public void renderButtonText(
      GuiGraphics guiGraphics, Font font, Component component, int x, int y) {
    if (component == null || component.getString().isEmpty()) {
      return;
    }

    int color =
        (this.active ? Constants.FONT_COLOR_WHITE : Constants.FONT_COLOR_LIGHT_GRAY)
            | Mth.ceil(this.alpha * 255.0F) << 24;
    int textTop = this.getY() + (this.height - 8) / 2;
    int areaLeft = this.getX() + TEXT_PADDING;
    int areaWidth = this.width - TEXT_PADDING - this.rightPadding;

    if (font.width(component) <= areaWidth) {
      // Short text: keep it centered within the area left of the lock.
      guiGraphics.drawString(
          font,
          component,
          areaLeft + (areaWidth - font.width(component)) / 2,
          textTop,
          color,
          true);
    } else {
      // Long text: render left to right and clip before the lock with an ellipsis.
      String clipped =
          font.plainSubstrByWidth(component.getString(), areaWidth - font.width(ELLIPSIS))
              + ELLIPSIS;
      guiGraphics.drawString(font, clipped, areaLeft, textTop, color, true);
    }
  }
}
