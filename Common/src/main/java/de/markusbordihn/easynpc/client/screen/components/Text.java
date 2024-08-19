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
import java.util.List;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;

public class Text {

  private Text() {}

  public static void drawString(GuiGraphics guiGraphics, Font font, String text, int x, int y) {
    drawString(guiGraphics, font, Component.literal(text), x, y);
  }

  public static void drawString(
      GuiGraphics guiGraphics, Font font, String text, int x, int y, int color) {
    drawString(guiGraphics, font, Component.literal(text), x, y, color);
  }

  public static void drawString(
      GuiGraphics guiGraphics, Font font, Component component, int x, int y) {
    drawString(guiGraphics, font, component, x, y, Constants.FONT_COLOR_DEFAULT);
  }

  public static int drawString(
      GuiGraphics guiGraphics, Font font, Component component, int x, int y, int color) {
    return guiGraphics.drawString(font, component, x, y, color, false);
  }

  public static void drawStringShadow(
      GuiGraphics guiGraphics, Font font, String text, int x, int y, int color) {
    guiGraphics.drawString(font, text, x, y, color, true);
  }

  public static int drawStringShadow(
      GuiGraphics guiGraphics, Font font, Component component, int x, int y, int color) {
    return guiGraphics.drawString(font, component, x, y, color, true);
  }

  public static void drawString(
      GuiGraphics guiGraphics,
      Font font,
      FormattedCharSequence formattedCharSequence,
      int x,
      int y) {
    drawString(guiGraphics, font, formattedCharSequence, x, y, Constants.FONT_COLOR_DEFAULT);
  }

  public static void drawErrorMessage(
      GuiGraphics guiGraphics, Font font, String text, int x, int y, int width) {
    drawErrorMessage(guiGraphics, font, Component.literal(text != null ? text : ""), x, y, width);
  }

  public static void drawErrorMessage(
      GuiGraphics guiGraphics, Font font, Component component, int x, int y, int width) {
    List<FormattedCharSequence> textComponents = font.split(component, width);
    int line = 0;
    for (FormattedCharSequence formattedCharSequence : textComponents) {
      Text.drawString(
          guiGraphics,
          font,
          formattedCharSequence,
          x,
          y + (line++ * (font.lineHeight + 2)),
          Constants.FONT_COLOR_RED);
    }
  }

  public static void drawString(
      GuiGraphics guiGraphics,
      Font font,
      FormattedCharSequence formattedCharSequence,
      int x,
      int y,
      int color) {
    guiGraphics.drawString(font, formattedCharSequence, x, y, color, false);
  }

  public static void drawConfigString(
      GuiGraphics guiGraphics, Font font, String translationKey, int x, int y) {
    drawConfigString(guiGraphics, font, translationKey, x, y, Constants.FONT_COLOR_DEFAULT);
  }

  public static void drawConfigString(
      GuiGraphics guiGraphics, Font font, String translationKey, int x, int y, int color) {
    drawString(
        guiGraphics,
        font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + translationKey),
        x,
        y,
        color);
  }

  public static int drawConfigStringShadow(
      GuiGraphics guiGraphics, Font font, String translationKey, int x, int y, int color) {
    return drawStringShadow(
        guiGraphics,
        font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + translationKey),
        x,
        y,
        color);
  }

  public static void drawConfigStringShadowWithData(
      GuiGraphics guiGraphics,
      Font font,
      String translationKey,
      Object data,
      int x,
      int y,
      int color) {
    drawStringShadow(
        guiGraphics,
        font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + translationKey, data),
        x,
        y,
        color);
  }
}
