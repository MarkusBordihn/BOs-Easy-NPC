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
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;

public class Text {

  public static int drawString(GuiGraphics guiGraphics, Font font, String text, int x, int y) {
    return guiGraphics.drawString(
        font, Component.literal(text), x, y, Constants.FONT_COLOR_DEFAULT, false);
  }

  public static int drawString(
      GuiGraphics guiGraphics, Font font, String text, int x, int y, int color) {
    return guiGraphics.drawString(font, Component.literal(text), x, y, color, false);
  }

  public static int drawString(
      GuiGraphics guiGraphics, Font font, Component component, int x, int y) {
    return guiGraphics.drawString(font, component, x, y, Constants.FONT_COLOR_DEFAULT, false);
  }

  public static int drawString(
      GuiGraphics guiGraphics, Font font, Component component, int x, int y, int color) {
    return guiGraphics.drawString(font, component, x, y, color, false);
  }

  public static int drawStringShadow(
      GuiGraphics guiGraphics, Font font, String text, int x, int y, int color) {
    return guiGraphics.drawString(font, Component.literal(text), x, y, color, false);
  }

  public static int drawStringShadow(
      GuiGraphics guiGraphics, Font font, Component component, int x, int y) {
    return guiGraphics.drawString(font, component, x, y, Constants.FONT_COLOR_DEFAULT, false);
  }

  public static int drawStringShadow(
      GuiGraphics guiGraphics, Font font, Component component, int x, int y, int color) {
    return guiGraphics.drawString(font, component, x, y, color, true);
  }

  public static int drawString(
      GuiGraphics guiGraphics,
      Font font,
      FormattedCharSequence formattedCharSequence,
      int x,
      int y) {
    return guiGraphics.drawString(
        font, formattedCharSequence, x, y, Constants.FONT_COLOR_DEFAULT, false);
  }

  public static int drawString(
      GuiGraphics guiGraphics,
      Font font,
      FormattedCharSequence formattedCharSequence,
      int x,
      int y,
      int color) {
    return guiGraphics.drawString(font, formattedCharSequence, x, y, color, false);
  }

  public static int drawConfigString(
      GuiGraphics guiGraphics, Font font, String translationKey, int x, int y) {
    return drawConfigString(guiGraphics, font, translationKey, x, y, Constants.FONT_COLOR_DEFAULT);
  }

  public static int drawConfigString(
      GuiGraphics guiGraphics, Font font, String translationKey, int x, int y, int color) {
    return drawString(
        guiGraphics,
        font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + translationKey),
        x,
        y,
        color);
  }

  public static int drawConfigStringShadow(
      GuiGraphics guiGraphics, Font font, String translationKey, int x, int y) {
    return drawConfigStringShadow(
        guiGraphics, font, translationKey, x, y, Constants.FONT_COLOR_DEFAULT);
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

  public static int drawConfigStringWithData(
      GuiGraphics guiGraphics,
      Font font,
      String translationKey,
      Object data,
      int x,
      int y,
      int color) {
    return drawString(
        guiGraphics,
        font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + translationKey, data),
        x,
        y,
        color);
  }

  public static int drawConfigStringShadowWithData(
      GuiGraphics guiGraphics,
      Font font,
      String translationKey,
      Object data,
      int x,
      int y,
      int color) {
    return drawStringShadow(
        guiGraphics,
        font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + translationKey, data),
        x,
        y,
        color);
  }
}
