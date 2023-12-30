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

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import net.minecraft.client.gui.Font;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;

public class Text {

  protected Text() {
  }

  public static void drawString(PoseStack poseStack, Font font, String text, int x, int y) {
    drawString(poseStack, font, Component.literal(text), x, y);
  }

  public static void drawString(
      PoseStack poseStack, Font font, String text, int x, int y, int color) {
    drawString(poseStack, font, Component.literal(text), x, y, color);
  }

  public static int drawString(PoseStack poseStack, Font font, Component component, int x, int y) {
    return drawString(poseStack, font, component, x, y, Constants.FONT_COLOR_DEFAULT);
  }

  public static int drawString(
      PoseStack poseStack, Font font, Component component, int x, int y, int color) {
    return font.draw(poseStack, component, x, y, color);
  }

  public static void drawStringShadow(
      PoseStack poseStack, Font font, String text, int x, int y, int color) {
    font.drawShadow(poseStack, text, x, y, color, false);
  }

  public static int drawStringShadow(
      PoseStack poseStack, Font font, Component component, int x, int y) {
    return drawStringShadow(poseStack, font, component, x, y, Constants.FONT_COLOR_DEFAULT);
  }

  public static int drawStringShadow(
      PoseStack poseStack, Font font, Component component, int x, int y, int color) {
    return font.drawShadow(poseStack, component, x, y, color);
  }

  public static void drawString(
      PoseStack poseStack, Font font, FormattedCharSequence formattedCharSequence, int x, int y) {
    drawString(poseStack, font, formattedCharSequence, x, y, Constants.FONT_COLOR_DEFAULT);
  }

  public static int drawString(
      PoseStack poseStack,
      Font font,
      FormattedCharSequence formattedCharSequence,
      int x,
      int y,
      int color) {
    return font.draw(poseStack, formattedCharSequence, x, y, color);
  }

  public static void drawConfigString(
      PoseStack poseStack, Font font, String translationKey, int x, int y) {
    drawConfigString(poseStack, font, translationKey, x, y, Constants.FONT_COLOR_DEFAULT);
  }

  public static int drawConfigString(
      PoseStack poseStack, Font font, String translationKey, int x, int y, int color) {
    return drawString(
        poseStack,
        font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + translationKey),
        x,
        y,
        color);
  }

  public static int drawConfigStringShadow(
      PoseStack poseStack, Font font, String translationKey, int x, int y) {
    return drawConfigStringShadow(
        poseStack, font, translationKey, x, y, Constants.FONT_COLOR_DEFAULT);
  }

  public static int drawConfigStringShadow(
      PoseStack poseStack, Font font, String translationKey, int x, int y, int color) {
    return drawStringShadow(
        poseStack,
        font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + translationKey),
        x,
        y,
        color);
  }

  public static int drawConfigStringWithData(
      PoseStack poseStack, Font font, String translationKey, Object data, int x, int y, int color) {
    return drawString(
        poseStack,
        font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + translationKey, data),
        x,
        y,
        color);
  }

  public static void drawConfigStringShadowWithData(
      PoseStack poseStack, Font font, String translationKey, Object data, int x, int y, int color) {
    drawStringShadow(
        poseStack,
        font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + translationKey, data),
        x,
        y,
        color);
  }
}
