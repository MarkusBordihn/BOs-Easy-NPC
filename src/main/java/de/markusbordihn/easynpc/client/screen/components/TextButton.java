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

import com.mojang.blaze3d.systems.RenderSystem;
import de.markusbordihn.easynpc.Constants;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.util.Mth;

public class TextButton extends CustomButton {

  public static final int DEFAULT_HEIGHT = 16;

  public TextButton(int left, int top, int width, String label, Object data, OnPress onPress) {
    this(
        left,
        top,
        width,
        DEFAULT_HEIGHT,
        label != null && !label.isBlank() && Character.isLowerCase(label.codePointAt(0))
            ? Component.translatable(Constants.TEXT_CONFIG_PREFIX + label, data)
            : Component.literal(label != null ? label : ""),
        onPress);
  }

  public TextButton(int left, int top, int width, String label, OnPress onPress) {
    this(
        left,
        top,
        width,
        DEFAULT_HEIGHT,
        label != null && !label.isBlank() && Character.isLowerCase(label.codePointAt(0))
            ? Component.translatable(Constants.TEXT_CONFIG_PREFIX + label)
            : Component.literal(label != null ? label : ""),
        onPress);
  }

  public TextButton(int left, int top, int width, int height, String label, OnPress onPress) {
    this(
        left,
        top,
        width,
        height,
        label != null && !label.isBlank() && Character.isLowerCase(label.codePointAt(0))
            ? Component.translatable(Constants.TEXT_CONFIG_PREFIX + label)
            : Component.literal(label != null ? label : ""),
        onPress);
  }

  public TextButton(int left, int top, int width, Component label, OnPress onPress) {
    this(left, top, width, DEFAULT_HEIGHT, label, onPress);
  }

  public TextButton(int left, int top, int width, int height, Component label, OnPress onPress) {
    super(left, top, width, height, label, onPress);
  }

  @Override
  public void renderButton(GuiGraphics guiGraphics, int left, int top, float partialTicks) {
    Minecraft minecraft = Minecraft.getInstance();
    Font font = minecraft.font;
    int i = this.getYImage(this.isHoveredOrFocused());
    RenderSystem.enableBlend();
    RenderSystem.defaultBlendFunc();
    RenderSystem.enableDepthTest();

    // Top Part
    guiGraphics.blit(
        WIDGETS_LOCATION,
        this.getX(),
        this.getY(),
        0,
        46 + i * 20,
        this.width / 2,
        this.height - 4);
    guiGraphics.blit(
        WIDGETS_LOCATION,
        this.getX() + this.width / 2,
        this.getY(),
        200 - this.width / 2,
        46 + i * 20,
        this.width / 2,
        this.height - 4);

    // Bottom Part (last only 4 pixel from the bottom)
    guiGraphics.blit(
        WIDGETS_LOCATION,
        this.getX(),
        this.getY() + this.height - 4,
        0,
        46 + i * 20 + 20 - 4,
        this.width / 2,
        4);
    guiGraphics.blit(
        WIDGETS_LOCATION,
        this.getX() + this.width / 2,
        this.getY() + this.height - 4,
        200 - this.width / 2,
        46 + i * 20 + 20 - 4,
        this.width / 2,
        4);

    int j = getFGColor();
    guiGraphics.drawCenteredString(
        font,
        this.getMessage(),
        this.getX() + this.width / 2,
        this.getY() + (this.height - 8) / 2,
        j | Mth.ceil(this.alpha * 255.0F) << 24);
  }
}
