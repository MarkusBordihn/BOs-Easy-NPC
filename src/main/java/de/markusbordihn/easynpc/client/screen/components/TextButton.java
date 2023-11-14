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
import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.util.Mth;

public class TextButton extends Button {

  public static final int DEFAULT_HEIGHT = 16;

  public TextButton(int left, int top, int width, String label, Object data, OnPress onPress) {
    this(
        left,
        top,
        width,
        DEFAULT_HEIGHT,
        label != null && !label.isBlank() && Character.isLowerCase(label.codePointAt(0))
            ? new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + label, data)
            : new TextComponent(label != null ? label : ""),
        onPress);
  }

  public TextButton(int left, int top, int width, String label, OnPress onPress) {
    this(
        left,
        top,
        width,
        DEFAULT_HEIGHT,
        label != null && !label.isBlank() && Character.isLowerCase(label.codePointAt(0))
            ? new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + label)
            : new TextComponent(label != null ? label : ""),
        onPress);
  }

  public TextButton(int left, int top, int width, int height, String label, OnPress onPress) {
    this(
        left,
        top,
        width,
        height,
        label != null && !label.isBlank() && Character.isLowerCase(label.codePointAt(0))
            ? new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + label)
            : new TextComponent(label != null ? label : ""),
        onPress);
  }

  public TextButton(int left, int top, int width, Component label, OnPress onPress) {
    this(left, top, width, DEFAULT_HEIGHT, label, onPress);
  }

  public TextButton(int left, int top, int width, int height, Component label, OnPress onPress) {
    super(left, top, width, height, label, onPress);
  }

  @Override
  public void renderButton(PoseStack poseStack, int left, int top, float partialTicks) {
    Minecraft minecraft = Minecraft.getInstance();
    Font font = minecraft.font;
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderTexture(0, WIDGETS_LOCATION);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, this.alpha);
    int i = this.getYImage(this.isHoveredOrFocused());
    RenderSystem.enableBlend();
    RenderSystem.defaultBlendFunc();
    RenderSystem.enableDepthTest();

    // Top Part
    this.blit(poseStack, this.x, this.y, 0, 46 + i * 20, this.width / 2, this.height - 4);
    this.blit(
        poseStack,
        this.x + this.width / 2,
        this.y,
        200 - this.width / 2,
        46 + i * 20,
        this.width / 2,
        this.height - 4);

    // Bottom Part (last only 4 pixel from the bottom)
    this.blit(
        poseStack, this.x, this.y + this.height - 4, 0, 46 + i * 20 + 20 - 4, this.width / 2, 4);
    this.blit(
        poseStack,
        this.x + this.width / 2,
        this.y + this.height - 4,
        200 - this.width / 2,
        46 + i * 20 + 20 - 4,
        this.width / 2,
        4);

    this.renderBg(poseStack, minecraft, left, top);
    int j = getFGColor();
    drawCenteredString(
        poseStack,
        font,
        this.getMessage(),
        this.x + this.width / 2,
        this.y + (this.height - 8) / 2,
        j | Mth.ceil(this.alpha * 255.0F) << 24);

    if (this.isHoveredOrFocused()) {
      this.renderToolTip(poseStack, left, top);
    }
  }
}
