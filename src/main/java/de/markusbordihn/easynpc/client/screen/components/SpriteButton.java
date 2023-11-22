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
import net.minecraft.client.gui.Font;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;

public class SpriteButton extends CustomButton {

  public static final int DEFAULT_HEIGHT = 16;
  public static final ResourceLocation DEFAULT_SPRITE = Constants.TEXTURE_CONFIGURATION;

  private final ResourceLocation sprite;
  private final int spriteX;
  private final int spriteY;
  private final int spriteOffsetX;
  private final int spriteOffsetY;
  private final int spriteWidth;
  private final int spriteHeight;
  private boolean renderBackground = true;

  public SpriteButton(
      int left,
      int top,
      int width,
      ResourceLocation sprite,
      int spriteOffsetX,
      int spriteOffsetY,
      OnPress onPress) {
    this(left, top, width, sprite, 0, 0, spriteOffsetX, spriteOffsetY, 16, 16, onPress);
  }

  public SpriteButton(
      int left,
      int top,
      int width,
      int spriteX,
      int spriteY,
      int spriteOffsetX,
      int spriteOffsetY,
      int spriteWidth,
      int spriteHeight,
      OnPress onPress) {
    this(
        left,
        top,
        width,
        DEFAULT_HEIGHT,
        DEFAULT_SPRITE,
        spriteX,
        spriteY,
        spriteOffsetX,
        spriteOffsetY,
        spriteWidth,
        spriteHeight,
        onPress);
  }

  public SpriteButton(
      int left,
      int top,
      int width,
      ResourceLocation sprite,
      int spriteX,
      int spriteY,
      int spriteOffsetX,
      int spriteOffsetY,
      int spriteWidth,
      int spriteHeight,
      OnPress onPress) {
    this(
        left,
        top,
        width,
        DEFAULT_HEIGHT,
        sprite,
        spriteX,
        spriteY,
        spriteOffsetX,
        spriteOffsetY,
        spriteWidth,
        spriteHeight,
        onPress);
  }

  public SpriteButton(
      int left,
      int top,
      int width,
      int height,
      ResourceLocation sprite,
      int spriteX,
      int spriteY,
      int spriteOffsetX,
      int spriteOffsetY,
      int spriteWidth,
      int spriteHeight,
      OnPress onPress) {
    this(
        left,
        top,
        width,
        height,
        Component.literal(""),
        sprite,
        spriteX,
        spriteY,
        spriteOffsetX,
        spriteOffsetY,
        spriteWidth,
        spriteHeight,
        onPress);
  }

  public SpriteButton(
      int left,
      int top,
      int width,
      int height,
      int spriteX,
      int spriteY,
      int spriteOffsetX,
      int spriteOffsetY,
      int spriteWidth,
      int spriteHeight,
      OnPress onPress) {
    this(
        left,
        top,
        width,
        height,
        Component.literal(""),
        DEFAULT_SPRITE,
        spriteX,
        spriteY,
        spriteOffsetX,
        spriteOffsetY,
        spriteWidth,
        spriteHeight,
        onPress);
  }

  public SpriteButton(
      int left,
      int top,
      int width,
      int height,
      ResourceLocation sprite,
      int spriteOffsetX,
      int spriteOffsetY,
      int spriteWidth,
      int spriteHeight,
      OnPress onPress) {
    this(
        left,
        top,
        width,
        height,
        "",
        sprite,
        0,
        0,
        spriteOffsetX,
        spriteOffsetY,
        spriteWidth,
        spriteHeight,
        onPress);
  }

  public SpriteButton(
      int left,
      int top,
      int width,
      int height,
      String label,
      int spriteX,
      int spriteY,
      int spriteOffsetX,
      int spriteOffsetY,
      int spriteWidth,
      int spriteHeight,
      OnPress onPress) {
    this(
        left,
        top,
        width,
        height,
        label != null && !label.isBlank() && Character.isLowerCase(label.codePointAt(0))
            ? Component.translatable(Constants.TEXT_CONFIG_PREFIX + label)
            : Component.literal(label != null ? label : ""),
        DEFAULT_SPRITE,
        spriteX,
        spriteY,
        spriteOffsetX,
        spriteOffsetY,
        spriteWidth,
        spriteHeight,
        onPress);
  }

  public SpriteButton(
      int left,
      int top,
      int width,
      int height,
      String label,
      ResourceLocation sprite,
      int spriteX,
      int spriteY,
      int spriteOffsetX,
      int spriteOffsetY,
      int spriteWidth,
      int spriteHeight,
      OnPress onPress) {
    this(
        left,
        top,
        width,
        height,
        label != null && !label.isBlank() && Character.isLowerCase(label.codePointAt(0))
            ? Component.translatable(Constants.TEXT_CONFIG_PREFIX + label)
            : Component.literal(label != null ? label : ""),
        sprite,
        spriteX,
        spriteY,
        spriteOffsetX,
        spriteOffsetY,
        spriteWidth,
        spriteHeight,
        onPress);
  }

  public SpriteButton(
      int left,
      int top,
      int width,
      int height,
      Component text,
      ResourceLocation sprite,
      int spriteX,
      int spriteY,
      int spriteOffsetX,
      int spriteOffsetY,
      int spriteWidth,
      int spriteHeight,
      OnPress onPress) {
    super(left, top, width, height, text != null ? text : Component.literal(""), onPress);
    this.sprite = sprite;
    this.spriteX = spriteX;
    this.spriteY = spriteY;
    this.spriteOffsetX = spriteOffsetX;
    this.spriteOffsetY = spriteOffsetY;
    this.spriteWidth = spriteWidth;
    this.spriteHeight = spriteHeight;
  }

  public void setRenderBackground(boolean renderBackground) {
    this.renderBackground = renderBackground;
  }

  @Override
  public void renderButtonText(PoseStack poseStack, Font font, Component component, int x, int y) {
    if (component != null && !component.getString().isEmpty()) {
      int fgColor = getFGColor();
      drawCenteredString(
          poseStack,
          font,
          component,
          this.getX() + (this.width + this.spriteWidth) / 2,
          this.getY() + (this.height - 8) / 2,
          fgColor | Mth.ceil(this.alpha * 255.0F) << 24);
    }
  }

  @Override
  public void renderButton(PoseStack poseStack, int left, int top, float partialTicks) {
    if (this.renderBackground) {
      super.renderButton(poseStack, left, top, partialTicks);
    }

    // Button Sprite
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, this.sprite);
    blit(
        poseStack,
        this.getX() + this.spriteX,
        this.getY() + this.spriteY,
        spriteOffsetX,
        isActive() ? this.spriteOffsetY : spriteOffsetY + spriteHeight,
        spriteWidth,
        spriteHeight);
  }
}
