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
import net.minecraft.client.gui.GuiComponent;
import net.minecraft.client.renderer.GameRenderer;
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
    super(font, x + SPRITE_WIDTH, y, width, height);
    this.setCursorPosition(15);
    this.setHighlightPos(15);
  }

  @Override
  public void renderButton(PoseStack poseStack, int left, int top, float partialTicks) {
    super.renderButton(poseStack, left, top, partialTicks);

    // Expand Search Field Background and Border
    int stylePositionX = this.x - SPRITE_WIDTH;
    GuiComponent.fill(
        poseStack,
        stylePositionX - 1,
        this.y - 1,
        stylePositionX + 16,
        this.y + this.height + 1,
        isFocused() ? 0xFFFFFFFF : 0xFFA0A0A0);
    GuiComponent.fill(
        poseStack, stylePositionX, this.y, stylePositionX + 16, this.y + this.height, 0xFF000000);

    // Button Sprite
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, SPRITE);
    blit(
        poseStack,
        stylePositionX + SPRITE_X,
        this.y + SPRITE_Y,
        SPRITE_OFFSET_X,
        isFocused() ? SPRITE_OFFSET_Y : SPRITE_OFFSET_Y + SPRITE_HEIGHT,
        SPRITE_WIDTH,
        SPRITE_HEIGHT);
  }
}
