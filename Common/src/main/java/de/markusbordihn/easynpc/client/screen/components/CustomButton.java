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
import net.minecraft.network.chat.Component;
import net.minecraft.util.Mth;

public class CustomButton extends Button {

  public CustomButton(int left, int top, int width, int height) {
    this(left, top, width, height, null, unused -> {});
  }

  public CustomButton(int left, int top, int width, int height, OnPress onPress) {
    this(left, top, width, height, null, onPress);
  }

  public CustomButton(
      int left, int top, int width, int height, Component component, OnPress onPress) {
    super(left, top, width, height, component != null ? component : Component.literal(""), onPress);
  }

  public boolean isHovered() {
    return this.isHovered;
  }

  public void renderButtonText(PoseStack poseStack, Font font, Component component, int x, int y) {
    if (component != null && !component.getString().isEmpty()) {
      int fgColor = this.active ? Constants.FONT_COLOR_WHITE : Constants.FONT_COLOR_LIGHT_GRAY;
      drawCenteredString(
          poseStack,
          font,
          component,
          this.x + (this.width) / 2,
          this.y + (this.height - 8) / 2,
          fgColor | Mth.ceil(this.alpha * 255.0F) << 24);
    }
  }

  @Override
  public void renderButton(PoseStack poseStack, int left, int top, float partialTicks) {
    Minecraft minecraft = Minecraft.getInstance();
    Font font = minecraft.font;
    int i = this.getYImage(this.isHoveredOrFocused());
    RenderSystem.enableBlend();
    RenderSystem.defaultBlendFunc();
    RenderSystem.enableDepthTest();

    // Top Part
    Graphics.blit(
        poseStack,
        WIDGETS_LOCATION,
        this.x,
        this.y,
        0,
        46 + i * 20,
        this.width / 2,
        this.height - 4);
    Graphics.blit(
        poseStack,
        WIDGETS_LOCATION,
        this.x + this.width / 2,
        this.y,
        200 - this.width / 2,
        46 + i * 20,
        this.width / 2,
        this.height - 4);

    // Bottom Part (last only 4 pixel from the bottom)
    Graphics.blit(
        poseStack,
        WIDGETS_LOCATION,
        this.x,
        this.y + this.height - 4,
        0,
        46 + i * 20 + 20 - 4,
        this.width / 2,
        4);
    Graphics.blit(
        poseStack,
        WIDGETS_LOCATION,
        this.x + this.width / 2,
        this.y + this.height - 4,
        200 - this.width / 2,
        46 + i * 20 + 20 - 4,
        this.width / 2,
        4);

    // Additional Background
    this.renderBg(poseStack, minecraft, left, top);

    // Button Text
    this.renderButtonText(poseStack, font, this.getMessage(), this.x, this.y);

    // Render tooltip
    if (this.onTooltip != NO_TOOLTIP && this.isHoveredOrFocused()) {
      this.renderToolTip(poseStack, left, top);
    }
  }
}
