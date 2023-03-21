/**
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

import com.mojang.blaze3d.platform.GlStateManager;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.components.AbstractButton;
import net.minecraft.client.gui.narration.NarratedElementType;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;

@OnlyIn(Dist.CLIENT)
public class Checkbox extends AbstractButton {
  private static final ResourceLocation TEXTURE =
      new ResourceLocation(Constants.MOD_ID, "textures/gui/checkbox.png");

  protected final Checkbox.OnChange onChange;

  private boolean selected;
  private final boolean showLabel;
  private final Minecraft minecraft;
  private final Font font;

  public Checkbox(int left, int top, String label, boolean selected, Checkbox.OnChange onChange) {
    this(left, top, new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + label), selected, true,
        onChange);
  }

  public Checkbox(int left, int top, String label, boolean selected) {
    this(left, top, new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + label), selected,
        true);
  }

  public Checkbox(int left, int top, Component component, boolean selected, boolean showLabel) {
    this(left, top, component, selected, showLabel, null);
  }

  public Checkbox(int left, int top, Component component, boolean selected, boolean showLabel,
      Checkbox.OnChange onChange) {
    super(left, top, 16, 16, component);
    this.selected = selected;
    this.showLabel = showLabel;
    this.minecraft = Minecraft.getInstance();
    this.font = this.minecraft.font;
    this.onChange = onChange;
  }

  public void onPress() {
    this.selected = !this.selected;
    this.onChange.onChange(this);
  }

  public boolean selected() {
    return this.selected;
  }

  public void updateNarration(NarrationElementOutput narrationElementOutput) {
    narrationElementOutput.add(NarratedElementType.TITLE, this.createNarrationMessage());
    if (this.active) {
      if (this.isFocused()) {
        narrationElementOutput.add(NarratedElementType.USAGE,
            new TranslatableComponent("narration.checkbox.usage.focused"));
      } else {
        narrationElementOutput.add(NarratedElementType.USAGE,
            new TranslatableComponent("narration.checkbox.usage.hovered"));
      }
    }
  }

  @OnlyIn(Dist.CLIENT)
  public interface OnChange {
    void onChange(Checkbox checkbox);
  }

  @Override
  public void renderButton(PoseStack poseStack, int p_93844_, int p_93845_, float p_93846_) {
    RenderSystem.setShaderTexture(0, TEXTURE);
    RenderSystem.enableDepthTest();
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, this.alpha);
    RenderSystem.enableBlend();
    RenderSystem.defaultBlendFunc();
    RenderSystem.blendFunc(GlStateManager.SourceFactor.SRC_ALPHA,
        GlStateManager.DestFactor.ONE_MINUS_SRC_ALPHA);
    blit(poseStack, this.x, this.y, this.isHoveredOrFocused() ? 16.0F : 0.0F,
        this.selected ? 16.0F : 0.0F, 16, 16, 32, 32);
    if (this.showLabel) {
      this.font.draw(poseStack, this.getMessage(), this.x + 18f, this.y + (this.height - 8) / 2f,
          Constants.FONT_COLOR_DEFAULT);
    }

  }
}
