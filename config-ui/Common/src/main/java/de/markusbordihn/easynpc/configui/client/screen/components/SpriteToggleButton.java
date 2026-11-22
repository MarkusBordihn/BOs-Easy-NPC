/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.client.screen.components;

import de.markusbordihn.easynpc.client.screen.components.CustomButton;
import de.markusbordihn.easynpc.configui.Constants;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.input.InputWithModifiers;
import net.minecraft.client.renderer.RenderPipelines;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

public class SpriteToggleButton extends CustomButton {

  public static final int SIZE = 18;
  public static final int SPRITE_SIZE = 12;
  private static final int TEXTURE_SIZE = 256;
  private static final int SPRITE_INSET = (SIZE - SPRITE_SIZE) / 2;
  private static final int SELECTED_BORDER_COLOR = 0xFF55FF55;
  private static final int HOVERED_BORDER_COLOR = 0xFFFFFFFF;

  private final int spriteOffsetX;
  private final int selectedSpriteOffsetY;
  private final int unselectedSpriteOffsetY;
  private final Component narration;
  private final SpriteToggleButton.OnChange onChange;
  private boolean selected;

  public SpriteToggleButton(
      int left,
      int top,
      int spriteOffsetX,
      int selectedSpriteOffsetY,
      Component narration,
      boolean selected,
      SpriteToggleButton.OnChange onChange) {
    super(left, top, SIZE, SIZE, Component.empty(), unused -> {});
    this.spriteOffsetX = spriteOffsetX;
    this.selectedSpriteOffsetY = selectedSpriteOffsetY;
    this.unselectedSpriteOffsetY = selectedSpriteOffsetY + SPRITE_SIZE;
    this.narration = narration;
    this.selected = selected;
    this.onChange = onChange;
  }

  @Override
  protected MutableComponent createNarrationMessage() {
    return wrapDefaultNarrationMessage(this.narration);
  }

  @Override
  public void onPress(InputWithModifiers input) {
    this.selected = !this.selected;
    if (this.onChange != null) {
      this.onChange.onChange(this);
    }
  }

  public boolean selected() {
    return this.selected;
  }

  public void setSelected(boolean selected) {
    this.selected = selected;
  }

  @Override
  public void renderButton(
      GuiGraphicsExtractor guiGraphics, int left, int top, float partialTicks) {
    super.renderButton(guiGraphics, left, top, partialTicks);
    guiGraphics.blit(
        RenderPipelines.GUI_TEXTURED,
        Constants.TEXTURE_CONFIGURATION,
        this.getX() + SPRITE_INSET,
        this.getY() + SPRITE_INSET,
        this.spriteOffsetX,
        this.selected ? this.selectedSpriteOffsetY : this.unselectedSpriteOffsetY,
        SPRITE_SIZE,
        SPRITE_SIZE,
        TEXTURE_SIZE,
        TEXTURE_SIZE);

    if (this.isHoveredOrFocused()) {
      guiGraphics.outline(this.getX(), this.getY(), this.width, this.height, HOVERED_BORDER_COLOR);
    } else if (this.selected) {
      guiGraphics.outline(this.getX(), this.getY(), this.width, this.height, SELECTED_BORDER_COLOR);
    }
  }

  public interface OnChange {
    void onChange(SpriteToggleButton spriteToggleButton);
  }
}
