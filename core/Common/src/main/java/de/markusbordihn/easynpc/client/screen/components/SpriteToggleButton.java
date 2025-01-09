/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.client.screen.components;

import net.minecraft.client.Minecraft;

public class SpriteToggleButton extends SpriteButton {

  public SpriteToggleButton(
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
    super(
        left,
        top,
        width,
        height,
        label,
        spriteX,
        spriteY,
        spriteOffsetX,
        spriteOffsetY,
        spriteWidth,
        spriteHeight,
        onPress);
  }

  @Override
  public boolean mouseClicked(double x, double y, int button) {
    if (!this.visible) {
      return false;
    }
    if (this.isValidClickButton(button) && this.clicked(x, y)) {
      this.playDownSound(Minecraft.getInstance().getSoundManager());
      this.active = !this.active;
      this.onClick(x, y);
      return true;
    }
    return false;
  }

  @Override
  protected boolean clicked(double x, double y) {
    return this.visible
        && x >= this.x
        && y >= this.y
        && x < (this.x + this.width)
        && y < (this.y + this.height);
  }

  @Override
  public boolean changeFocus(boolean focus) {
    if (!this.visible) {
      return false;
    }
    this.setFocused(true);
    this.onFocusedChanged(true);
    return true;
  }

  @Override
  public boolean isMouseOver(double x, double y) {
    return this.visible
        && x >= this.x
        && y >= this.y
        && x < (this.x + this.width)
        && y < (this.y + this.height);
  }
}
