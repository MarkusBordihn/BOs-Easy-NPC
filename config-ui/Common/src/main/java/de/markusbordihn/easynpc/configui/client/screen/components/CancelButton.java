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

package de.markusbordihn.easynpc.configui.client.screen.components;

import de.markusbordihn.easynpc.client.screen.components.SpriteButton;
import de.markusbordihn.easynpc.configui.Constants;
import net.minecraft.client.gui.components.Button.OnPress;
import net.minecraft.resources.ResourceLocation;

public class CancelButton extends SpriteButton {

  private static final int DEFAULT_HEIGHT = 18;
  private static final int DEFAULT_WIDTH = 100;
  private static final int SPRITE_HEIGHT = 10;
  private static final int SPRITE_OFFSET_X = 64;
  private static final int SPRITE_OFFSET_Y = 47;
  private static final int SPRITE_WIDTH = 10;
  private static final int SPRITE_X = 4;
  private static final int SPRITE_Y = 3;
  private static final ResourceLocation TEXTURE = Constants.TEXTURE_CONFIGURATION;

  public CancelButton(int left, int top, String label, OnPress onPress) {
    this(left, top, DEFAULT_WIDTH, label, onPress);
  }

  public CancelButton(int left, int top, int width, String label, OnPress onPress) {
    super(
        left,
        top,
        width,
        DEFAULT_HEIGHT,
        label,
        TEXTURE,
        SPRITE_X,
        3,
        SPRITE_OFFSET_X,
        SPRITE_OFFSET_Y,
        SPRITE_WIDTH,
        SPRITE_HEIGHT,
        onPress);
  }
}
