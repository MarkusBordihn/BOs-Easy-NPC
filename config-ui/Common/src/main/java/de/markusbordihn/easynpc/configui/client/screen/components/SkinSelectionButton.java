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

public class SkinSelectionButton extends SpriteButton {

  private static final int DEFAULT_HEIGHT = 84;
  private static final int DEFAULT_WIDTH = 60;
  private static final int SPRITE_HEIGHT = 84;
  private static final int SPRITE_OFFSET_X = 0;
  private static final int SPRITE_OFFSET_Y = 0;
  private static final int SPRITE_WIDTH = 60;
  private static final int SPRITE_X = 0;
  private static final int SPRITE_Y = 0;
  private static final ResourceLocation TEXTURE = Constants.TEXTURE_CONFIGURATION;

  public SkinSelectionButton(int left, int top, OnPress onPress) {
    super(
        left,
        top,
        DEFAULT_WIDTH,
        DEFAULT_HEIGHT,
        TEXTURE,
        SPRITE_X,
        SPRITE_Y,
        SPRITE_OFFSET_X,
        SPRITE_OFFSET_Y,
        SPRITE_WIDTH,
        SPRITE_HEIGHT,
        onPress);
    this.setRenderBackground(false);
  }
}
