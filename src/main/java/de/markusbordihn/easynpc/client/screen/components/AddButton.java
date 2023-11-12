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

import de.markusbordihn.easynpc.Constants;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;

public class AddButton extends SpriteButton {

  public static final int DEFAULT_WIDTH = 100;
  public static final int DEFAULT_HEIGHT = 18;
  public static final ResourceLocation SPRITE = Constants.TEXTURE_CONFIGURATION;
  public static final int SPRITE_X = 4;
  public static final int SPRITE_Y = 3;
  public static final int SPRITE_OFFSET_X = 65;
  public static final int SPRITE_OFFSET_Y = 104;
  public static final int SPRITE_WIDTH = 12;
  public static final int SPRITE_HEIGHT = 12;

  public AddButton(int left, int top, OnPress onPress) {
    super(left, top, 20, 18, Constants.TEXTURE_CONFIGURATION, 4, 3, 64, 4, 13, 13, onPress);
  }

  public AddButton(int left, int top, String label, OnPress onPress) {
    super(
        left,
        top,
        DEFAULT_WIDTH,
        18,
        label,
        SPRITE,
        SPRITE_X,
        SPRITE_Y,
        SPRITE_OFFSET_X,
        SPRITE_OFFSET_Y,
        SPRITE_WIDTH,
        SPRITE_HEIGHT,
        onPress);
  }

  public AddButton(int left, int top, int width, String label, OnPress onPress) {
    super(
        left,
        top,
        width,
        DEFAULT_HEIGHT,
        label,
        SPRITE,
        SPRITE_X,
        3,
        SPRITE_OFFSET_X,
        SPRITE_OFFSET_Y,
        SPRITE_WIDTH,
        SPRITE_HEIGHT,
        onPress);
  }

  public AddButton(int left, int top, int width, Component component, OnPress onPress) {
    super(
        left,
        top,
        width,
        DEFAULT_HEIGHT,
        component,
        SPRITE,
        SPRITE_X,
        SPRITE_Y,
        SPRITE_OFFSET_X,
        SPRITE_OFFSET_Y,
        SPRITE_WIDTH,
        SPRITE_HEIGHT,
        onPress);
  }
}
