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

public class ReloadButton extends SpriteButton {

  public static final int DEFAULT_HEIGHT = 18;
  public static final int SPRITE_HEIGHT = 12;
  public static final int SPRITE_OFFSET_X = 92;
  public static final int SPRITE_OFFSET_Y = 4;
  public static final int SPRITE_WIDTH = 12;
  public static final int SPRITE_X = 3;
  public static final int SPRITE_Y = 3;

  public ReloadButton(int left, int top, int width, int height, String label, OnPress onPress) {
    super(
        left,
        top,
        width,
        height,
        label,
        SPRITE_X,
        height <= 16 ? 2 : SPRITE_Y,
        SPRITE_OFFSET_X,
        SPRITE_OFFSET_Y,
        SPRITE_WIDTH,
        SPRITE_HEIGHT,
        onPress);
  }

  public ReloadButton(int left, int top, int width, String label, OnPress onPress) {
    this(left, top, width, DEFAULT_HEIGHT, label, onPress);
  }
}
