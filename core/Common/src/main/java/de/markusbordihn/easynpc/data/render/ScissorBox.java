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

package de.markusbordihn.easynpc.data.render;

public record ScissorBox(
    float scaleMultiplier, Integer width, Integer height, Integer left, Integer top) {

  public static final ScissorBox NONE = null;

  public static final ScissorBox LARGE = new ScissorBox(3.0f, null, null, null, null);

  public static final ScissorBox EXTRA_LARGE = new ScissorBox(5.0f, null, null, null, null);

  public ScissorBox(float scaleMultiplier) {
    this(scaleMultiplier, null, null, null, null);
  }

  public static ScissorBox of(float multiplier) {
    return new ScissorBox(multiplier);
  }

  public static ScissorBox of(int width, int height, int left, int top) {
    return new ScissorBox(1.0f, width, height, left, top);
  }

  public boolean hasCustomScissor() {
    return width != null && height != null && left != null && top != null;
  }
}
