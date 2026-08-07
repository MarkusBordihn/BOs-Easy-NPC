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

package de.markusbordihn.easynpc.data.model;

public record ModelAnimationTransition(
    ModelAnimationSwitchTiming timing, float blendDurationTicks) {

  public static final float DEFAULT_BLEND_DURATION_TICKS = 5.0F;
  public static final ModelAnimationTransition DEFAULT =
      new ModelAnimationTransition(
          ModelAnimationSwitchTiming.IMMEDIATE, DEFAULT_BLEND_DURATION_TICKS);

  public ModelAnimationTransition {
    timing = timing == null ? ModelAnimationSwitchTiming.IMMEDIATE : timing;
    if (!Float.isFinite(blendDurationTicks) || blendDurationTicks < 0.0F) {
      blendDurationTicks = DEFAULT_BLEND_DURATION_TICKS;
    }
  }

  public static ModelAnimationTransition immediate(float blendDurationTicks) {
    return new ModelAnimationTransition(ModelAnimationSwitchTiming.IMMEDIATE, blendDurationTicks);
  }

  public static ModelAnimationTransition afterCurrent(float blendDurationTicks) {
    return new ModelAnimationTransition(
        ModelAnimationSwitchTiming.AFTER_CURRENT, blendDurationTicks);
  }
}
