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

public record ModelAnimationPlayback(
    ModelAnimationPlaybackMode mode, int repeatCount, float durationTicks) {

  public static final int DEFAULT_REPEAT_COUNT = 1;
  public static final float UNLIMITED_DURATION_TICKS = 0.0F;
  public static final ModelAnimationPlayback DEFAULT =
      new ModelAnimationPlayback(
          ModelAnimationPlaybackMode.ONCE, DEFAULT_REPEAT_COUNT, UNLIMITED_DURATION_TICKS);

  public ModelAnimationPlayback {
    mode = mode == null ? ModelAnimationPlaybackMode.ONCE : mode;
    if (mode != ModelAnimationPlaybackMode.REPEAT || repeatCount < DEFAULT_REPEAT_COUNT) {
      repeatCount = DEFAULT_REPEAT_COUNT;
    }
    if (!Float.isFinite(durationTicks) || durationTicks < UNLIMITED_DURATION_TICKS) {
      durationTicks = UNLIMITED_DURATION_TICKS;
    }
  }

  public static ModelAnimationPlayback of(ModelAnimationPlaybackMode mode) {
    return new ModelAnimationPlayback(mode, DEFAULT_REPEAT_COUNT, UNLIMITED_DURATION_TICKS);
  }

  public static ModelAnimationPlayback repeat(int repeatCount) {
    return new ModelAnimationPlayback(
        ModelAnimationPlaybackMode.REPEAT, repeatCount, UNLIMITED_DURATION_TICKS);
  }

  public ModelAnimationPlayback withMode(ModelAnimationPlaybackMode mode) {
    return new ModelAnimationPlayback(mode, this.repeatCount, this.durationTicks);
  }

  public ModelAnimationPlayback withRepeatCount(int repeatCount) {
    return new ModelAnimationPlayback(this.mode, repeatCount, this.durationTicks);
  }

  public ModelAnimationPlayback withDurationTicks(float durationTicks) {
    return new ModelAnimationPlayback(this.mode, this.repeatCount, durationTicks);
  }

  public boolean hasDurationLimit() {
    return this.durationTicks > UNLIMITED_DURATION_TICKS;
  }

  public boolean isSingleRun() {
    return this.mode != ModelAnimationPlaybackMode.LOOP;
  }
}
