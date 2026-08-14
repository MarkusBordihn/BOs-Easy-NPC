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

package de.markusbordihn.easynpc.data.action;

import de.markusbordihn.easynpc.data.model.ModelAnimationPlayback;
import de.markusbordihn.easynpc.data.model.ModelAnimationPlaybackMode;
import de.markusbordihn.easynpc.data.model.ModelAnimationSwitchTiming;
import de.markusbordihn.easynpc.data.model.ModelAnimationTransition;
import java.util.Locale;
import net.minecraft.nbt.CompoundTag;

public record ModelAnimationActionData(
    String animationName, ModelAnimationPlayback playback, ModelAnimationTransition transition) {

  public static final ModelAnimationActionData DEFAULT =
      new ModelAnimationActionData(
          "", ModelAnimationPlayback.DEFAULT, ModelAnimationTransition.DEFAULT);
  public static final String DATA_NAME_TAG = "Name";
  public static final String DATA_LOOP_TAG = "Loop";
  public static final String DATA_REPEAT_TAG = "Repeat";
  public static final String DATA_DURATION_TAG = "Duration";
  public static final String DATA_AFTER_CURRENT_TAG = "After";
  public static final String DATA_BLEND_TAG = "Blend";

  public ModelAnimationActionData {
    animationName = animationName == null ? "" : animationName.trim().toLowerCase(Locale.ROOT);
    playback = playback == null ? ModelAnimationPlayback.DEFAULT : playback;
    transition = transition == null ? ModelAnimationTransition.DEFAULT : transition;
  }

  public ModelAnimationActionData(String animationName) {
    this(animationName, ModelAnimationPlayback.DEFAULT, ModelAnimationTransition.DEFAULT);
  }

  public static ModelAnimationActionData fromTag(CompoundTag compoundTag) {
    if (compoundTag == null || compoundTag.isEmpty()) {
      return DEFAULT;
    }

    ModelAnimationSwitchTiming timing =
        compoundTag.getBoolean(DATA_AFTER_CURRENT_TAG)
            ? ModelAnimationSwitchTiming.AFTER_CURRENT
            : ModelAnimationSwitchTiming.IMMEDIATE;
    float blend =
        compoundTag.contains(DATA_BLEND_TAG)
            ? compoundTag.getFloat(DATA_BLEND_TAG)
            : ModelAnimationTransition.DEFAULT_BLEND_DURATION_TICKS;
    return new ModelAnimationActionData(
        compoundTag.getString(DATA_NAME_TAG),
        playbackFromTag(compoundTag),
        new ModelAnimationTransition(timing, blend));
  }

  private static ModelAnimationPlayback playbackFromTag(CompoundTag compoundTag) {
    float durationTicks = compoundTag.getFloat(DATA_DURATION_TAG);
    if (compoundTag.contains(DATA_REPEAT_TAG)) {
      return new ModelAnimationPlayback(
          ModelAnimationPlaybackMode.REPEAT, compoundTag.getInt(DATA_REPEAT_TAG), durationTicks);
    }

    return new ModelAnimationPlayback(
        compoundTag.getBoolean(DATA_LOOP_TAG)
            ? ModelAnimationPlaybackMode.LOOP
            : ModelAnimationPlaybackMode.ONCE,
        ModelAnimationPlayback.DEFAULT_REPEAT_COUNT,
        durationTicks);
  }

  public CompoundTag createTag() {
    CompoundTag compoundTag = new CompoundTag();
    if (!this.animationName.isEmpty()) {
      compoundTag.putString(DATA_NAME_TAG, this.animationName);
    }
    if (this.playback.mode() == ModelAnimationPlaybackMode.LOOP) {
      compoundTag.putBoolean(DATA_LOOP_TAG, true);
    }
    if (this.playback.mode() == ModelAnimationPlaybackMode.REPEAT) {
      compoundTag.putInt(DATA_REPEAT_TAG, this.playback.repeatCount());
    }
    if (this.playback.hasDurationLimit()) {
      compoundTag.putFloat(DATA_DURATION_TAG, this.playback.durationTicks());
    }
    if (this.transition.timing() == ModelAnimationSwitchTiming.AFTER_CURRENT) {
      compoundTag.putBoolean(DATA_AFTER_CURRENT_TAG, true);
    }
    if (this.transition.blendDurationTicks()
        != ModelAnimationTransition.DEFAULT_BLEND_DURATION_TICKS) {
      compoundTag.putFloat(DATA_BLEND_TAG, this.transition.blendDurationTicks());
    }
    return compoundTag;
  }

  public boolean hasAnimationName() {
    return !this.animationName.isEmpty();
  }
}
