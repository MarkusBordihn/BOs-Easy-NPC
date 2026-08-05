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

import de.markusbordihn.easynpc.data.model.ModelAnimationPlaybackMode;
import de.markusbordihn.easynpc.data.model.ModelAnimationSwitchTiming;
import de.markusbordihn.easynpc.data.model.ModelAnimationTransition;
import java.util.Locale;
import net.minecraft.nbt.CompoundTag;

public record ModelAnimationActionData(
    String animationName,
    ModelAnimationPlaybackMode playbackMode,
    ModelAnimationTransition transition) {

  public static final ModelAnimationActionData DEFAULT =
      new ModelAnimationActionData(
          "", ModelAnimationPlaybackMode.ONCE, ModelAnimationTransition.DEFAULT);
  public static final String DATA_NAME_TAG = "Name";
  public static final String DATA_LOOP_TAG = "Loop";
  public static final String DATA_AFTER_CURRENT_TAG = "After";
  public static final String DATA_BLEND_TAG = "Blend";

  public ModelAnimationActionData {
    animationName = animationName == null ? "" : animationName.trim().toLowerCase(Locale.ROOT);
    playbackMode = playbackMode == null ? ModelAnimationPlaybackMode.ONCE : playbackMode;
    transition = transition == null ? ModelAnimationTransition.DEFAULT : transition;
  }

  public ModelAnimationActionData(String animationName) {
    this(animationName, ModelAnimationPlaybackMode.ONCE, ModelAnimationTransition.DEFAULT);
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
        compoundTag.getBoolean(DATA_LOOP_TAG)
            ? ModelAnimationPlaybackMode.LOOP
            : ModelAnimationPlaybackMode.ONCE,
        new ModelAnimationTransition(timing, blend));
  }

  public CompoundTag createTag() {
    CompoundTag compoundTag = new CompoundTag();
    if (!this.animationName.isEmpty()) {
      compoundTag.putString(DATA_NAME_TAG, this.animationName);
    }
    if (this.playbackMode == ModelAnimationPlaybackMode.LOOP) {
      compoundTag.putBoolean(DATA_LOOP_TAG, true);
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
