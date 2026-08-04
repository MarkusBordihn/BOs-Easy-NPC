/*
 * Copyright 2025 Markus Bordihn
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

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;

public record ModelAnimationData(
    ModelAnimationBehavior behavior, ModelAnimationRequest playbackRequest) {


  public static final String DATA_BEHAVIOR_TAG = "Behavior";
  public static final ModelAnimationData DEFAULT =
      new ModelAnimationData(ModelAnimationBehavior.SMART);

  public ModelAnimationData {
    behavior = behavior != null ? behavior : ModelAnimationBehavior.SMART;
    playbackRequest = playbackRequest != null ? playbackRequest : ModelAnimationRequest.NONE;
  }

  public ModelAnimationData() {
    this(ModelAnimationBehavior.SMART, ModelAnimationRequest.NONE);
  }

  public ModelAnimationData(ModelAnimationBehavior behavior) {
    this(behavior, ModelAnimationRequest.NONE);
  }

  public ModelAnimationData(CompoundTag compoundTag) {
    this(
        compoundTag.contains(DATA_BEHAVIOR_TAG)
            ? ModelAnimationBehavior.get(compoundTag.getString(DATA_BEHAVIOR_TAG))
            : ModelAnimationBehavior.SMART,
        ModelAnimationRequest.NONE);
  }

  public static ModelAnimationData decode(FriendlyByteBuf buffer) {
    return new ModelAnimationData(
        buffer.readEnum(ModelAnimationBehavior.class), ModelAnimationRequest.decode(buffer));
  }

  public boolean hasChanged() {
    return this.behavior != ModelAnimationBehavior.SMART;
  }

  public CompoundTag save() {
    CompoundTag compoundTag = new CompoundTag();
    if (this.behavior != null) {
      compoundTag.putString(DATA_BEHAVIOR_TAG, this.behavior.name());
    }
    return compoundTag;
  }

  public void encode(FriendlyByteBuf buffer) {
    buffer.writeEnum(this.behavior);
    this.playbackRequest.encode(buffer);
  }
}
