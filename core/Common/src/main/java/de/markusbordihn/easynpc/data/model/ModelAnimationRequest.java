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

import java.util.Locale;
import java.util.Objects;
import net.minecraft.network.FriendlyByteBuf;

public record ModelAnimationRequest(
    ModelAnimationOperation operation,
    String animationName,
    ModelAnimationPlayback playback,
    ModelAnimationTransition transition,
    int sequence,
    long issuedGameTime) {

  public static final ModelAnimationRequest NONE =
      new ModelAnimationRequest(
          ModelAnimationOperation.NONE,
          "",
          ModelAnimationPlayback.DEFAULT,
          ModelAnimationTransition.DEFAULT,
          0,
          0L);

  public ModelAnimationRequest {
    operation = Objects.requireNonNull(operation, "operation");
    animationName = animationName == null ? "" : animationName.trim().toLowerCase(Locale.ROOT);
    playback = playback == null ? ModelAnimationPlayback.DEFAULT : playback;
    transition = transition == null ? ModelAnimationTransition.DEFAULT : transition;
  }

  public static ModelAnimationRequest decode(FriendlyByteBuf buffer) {
    return new ModelAnimationRequest(
        buffer.readEnum(ModelAnimationOperation.class),
        buffer.readUtf(256),
        new ModelAnimationPlayback(
            buffer.readEnum(ModelAnimationPlaybackMode.class),
            buffer.readVarInt(),
            buffer.readFloat()),
        new ModelAnimationTransition(
            buffer.readEnum(ModelAnimationSwitchTiming.class), buffer.readFloat()),
        buffer.readVarInt(),
        buffer.readVarLong());
  }

  public void encode(FriendlyByteBuf buffer) {
    buffer.writeEnum(this.operation);
    buffer.writeUtf(this.animationName, 256);
    buffer.writeEnum(this.playback.mode());
    buffer.writeVarInt(this.playback.repeatCount());
    buffer.writeFloat(this.playback.durationTicks());
    buffer.writeEnum(this.transition.timing());
    buffer.writeFloat(this.transition.blendDurationTicks());
    buffer.writeVarInt(this.sequence);
    buffer.writeVarLong(this.issuedGameTime);
  }

  public boolean isPresent() {
    return this.operation != ModelAnimationOperation.NONE;
  }
}
