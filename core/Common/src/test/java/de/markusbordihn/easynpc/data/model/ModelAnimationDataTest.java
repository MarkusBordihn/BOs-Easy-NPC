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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import io.netty.buffer.Unpooled;
import net.minecraft.network.FriendlyByteBuf;
import org.junit.jupiter.api.Test;

class ModelAnimationDataTest {
  @Test
  void playbackRequestIsNetworkedButNotPersisted() {
    ModelAnimationRequest request =
        new ModelAnimationRequest(
            ModelAnimationOperation.PLAY,
            "named:wave",
            ModelAnimationPlayback.DEFAULT,
            ModelAnimationTransition.DEFAULT,
            4,
            120L);
    ModelAnimationData data = new ModelAnimationData(ModelAnimationBehavior.NONE, request);

    FriendlyByteBuf buffer = new FriendlyByteBuf(Unpooled.buffer());
    data.encode(buffer);
    assertEquals(data, ModelAnimationData.decode(buffer));

    ModelAnimationData loaded = new ModelAnimationData(data.save());
    assertEquals(ModelAnimationBehavior.NONE, loaded.behavior());
    assertFalse(loaded.playbackRequest().isPresent());
  }

  @Test
  void networksRepeatCountAndDurationLimit() {
    ModelAnimationRequest request =
        new ModelAnimationRequest(
            ModelAnimationOperation.PLAY,
            "named:wave",
            ModelAnimationPlayback.repeat(3).withDurationTicks(40.0F),
            ModelAnimationTransition.DEFAULT,
            7,
            240L);

    FriendlyByteBuf buffer = new FriendlyByteBuf(Unpooled.buffer());
    request.encode(buffer);
    ModelAnimationRequest decoded = ModelAnimationRequest.decode(buffer);

    assertEquals(request, decoded);
    assertEquals(3, decoded.playback().repeatCount());
    assertTrue(decoded.playback().hasDurationLimit());
    assertTrue(decoded.playback().isSingleRun());
  }

  @Test
  void repeatCountAndDurationAreNormalized() {
    ModelAnimationPlayback looped =
        new ModelAnimationPlayback(ModelAnimationPlaybackMode.LOOP, 5, -1.0F);

    assertEquals(ModelAnimationPlayback.DEFAULT_REPEAT_COUNT, looped.repeatCount());
    assertEquals(ModelAnimationPlayback.UNLIMITED_DURATION_TICKS, looped.durationTicks());
    assertFalse(looped.hasDurationLimit());
    assertFalse(looped.isSingleRun());
    assertEquals(
        ModelAnimationPlayback.DEFAULT_REPEAT_COUNT,
        ModelAnimationPlayback.repeat(0).repeatCount());
  }
}
