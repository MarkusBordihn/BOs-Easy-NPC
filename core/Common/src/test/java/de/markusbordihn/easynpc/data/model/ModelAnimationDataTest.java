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
