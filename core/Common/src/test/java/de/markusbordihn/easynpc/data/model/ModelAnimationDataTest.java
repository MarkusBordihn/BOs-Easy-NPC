package de.markusbordihn.easynpc.data.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

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
            ModelAnimationPlaybackMode.ONCE,
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
}
