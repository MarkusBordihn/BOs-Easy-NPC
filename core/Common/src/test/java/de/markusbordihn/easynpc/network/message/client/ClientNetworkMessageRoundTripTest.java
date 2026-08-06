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

package de.markusbordihn.easynpc.network.message.client;

import static org.junit.jupiter.api.Assertions.assertEquals;

import io.netty.buffer.Unpooled;
import java.util.UUID;
import net.minecraft.SharedConstants;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ClientNetworkMessageRoundTripTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static SpeechBubbleMessage roundTrip(SpeechBubbleMessage message) {
    FriendlyByteBuf buffer = new FriendlyByteBuf(Unpooled.buffer());
    message.write(buffer);
    return SpeechBubbleMessage.create(buffer);
  }

  @Test
  @DisplayName("A literal speech bubble survives a network round trip")
  void testSpeechBubbleWithLiteralText() {
    UUID uuid = UUID.randomUUID();
    SpeechBubbleMessage message =
        new SpeechBubbleMessage(uuid, Component.literal("Hello, traveller!"), 120);

    SpeechBubbleMessage loaded = roundTrip(message);

    assertEquals(uuid, loaded.uuid());
    assertEquals(message.text(), loaded.text());
    assertEquals(120, loaded.durationTicks());
  }

  @Test
  @DisplayName("A translated speech bubble keeps its translation key")
  void testSpeechBubbleWithTranslationKey() {
    SpeechBubbleMessage message =
        new SpeechBubbleMessage(
            UUID.randomUUID(), Component.translatable("text.easy_npc.example.hello"), 100);

    SpeechBubbleMessage loaded = roundTrip(message);

    assertEquals(message.text(), loaded.text());
  }
}
