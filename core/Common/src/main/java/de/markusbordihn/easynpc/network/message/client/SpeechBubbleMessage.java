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

import com.google.gson.JsonParser;
import com.mojang.serialization.JsonOps;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.SpeechBubbleManager;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.ComponentSerialization;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;

public record SpeechBubbleMessage(UUID uuid, Component text, int durationTicks)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "speech_bubble");
  public static final CustomPacketPayload.Type<SpeechBubbleMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, SpeechBubbleMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), SpeechBubbleMessage::create);

  public static SpeechBubbleMessage create(final FriendlyByteBuf buffer) {
    return new SpeechBubbleMessage(
        buffer.readUUID(),
        ComponentSerialization.CODEC
            .parse(JsonOps.INSTANCE, JsonParser.parseString(buffer.readUtf()))
            .getOrThrow(),
        buffer.readVarInt());
  }

  @Override
  public void write(FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUtf(
        ComponentSerialization.CODEC
            .encodeStart(JsonOps.INSTANCE, this.text)
            .getOrThrow()
            .toString());
    buffer.writeVarInt(this.durationTicks);
  }

  @Override
  public Type<SpeechBubbleMessage> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public void handleClient() {
    if (this.uuid == null || this.text == null) {
      log.error("Invalid speech bubble {} for {}", this.text, this.uuid);
      return;
    }

    SpeechBubbleManager.show(this.uuid, this.text, this.durationTicks);
  }
}
