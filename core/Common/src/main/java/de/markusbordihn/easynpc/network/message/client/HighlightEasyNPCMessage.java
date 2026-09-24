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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.highlight.NPCHighlightManager;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import io.netty.buffer.ByteBuf;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import net.minecraft.core.UUIDUtil;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.Identifier;

public record HighlightEasyNPCMessage(List<UUID> uuids, int durationTicks)
    implements NetworkMessageRecord {

  public static final Identifier MESSAGE_ID =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "highlight_easy_npc");
  public static final CustomPacketPayload.Type<HighlightEasyNPCMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, HighlightEasyNPCMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), HighlightEasyNPCMessage::create);

  public static final int MAXIMUM_UUIDS = 256;
  public static final int MAXIMUM_DURATION_TICKS = 24000;
  private static final StreamCodec<ByteBuf, List<UUID>> UUIDS_DECODER =
      ByteBufCodecs.collection(ArrayList::new, UUIDUtil.STREAM_CODEC, MAXIMUM_UUIDS);
  private static final StreamCodec<ByteBuf, List<UUID>> UUIDS_ENCODER =
      ByteBufCodecs.collection(ArrayList::new, UUIDUtil.STREAM_CODEC);

  public static HighlightEasyNPCMessage create(final FriendlyByteBuf buffer) {
    return new HighlightEasyNPCMessage(UUIDS_DECODER.decode(buffer), buffer.readVarInt());
  }

  @Override
  public void write(FriendlyByteBuf buffer) {
    UUIDS_ENCODER.encode(buffer, this.uuids);
    buffer.writeVarInt(this.durationTicks);
  }

  @Override
  public Type<HighlightEasyNPCMessage> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public Identifier id() {
    return MESSAGE_ID;
  }

  @Override
  public void handleClient() {
    if (this.uuids == null || this.uuids.isEmpty()) {
      log.error("Invalid highlight request for {} NPCs", this.uuids);
      return;
    }

    int highlightDurationTicks = Math.min(this.durationTicks, MAXIMUM_DURATION_TICKS);
    for (UUID uuid : this.uuids) {
      NPCHighlightManager.highlight(uuid, highlightDurationTicks);
    }
  }
}
