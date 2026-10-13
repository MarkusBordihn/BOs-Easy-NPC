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
import de.markusbordihn.easynpc.client.renderer.layer.ShoulderNPCCache;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.Identifier;

public record ShoulderNPCMessage(
    int playerId, CompoundTag leftShoulderEntity, CompoundTag rightShoulderEntity)
    implements NetworkMessageRecord {

  public static final Identifier MESSAGE_ID =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "shoulder_npc");
  public static final CustomPacketPayload.Type<ShoulderNPCMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ShoulderNPCMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), ShoulderNPCMessage::create);

  public static ShoulderNPCMessage create(final FriendlyByteBuf buffer) {
    return new ShoulderNPCMessage(buffer.readVarInt(), buffer.readNbt(), buffer.readNbt());
  }

  public static ShoulderNPCMessage empty(int playerId) {
    return new ShoulderNPCMessage(playerId, new CompoundTag(), new CompoundTag());
  }

  public boolean hasShoulderNPC() {
    return !this.leftShoulderEntity.isEmpty() || !this.rightShoulderEntity.isEmpty();
  }

  @Override
  public void write(FriendlyByteBuf buffer) {
    buffer.writeVarInt(this.playerId);
    buffer.writeNbt(this.leftShoulderEntity);
    buffer.writeNbt(this.rightShoulderEntity);
  }

  @Override
  public Type<ShoulderNPCMessage> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public Identifier id() {
    return MESSAGE_ID;
  }

  @Override
  public void handleClient() {
    if (this.leftShoulderEntity == null || this.rightShoulderEntity == null) {
      log.error("Invalid shoulder NPC data received for player {}", this.playerId);
      return;
    }

    ShoulderNPCCache.setShoulderEntities(
        this.playerId, this.leftShoulderEntity, this.rightShoulderEntity);
  }
}
