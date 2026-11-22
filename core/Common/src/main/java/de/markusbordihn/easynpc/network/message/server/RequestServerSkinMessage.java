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

package de.markusbordihn.easynpc.network.message.server;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.config.CustomSkinConfig;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.io.ServerSkinIndex;
import de.markusbordihn.easynpc.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import de.markusbordihn.easynpc.network.message.client.ServerSkinDataMessage;
import de.markusbordihn.easynpc.security.ServerSkinRequestLimiter;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record RequestServerSkinMessage(UUID skinUUID, SkinModel skinModel)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "request_server_skin");
  public static final CustomPacketPayload.Type<RequestServerSkinMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, RequestServerSkinMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), RequestServerSkinMessage::create);

  public static RequestServerSkinMessage create(final FriendlyByteBuf buffer) {
    return new RequestServerSkinMessage(
        buffer.readUUID(), NetworkMessageRecord.readEnum(buffer, SkinModel.class));
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.skinUUID);
    buffer.writeEnum(this.skinModel);
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public Type<? extends CustomPacketPayload> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public void handleServer(final ServerPlayer serverPlayer) {
    if (serverPlayer == null || this.skinModel == null || !CustomSkinConfig.SHARE_SERVER_SKINS) {
      return;
    }

    ServerSkinIndex.ServerSkin serverSkin = ServerSkinIndex.getSkin(this.skinModel, this.skinUUID);
    byte[] pngBytes = serverSkin != null ? serverSkin.pngBytes() : new byte[0];
    if (!ServerSkinRequestLimiter.checkAndRecord(serverPlayer.getUUID(), pngBytes.length)) {
      log.debug("Dropped server skin request {} from {}", this.skinUUID, serverPlayer);
      return;
    }

    NetworkHandlerManager.sendMessageToPlayer(
        new ServerSkinDataMessage(this.skinUUID, this.skinModel, pngBytes), serverPlayer);
  }
}
