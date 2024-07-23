/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.network;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.network.message.NetworkHandlerInterface;
import de.markusbordihn.easynpc.network.message.NetworkHandlerManager;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.function.Function;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.server.level.ServerPlayer;
import net.neoforged.neoforge.network.PacketDistributor;
import net.neoforged.neoforge.network.event.RegisterPayloadHandlersEvent;
import net.neoforged.neoforge.network.registration.PayloadRegistrar;

public class NetworkHandler implements NetworkHandlerInterface {

  private static final int PROTOCOL_VERSION = 21;
  public static PayloadRegistrar INSTANCE;

  public NetworkHandler() {
    log.info("{} NetworkHandler ...", Constants.LOG_REGISTER_PREFIX);
  }

  public static void registerNetworkHandler(
      final RegisterPayloadHandlersEvent payloadHandlersEvent) {

    INSTANCE =
        payloadHandlersEvent
            .registrar(Constants.MOD_ID)
            .versioned(String.valueOf(PROTOCOL_VERSION))
            .optional();
    log.info(
        "{} Network Handler for {} with version {} ...",
        Constants.LOG_REGISTER_PREFIX,
        INSTANCE,
        PROTOCOL_VERSION);

    NetworkHandlerManager.registerClientNetworkHandler();
    NetworkHandlerManager.registerServerNetworkHandler();
  }

  @Override
  public <M extends NetworkMessageRecord> void sendToServer(M networkMessageRecord) {
    try {
      PacketDistributor.sendToServer(networkMessageRecord);
    } catch (Exception e) {
      log.error("Failed to send {} to server: {}", networkMessageRecord, e);
    }
  }

  @Override
  public <M extends NetworkMessageRecord> void sendToPlayer(
      M networkMessageRecord, ServerPlayer serverPlayer) {
    try {
      PacketDistributor.sendToPlayer(serverPlayer, networkMessageRecord);
    } catch (Exception e) {
      log.error(
          "Failed to send {} to player {}: {}",
          networkMessageRecord,
          serverPlayer.getName().getString(),
          e);
    }
  }

  @Override
  public <M extends NetworkMessageRecord> void registerClientNetworkMessageHandler(
      final CustomPacketPayload.Type<M> type,
      final StreamCodec<RegistryFriendlyByteBuf, M> codec,
      final Class<M> networkMessageRecord,
      final Function<FriendlyByteBuf, M> creator) {
    try {
      INSTANCE.playToClient(
          type,
          codec,
          (customPacketPayload, playPayloadContext) -> customPacketPayload.handleClient());
    } catch (Exception e) {
      log.error("Failed to register network message handler {}:", networkMessageRecord, e);
    }
  }

  @Override
  public <M extends NetworkMessageRecord> void registerServerNetworkMessageHandler(
      final CustomPacketPayload.Type<M> type,
      final StreamCodec<RegistryFriendlyByteBuf, M> codec,
      Class<M> networkMessage,
      Function<FriendlyByteBuf, M> creator) {
    try {
      INSTANCE.playToServer(
          type,
          codec,
          (customPacketPayload, playPayloadContext) -> {
            if (playPayloadContext.player() instanceof ServerPlayer serverPlayer) {
              customPacketPayload.handleServer(serverPlayer);
            } else {
              log.error("Unable to get valid player for network message {}", customPacketPayload);
            }
          });
    } catch (Exception e) {
      log.error("Failed to register network message handler {}:", networkMessage, e);
    }
  }
}
