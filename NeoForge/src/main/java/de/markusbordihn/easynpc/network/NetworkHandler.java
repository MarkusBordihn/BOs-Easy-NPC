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
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.neoforged.neoforge.network.PacketDistributor;
import net.neoforged.neoforge.network.event.RegisterPayloadHandlerEvent;
import net.neoforged.neoforge.network.registration.IPayloadRegistrar;

public class NetworkHandler implements NetworkHandlerInterface {

  private static final int PROTOCOL_VERSION = 21;
  public static IPayloadRegistrar INSTANCE;

  public NetworkHandler() {
    log.info("{} NetworkHandler ...", Constants.LOG_REGISTER_PREFIX);
  }

  public static void registerNetworkHandler(final RegisterPayloadHandlerEvent payloadHandlerEvent) {

    INSTANCE =
        payloadHandlerEvent
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
      PacketDistributor.SERVER.noArg().send(networkMessageRecord);
    } catch (Exception e) {
      log.error("Failed to send {} to server, got error: {}", networkMessageRecord, e);
    }
  }

  @Override
  public <M extends NetworkMessageRecord> void sendToPlayer(
      M networkMessageRecord, ServerPlayer serverPlayer) {
    try {
      PacketDistributor.PLAYER.with(serverPlayer).send(networkMessageRecord);
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
      final ResourceLocation messageID,
      final Class<M> networkMessageRecord,
      final Function<FriendlyByteBuf, M> creator) {
    INSTANCE.play(
        messageID,
        creator::apply,
        handler ->
            handler.client((customPacketPayload, unused) -> customPacketPayload.handleClient()));
  }

  @Override
  public <M extends NetworkMessageRecord> void registerServerNetworkMessageHandler(
      ResourceLocation messageID, Class<M> networkMessage, Function<FriendlyByteBuf, M> creator) {
    INSTANCE.play(
        messageID,
        creator::apply,
        handler ->
            handler.server(
                (customPacketPayload, playPayloadContext) -> {
                  if (playPayloadContext.player().get() instanceof ServerPlayer serverPlayer) {
                    customPacketPayload.handleServer(serverPlayer);
                  } else {
                    log.error("Failed to get server player for {}", customPacketPayload);
                  }
                }));
  }
}
