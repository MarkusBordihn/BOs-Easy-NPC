/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
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
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking;
import net.fabricmc.fabric.api.networking.v1.PayloadTypeRegistry;
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NetworkHandler implements NetworkHandlerInterface {
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public NetworkHandler() {
    log.info("{} NetworkHandler ...", Constants.LOG_REGISTER_PREFIX);
  }

  public static void registerClientNetworkHandler() {
    NetworkHandlerManager.registerClientNetworkHandler();
  }

  public static void registerServerNetworkHandler() {
    NetworkHandlerManager.registerServerNetworkHandler();
  }

  @Override
  public void sendToServer(NetworkMessageRecord networkMessageRecord) {
    try {
      ClientPlayNetworking.send(networkMessageRecord);
    } catch (Exception e) {
      log.error("Failed to send {} to server:", networkMessageRecord, e);
    }
  }

  @Override
  public void sendToPlayer(NetworkMessageRecord networkMessageRecord, ServerPlayer serverPlayer) {
    try {
      ServerPlayNetworking.send(serverPlayer, networkMessageRecord);
    } catch (Exception e) {
      log.error("Failed to send {} to player {}:", networkMessageRecord, serverPlayer, e);
    }
  }

  @Override
  public <M extends NetworkMessageRecord> void registerClientNetworkMessageHandler(
      final CustomPacketPayload.Type<M> type,
      final StreamCodec<RegistryFriendlyByteBuf, M> codec,
      final Class<M> networkMessageRecord,
      final Function<FriendlyByteBuf, M> creator) {
    try {
      log.info("Registering client payload type {} with {}", type, codec);
      PayloadTypeRegistry.playS2C().register(type, codec);
      ClientPlayNetworking.registerGlobalReceiver(
          type, (payload, context) -> payload.handleClient());
    } catch (Exception e) {
      log.error("Failed to register network message handler {}:", networkMessageRecord, e);
    }
  }

  @Override
  public <M extends NetworkMessageRecord> void registerServerNetworkMessageHandler(
      final CustomPacketPayload.Type<M> type,
      final StreamCodec<RegistryFriendlyByteBuf, M> codec,
      final Class<M> networkMessageRecord,
      final Function<FriendlyByteBuf, M> creator) {
    try {
      log.info("Registering server payload type {} with {}", type, codec);
      PayloadTypeRegistry.playC2S().register(type, codec);
      ServerPlayNetworking.registerGlobalReceiver(
          type, (payload, context) -> payload.handleServer(context.player()));
    } catch (Exception e) {
      log.error("Failed to register network message handler {}:", networkMessageRecord, e);
    }
  }
}
