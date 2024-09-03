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
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.function.Function;
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking;
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking;
import net.minecraft.client.Minecraft;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
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
  public void sendToServer(final NetworkMessageRecord networkMessageRecord) {
    if (Minecraft.getInstance().getConnection() == null) {
      log.warn("Failed to send {} to server: No connection available", networkMessageRecord);
      return;
    }
    try {
      ClientPlayNetworking.send(networkMessageRecord.id(), networkMessageRecord.payload());
    } catch (Exception e) {
      log.error("Failed to send {} to server:", networkMessageRecord, e);
    }
  }

  @Override
  public void sendToPlayer(
      final NetworkMessageRecord networkMessageRecord, final ServerPlayer serverPlayer) {
    try {
      ServerPlayNetworking.send(
          serverPlayer, networkMessageRecord.id(), networkMessageRecord.payload());
    } catch (Exception e) {
      log.error("Failed to send {} to player {}:", networkMessageRecord, serverPlayer, e);
    }
  }

  @Override
  public <M extends NetworkMessageRecord> void registerClientNetworkMessageHandler(
      final ResourceLocation messageID,
      final Class<M> networkMessageRecord,
      final Function<FriendlyByteBuf, M> creator) {
    try {
      ClientPlayNetworking.registerGlobalReceiver(
          messageID,
          (client, channelHandler, buffer, responseSender) -> {
            M networkMessage = creator.apply(buffer);
            networkMessage.handleClient();
          });
    } catch (Exception e) {
      log.error("Failed to register client network message handler {}:", networkMessageRecord, e);
    }
  }

  @Override
  public <M extends NetworkMessageRecord> void registerServerNetworkMessageHandler(
      final ResourceLocation messageID,
      final Class<M> networkMessageRecord,
      final Function<FriendlyByteBuf, M> creator) {
    try {
      ServerPlayNetworking.registerGlobalReceiver(
          messageID,
          (server, serverPlayer, channelHandler, buffer, responseSender) -> {
            M networkMessage = creator.apply(buffer);
            networkMessage.handleServer(serverPlayer);
          });
    } catch (Exception e) {
      log.error("Failed to register server network message handler {}:", messageID, e);
    }
  }
}
