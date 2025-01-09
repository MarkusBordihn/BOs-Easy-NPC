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
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking;
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class NetworkHandler implements NetworkHandlerInterface {

  private final Map<ResourceLocation, Class<? extends NetworkMessageRecord>> clientMessages =
      new LinkedHashMap<>();
  private final Map<ResourceLocation, Class<? extends NetworkMessageRecord>> serverMessages =
      new LinkedHashMap<>();
  private final Map<ResourceLocation, Class<? extends NetworkMessageRecord>>
      registeredClientMessages = new LinkedHashMap<>();
  private final Map<ResourceLocation, Class<? extends NetworkMessageRecord>>
      registeredServerMessages = new LinkedHashMap<>();

  public NetworkHandler() {
    log.info("{} NetworkHandler with version {}", Constants.LOG_REGISTER_PREFIX, PROTOCOL_VERSION);
  }

  @Override
  public void sendToServer(final NetworkMessageRecord networkMessageRecord) {
    ClientPlayNetworking.send(networkMessageRecord.id(), networkMessageRecord.payload());
  }

  @Override
  public void sendToPlayer(
      final NetworkMessageRecord networkMessageRecord, final ServerPlayer serverPlayer) {
    ServerPlayNetworking.send(
        serverPlayer, networkMessageRecord.id(), networkMessageRecord.payload());
  }

  @Override
  public <M extends NetworkMessageRecord> void registerClientNetworkMessageHandler(
      final ResourceLocation messageID,
      final Class<M> networkMessageRecord,
      final Function<FriendlyByteBuf, M> creator) {
    if (!ClientPlayNetworking.registerGlobalReceiver(
        messageID,
        (client, channelHandler, buffer, responseSender) -> {
          M networkMessage = creator.apply(buffer);
          client.execute(networkMessage::handleClient);
        })) {
      log.error("Failed to register client network message handler for {}", messageID);
    } else {
      logRegisterClientNetworkMessageHandler(messageID, networkMessageRecord);
    }
  }

  @Override
  public <M extends NetworkMessageRecord> void registerServerNetworkMessageHandler(
      final ResourceLocation messageID,
      final Class<M> networkMessageRecord,
      final Function<FriendlyByteBuf, M> creator) {
    if (!ServerPlayNetworking.registerGlobalReceiver(
        messageID,
        (server, serverPlayer, channelHandler, buffer, responseSender) -> {
          M networkMessage = creator.apply(buffer);
          server.execute(() -> networkMessage.handleServer(serverPlayer));
        })) {
      log.error("Failed to register server network message handler for {}", messageID);
    } else {
      logRegisterServerNetworkMessageHandler(messageID, networkMessageRecord);
    }
  }

  @Override
  public <M extends NetworkMessageRecord> void addClientMessage(
      final ResourceLocation messageID, final Class<M> networkMessage) {
    clientMessages.put(messageID, networkMessage);
  }

  @Override
  public <M extends NetworkMessageRecord> void addServerMessage(
      final ResourceLocation messageID, final Class<M> networkMessage) {
    serverMessages.put(messageID, networkMessage);
  }

  @Override
  public Map<ResourceLocation, Class<? extends NetworkMessageRecord>> getClientMessages() {
    return clientMessages;
  }

  @Override
  public Map<ResourceLocation, Class<? extends NetworkMessageRecord>> getServerMessages() {
    return serverMessages;
  }

  @Override
  public <M extends NetworkMessageRecord> void addRegisteredClientMessage(
      final ResourceLocation messageID, final Class<M> networkMessage) {
    registeredClientMessages.put(messageID, networkMessage);
  }

  @Override
  public <M extends NetworkMessageRecord> void addRegisteredServerMessage(
      final ResourceLocation messageID, final Class<M> networkMessage) {
    registeredServerMessages.put(messageID, networkMessage);
  }

  @Override
  public Map<ResourceLocation, Class<? extends NetworkMessageRecord>>
      getRegisteredClientMessages() {
    return registeredClientMessages;
  }

  @Override
  public Map<ResourceLocation, Class<? extends NetworkMessageRecord>>
      getRegisteredServerMessages() {
    return registeredServerMessages;
  }
}
