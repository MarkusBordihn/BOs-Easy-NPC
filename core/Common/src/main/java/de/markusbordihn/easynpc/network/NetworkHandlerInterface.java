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
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.Map;
import java.util.function.Function;
import net.minecraft.client.Minecraft;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface NetworkHandlerInterface {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);
  String LOG_PREFIX = "[NetworkHandler]";
  int PROTOCOL_VERSION = 23;

  <M extends NetworkMessageRecord> void registerClientNetworkMessageHandler(
      final CustomPacketPayload.Type<M> type,
      final StreamCodec<RegistryFriendlyByteBuf, M> codec,
      final Class<M> networkMessage,
      final Function<FriendlyByteBuf, M> creator);

  <M extends NetworkMessageRecord> void registerServerNetworkMessageHandler(
      final CustomPacketPayload.Type<M> type,
      final StreamCodec<RegistryFriendlyByteBuf, M> codec,
      final Class<M> networkMessage,
      final Function<FriendlyByteBuf, M> creator);

  <M extends NetworkMessageRecord> void sendToServer(M networkMessageRecord);

  <M extends NetworkMessageRecord> void sendToPlayer(
      M networkMessageRecord, ServerPlayer serverPlayer);

  <M extends NetworkMessageRecord> void addClientMessage(
      final CustomPacketPayload.Type<M> messageID, final Class<M> networkMessage);

  <M extends NetworkMessageRecord> void addServerMessage(
      final CustomPacketPayload.Type<M> messageID, final Class<M> networkMessage);

  Map<CustomPacketPayload.Type<?>, Class<? extends NetworkMessageRecord>> getClientMessages();

  Map<CustomPacketPayload.Type<?>, Class<? extends NetworkMessageRecord>> getServerMessages();

  <M extends NetworkMessageRecord> void addRegisteredClientMessage(
      final CustomPacketPayload.Type<M> messageID, final Class<M> networkMessage);

  <M extends NetworkMessageRecord> void addRegisteredServerMessage(
      final CustomPacketPayload.Type<M> messageID, final Class<M> networkMessage);

  Map<CustomPacketPayload.Type<?>, Class<? extends NetworkMessageRecord>>
      getRegisteredClientMessages();

  Map<CustomPacketPayload.Type<?>, Class<? extends NetworkMessageRecord>>
      getRegisteredServerMessages();

  default <M extends NetworkMessageRecord> void registerClientPayloadType(
      final CustomPacketPayload.Type<M> type,
      final StreamCodec<RegistryFriendlyByteBuf, M> codec) {}

  default <M extends NetworkMessageRecord> void registerServerPayloadType(
      final CustomPacketPayload.Type<M> type,
      final StreamCodec<RegistryFriendlyByteBuf, M> codec) {}

  default boolean sendMessageToPlayer(
      final NetworkMessageRecord networkMessageRecord, final ServerPlayer serverPlayer) {
    if (!hasClientMessage(networkMessageRecord.type())) {
      log.error(
          "{} Message {} is not registered as client message",
          LOG_PREFIX,
          networkMessageRecord.type());
      return false;
    }
    try {
      sendToPlayer(networkMessageRecord, serverPlayer);
    } catch (Exception e) {
      log.error(
          "{} Failed to send message {} to player {}",
          LOG_PREFIX,
          networkMessageRecord.id(),
          serverPlayer.getName().getString(),
          e);
      return false;
    }
    return true;
  }

  default boolean sendMessageToServer(final NetworkMessageRecord networkMessageRecord) {
    if (!hasServerMessage(networkMessageRecord.type())) {
      log.error(
          "{} Message {} is not registered as server message",
          LOG_PREFIX,
          networkMessageRecord.type());
      return false;
    }
    if (Minecraft.getInstance().getConnection() == null) {
      log.error(
          "{} Failed to send message {} to server: No connection available",
          LOG_PREFIX,
          networkMessageRecord.type());
      return false;
    }
    try {
      sendToServer(networkMessageRecord);
    } catch (Exception e) {
      log.error("{} Failed to send message {} to server", LOG_PREFIX, networkMessageRecord.id(), e);
      return false;
    }
    return true;
  }

  default boolean hasClientMessage(final CustomPacketPayload.Type<?> messageID) {
    return getClientMessages().containsKey(messageID);
  }

  default boolean hasServerMessage(final CustomPacketPayload.Type<?> messageID) {
    return getServerMessages().containsKey(messageID);
  }

  default Class<? extends NetworkMessageRecord> getRegisteredClientMessage(
      final CustomPacketPayload.Type<?> messageID) {
    return getRegisteredClientMessages().get(messageID);
  }

  default Class<? extends NetworkMessageRecord> getRegisteredServerMessage(
      final CustomPacketPayload.Type<?> messageID) {
    return getRegisteredServerMessages().get(messageID);
  }

  default CustomPacketPayload.Type<?> getRegisteredClientMessageId(
      final Class<? extends NetworkMessageRecord> networkMessage) {
    return getRegisteredClientMessages().entrySet().stream()
        .filter(entry -> entry.getValue().equals(networkMessage))
        .map(Map.Entry::getKey)
        .findFirst()
        .orElse(null);
  }

  default CustomPacketPayload.Type<?> getRegisteredServerMessageId(
      final Class<? extends NetworkMessageRecord> networkMessage) {
    return getRegisteredServerMessages().entrySet().stream()
        .filter(entry -> entry.getValue().equals(networkMessage))
        .map(Map.Entry::getKey)
        .findFirst()
        .orElse(null);
  }

  default boolean hasRegisteredClientMessage(final CustomPacketPayload.Type<?> messageID) {
    return getRegisteredClientMessages().containsKey(messageID);
  }

  default boolean hasRegisteredClientMessage(
      final Class<? extends NetworkMessageRecord> networkMessage) {
    return getRegisteredClientMessages().containsValue(networkMessage);
  }

  default boolean hasRegisteredServerMessage(final CustomPacketPayload.Type<?> messageID) {
    return getRegisteredServerMessages().containsKey(messageID);
  }

  default boolean hasRegisteredServerMessage(
      final Class<? extends NetworkMessageRecord> networkMessage) {
    return getRegisteredServerMessages().containsValue(networkMessage);
  }

  default <M extends NetworkMessageRecord> void registerServerNetworkMessage(
      final CustomPacketPayload.Type<M> type,
      final StreamCodec<RegistryFriendlyByteBuf, M> codec,
      final Class<M> networkMessage,
      final Function<FriendlyByteBuf, M> creator) {
    if (!hasServerMessage(type)) {
      registerServerPayloadType(type, codec);
    }
    if (NetworkHandlerManager.isServerNetworkHandler()) {
      if (hasRegisteredServerMessage(type)) {
        log.error(
            "{} Server network message id {} already registered with {}",
            LOG_PREFIX,
            type,
            getRegisteredServerMessage(type));
        return;
      }
      if (hasRegisteredServerMessage(networkMessage)) {
        log.error(
            "{} Server network message {} already registered with id {}",
            LOG_PREFIX,
            networkMessage,
            getRegisteredServerMessageId(networkMessage));
        return;
      }
      try {
        registerServerNetworkMessageHandler(type, codec, networkMessage, creator);
        addRegisteredServerMessage(type, networkMessage);
      } catch (Exception e) {
        log.error(
            "{} Failed to register server network message id {} with {}",
            LOG_PREFIX,
            type,
            networkMessage,
            e);
        return;
      }
    }
    addServerMessage(type, networkMessage);
  }

  default <M extends NetworkMessageRecord> void registerClientNetworkMessage(
      final CustomPacketPayload.Type<M> type,
      final StreamCodec<RegistryFriendlyByteBuf, M> codec,
      final Class<M> networkMessage,
      final Function<FriendlyByteBuf, M> creator) {
    if (!hasClientMessage(type)) {
      registerClientPayloadType(type, codec);
    }
    if (NetworkHandlerManager.isClientNetworkHandler()) {
      if (hasRegisteredClientMessage(type)) {
        log.error(
            "{} Client network message id {} already registered with {}",
            LOG_PREFIX,
            type,
            getRegisteredClientMessage(type));
        return;
      }
      if (hasRegisteredClientMessage(networkMessage)) {
        log.error(
            "{} Client network message {} already registered with id {}",
            LOG_PREFIX,
            networkMessage,
            getRegisteredClientMessageId(networkMessage));
        return;
      }
      try {
        registerClientNetworkMessageHandler(type, codec, networkMessage, creator);
        addRegisteredClientMessage(type, networkMessage);
      } catch (Exception e) {
        log.error(
            "{} Failed to register client network message id {} with {}",
            LOG_PREFIX,
            type,
            networkMessage,
            e);
        return;
      }
    }
    addClientMessage(type, networkMessage);
  }

  default void logRegisterClientNetworkMessageHandler(
      final CustomPacketPayload.Type<?> messageID, final Class<?> networkMessage) {
    log.info(
        "{} Registering client network message {} with {}",
        LOG_PREFIX,
        networkMessage.getSimpleName(),
        messageID);
  }

  default void logRegisterClientNetworkMessageHandler(
      final CustomPacketPayload.Type<?> messageID,
      final Class<?> networkMessage,
      final int registrationID) {
    log.info(
        "{} Registering client network message {} with {} ({})",
        LOG_PREFIX,
        networkMessage.getSimpleName(),
        messageID,
        registrationID);
  }

  default void logRegisterServerNetworkMessageHandler(
      final CustomPacketPayload.Type<?> messageID, final Class<?> networkMessage) {
    log.info(
        "{} Registering server network message {} with {}",
        LOG_PREFIX,
        networkMessage.getSimpleName(),
        messageID);
  }

  default void logRegisterServerNetworkMessageHandler(
      final CustomPacketPayload.Type<?> messageID,
      final Class<?> networkMessage,
      final int registrationID) {
    log.info(
        "{} Registering server network message {} with {} ({})",
        LOG_PREFIX,
        networkMessage.getSimpleName(),
        messageID,
        registrationID);
  }
}
