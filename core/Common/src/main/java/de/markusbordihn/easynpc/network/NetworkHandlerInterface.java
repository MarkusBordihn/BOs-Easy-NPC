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
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface NetworkHandlerInterface {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);
  String LOG_PREFIX = "[NetworkHandler]";
  int PROTOCOL_VERSION = 32;

  <M extends NetworkMessageRecord> void registerClientNetworkMessageHandler(
      final ResourceLocation messageID,
      final Class<M> networkMessage,
      final Function<FriendlyByteBuf, M> creator);

  <M extends NetworkMessageRecord> void registerServerNetworkMessageHandler(
      final ResourceLocation messageID,
      final Class<M> networkMessage,
      final Function<FriendlyByteBuf, M> creator);

  <M extends NetworkMessageRecord> void sendToServer(M networkMessageRecord);

  <M extends NetworkMessageRecord> void sendToPlayer(
      M networkMessageRecord, ServerPlayer serverPlayer);

  <M extends NetworkMessageRecord> void addClientMessage(
      final ResourceLocation messageID, final Class<M> networkMessage);

  <M extends NetworkMessageRecord> void addServerMessage(
      final ResourceLocation messageID, final Class<M> networkMessage);

  Map<ResourceLocation, Class<? extends NetworkMessageRecord>> getClientMessages();

  Map<ResourceLocation, Class<? extends NetworkMessageRecord>> getServerMessages();

  <M extends NetworkMessageRecord> void addRegisteredClientMessage(
      final ResourceLocation messageID, final Class<M> networkMessage);

  <M extends NetworkMessageRecord> void addRegisteredServerMessage(
      final ResourceLocation messageID, final Class<M> networkMessage);

  Map<ResourceLocation, Class<? extends NetworkMessageRecord>> getRegisteredClientMessages();

  Map<ResourceLocation, Class<? extends NetworkMessageRecord>> getRegisteredServerMessages();

  default boolean sendMessageToPlayer(
      final NetworkMessageRecord networkMessageRecord, final ServerPlayer serverPlayer) {
    if (!this.hasClientMessage(networkMessageRecord.id())) {
      log.error(
          "{} Message {} is not registered as client message",
          LOG_PREFIX,
          networkMessageRecord.id());
      return false;
    }

    try {
      this.sendToPlayer(networkMessageRecord, serverPlayer);
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
    if (!this.hasServerMessage(networkMessageRecord.id())) {
      log.error(
          "{} Message {} is not registered as server message",
          LOG_PREFIX,
          networkMessageRecord.id());
      return false;
    }

    if (Minecraft.getInstance().getConnection() == null) {
      log.error(
          "{} Failed to send message {} to server: No connection available",
          LOG_PREFIX,
          networkMessageRecord.id());
      return false;
    }

    try {
      this.sendToServer(networkMessageRecord);
    } catch (Exception e) {
      log.error("{} Failed to send message {} to server", LOG_PREFIX, networkMessageRecord.id(), e);
      return false;
    }
    return true;
  }

  default boolean hasClientMessage(final ResourceLocation messageID) {
    return this.getClientMessages().containsKey(messageID);
  }

  default boolean hasServerMessage(final ResourceLocation messageID) {
    return this.getServerMessages().containsKey(messageID);
  }

  default Class<? extends NetworkMessageRecord> getRegisteredClientMessage(
      final ResourceLocation messageID) {
    return this.getRegisteredClientMessages().get(messageID);
  }

  default Class<? extends NetworkMessageRecord> getRegisteredServerMessage(
      final ResourceLocation messageID) {
    return this.getRegisteredServerMessages().get(messageID);
  }

  default ResourceLocation getRegisteredClientMessageId(
      final Class<? extends NetworkMessageRecord> networkMessage) {
    return this.getRegisteredClientMessages().entrySet().stream()
        .filter(entry -> entry.getValue().equals(networkMessage))
        .map(Map.Entry::getKey)
        .findFirst()
        .orElse(null);
  }

  default ResourceLocation getRegisteredServerMessageId(
      final Class<? extends NetworkMessageRecord> networkMessage) {
    return this.getRegisteredServerMessages().entrySet().stream()
        .filter(entry -> entry.getValue().equals(networkMessage))
        .map(Map.Entry::getKey)
        .findFirst()
        .orElse(null);
  }

  default boolean hasRegisteredClientMessage(final ResourceLocation messageID) {
    return this.getRegisteredClientMessages().containsKey(messageID);
  }

  default boolean hasRegisteredClientMessage(
      final Class<? extends NetworkMessageRecord> networkMessage) {
    return this.getRegisteredClientMessages().containsValue(networkMessage);
  }

  default boolean hasRegisteredServerMessage(final ResourceLocation messageID) {
    return this.getRegisteredServerMessages().containsKey(messageID);
  }

  default boolean hasRegisteredServerMessage(
      final Class<? extends NetworkMessageRecord> networkMessage) {
    return this.getRegisteredServerMessages().containsValue(networkMessage);
  }

  default <M extends NetworkMessageRecord> void registerServerNetworkMessage(
      final ResourceLocation messageID,
      final Class<M> networkMessage,
      final Function<FriendlyByteBuf, M> creator) {
    if (NetworkHandlerManager.isServerNetworkHandler()) {
      if (this.hasRegisteredServerMessage(messageID)) {
        log.error(
            "{} Server network message id {} already registered with {}",
            LOG_PREFIX,
            messageID,
            this.getRegisteredServerMessage(messageID));
        return;
      }

      if (this.hasRegisteredServerMessage(networkMessage)) {
        log.error(
            "{} Server network message {} already registered with id {}",
            LOG_PREFIX,
            networkMessage,
            this.getRegisteredServerMessageId(networkMessage));
        return;
      }

      try {
        this.registerServerNetworkMessageHandler(messageID, networkMessage, creator);
        this.addRegisteredServerMessage(messageID, networkMessage);
      } catch (Exception e) {
        log.error(
            "{} Failed to register server network message id {} with {}",
            LOG_PREFIX,
            messageID,
            networkMessage,
            e);
        return;
      }
    }
    this.addServerMessage(messageID, networkMessage);
  }

  default <M extends NetworkMessageRecord> void registerClientNetworkMessage(
      final ResourceLocation messageID,
      final Class<M> networkMessage,
      final Function<FriendlyByteBuf, M> creator) {
    if (NetworkHandlerManager.isClientNetworkHandler()) {
      if (this.hasRegisteredClientMessage(messageID)) {
        log.error(
            "{} Client network message id {} already registered with {}",
            LOG_PREFIX,
            messageID,
            this.getRegisteredClientMessage(messageID));
        return;
      }

      if (this.hasRegisteredClientMessage(networkMessage)) {
        log.error(
            "{} Client network message {} already registered with id {}",
            LOG_PREFIX,
            networkMessage,
            this.getRegisteredClientMessageId(networkMessage));
        return;
      }

      try {
        this.registerClientNetworkMessageHandler(messageID, networkMessage, creator);
        this.addRegisteredClientMessage(messageID, networkMessage);
      } catch (Exception e) {
        log.error(
            "{} Failed to register client network message id {} with {}",
            LOG_PREFIX,
            messageID,
            networkMessage,
            e);
        return;
      }
    }
    this.addClientMessage(messageID, networkMessage);
  }

  default void logRegisterClientNetworkMessageHandler(
      final ResourceLocation messageID, final Class<?> networkMessage) {
    log.debug(
        "{} Registering client network message {} with {}",
        LOG_PREFIX,
        networkMessage.getSimpleName(),
        messageID);
  }

  default void logRegisterClientNetworkMessageHandler(
      final ResourceLocation messageID, final Class<?> networkMessage, final int registrationID) {
    log.debug(
        "{} Registering client network message {} with {} ({})",
        LOG_PREFIX,
        networkMessage.getSimpleName(),
        messageID,
        registrationID);
  }

  default void logRegisterServerNetworkMessageHandler(
      final ResourceLocation messageID, final Class<?> networkMessage) {
    log.debug(
        "{} Registering server network message {} with {}",
        LOG_PREFIX,
        networkMessage.getSimpleName(),
        messageID);
  }

  default void logRegisterServerNetworkMessageHandler(
      final ResourceLocation messageID, final Class<?> networkMessage, final int registrationID) {
    log.debug(
        "{} Registering server network message {} with {} ({})",
        LOG_PREFIX,
        networkMessage.getSimpleName(),
        messageID,
        registrationID);
  }
}
