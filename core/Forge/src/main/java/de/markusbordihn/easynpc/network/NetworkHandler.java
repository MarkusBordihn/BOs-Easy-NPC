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
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.BooleanSupplier;
import java.util.function.Function;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload.Type;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.loading.FMLEnvironment;
import net.minecraftforge.network.ChannelBuilder;
import net.minecraftforge.network.NetworkDirection;
import net.minecraftforge.network.PacketDistributor;
import net.minecraftforge.network.SimpleChannel;

public class NetworkHandler implements NetworkHandlerInterface {

  private final SimpleChannel channel;
  private final BooleanSupplier clientNetworkHandler;
  private final BooleanSupplier serverNetworkHandler;
  private final Map<Type<?>, Class<? extends NetworkMessageRecord>> clientMessages =
      new LinkedHashMap<>();
  private final Map<CustomPacketPayload.Type<?>, Class<? extends NetworkMessageRecord>>
      serverMessages = new LinkedHashMap<>();
  private final Map<CustomPacketPayload.Type<?>, Class<? extends NetworkMessageRecord>>
      registeredClientMessages = new LinkedHashMap<>();
  private final Map<CustomPacketPayload.Type<?>, Class<? extends NetworkMessageRecord>>
      registeredServerMessages = new LinkedHashMap<>();
  private int id = 0;

  public NetworkHandler(
      final String modId,
      final BooleanSupplier clientNetworkHandler,
      final BooleanSupplier serverNetworkHandler) {
    this.channel =
        ChannelBuilder.named(Identifier.fromNamespaceAndPath(modId, "network"))
            .networkProtocolVersion(PROTOCOL_VERSION)
            .simpleChannel();
    this.clientNetworkHandler = clientNetworkHandler;
    this.serverNetworkHandler = serverNetworkHandler;
    log.info(
        "{} Network Handler for {} with version {} ...",
        Constants.LOG_REGISTER_PREFIX,
        this.channel,
        PROTOCOL_VERSION);
  }

  @Override
  public boolean isClientNetworkHandler() {
    return this.clientNetworkHandler.getAsBoolean();
  }

  @Override
  public boolean isServerNetworkHandler() {
    return this.serverNetworkHandler.getAsBoolean();
  }

  @Override
  public <M extends NetworkMessageRecord> void sendToServer(M networkMessageRecord) {
    if (FMLEnvironment.dist == Dist.CLIENT) {
      this.channel.send(networkMessageRecord, PacketDistributor.SERVER.noArg());
    }
  }

  @Override
  public <M extends NetworkMessageRecord> void sendToPlayer(
      M networkMessageRecord, ServerPlayer serverPlayer) {
    this.channel.send(networkMessageRecord, PacketDistributor.PLAYER.with(serverPlayer));
  }

  @Override
  public <M extends NetworkMessageRecord> void registerClientNetworkMessageHandler(
      final CustomPacketPayload.Type<M> type,
      final StreamCodec<RegistryFriendlyByteBuf, M> codec,
      final Class<M> networkMessage,
      final Function<FriendlyByteBuf, M> creator) {
    int registrationID = this.id++;
    this.logRegisterClientNetworkMessageHandler(type, networkMessage, registrationID);
    this.channel
        .messageBuilder(networkMessage, registrationID, NetworkDirection.PLAY_TO_CLIENT)
        .encoder(M::write)
        .decoder(creator::apply)
        .consumerNetworkThread((message, context) -> {
          context.enqueueWork(() -> {
            if (message == null) {
              log.warn("Received null client message, ignoring packet");
              return;
            }

            NetworkMessageRecord.guardedHandler(
                    type.id(),
                    () -> {
                      if (FMLEnvironment.dist == Dist.CLIENT) {
                        message.handleClient();
                      }
                    })
                .run();
          });
          context.setPacketHandled(true);
        })
        .add();
  }

  @Override
  public <M extends NetworkMessageRecord> void registerServerNetworkMessageHandler(
      final CustomPacketPayload.Type<M> type,
      final StreamCodec<RegistryFriendlyByteBuf, M> codec,
      final Class<M> networkMessage,
      final Function<FriendlyByteBuf, M> creator) {
    int registrationID = this.id++;
    this.logRegisterServerNetworkMessageHandler(type, networkMessage, registrationID);
    this.channel
        .messageBuilder(networkMessage, registrationID, NetworkDirection.PLAY_TO_SERVER)
        .encoder(M::write)
        .decoder(creator::apply)
        .consumerNetworkThread((message, context) -> {
          context.enqueueWork(() -> {
            if (message == null) {
              log.warn("Received null message, ignoring packet");
              return;
            }

            ServerPlayer sender = context.getSender();
            if (sender == null) {
              log.error("Unable to get valid player for network message {}", message);
              return;
            }

            NetworkMessageRecord.guardedHandler(type.id(), () -> message.handleServer(sender))
                .run();
          });
          context.setPacketHandled(true);
        })
        .add();
  }

  @Override
  public <M extends NetworkMessageRecord> void addClientMessage(
      final CustomPacketPayload.Type<M> messageID, final Class<M> networkMessage) {
    this.clientMessages.put(messageID, networkMessage);
  }

  @Override
  public <M extends NetworkMessageRecord> void addServerMessage(
      final CustomPacketPayload.Type<M> messageID, final Class<M> networkMessage) {
    this.serverMessages.put(messageID, networkMessage);
  }

  @Override
  public Map<CustomPacketPayload.Type<?>, Class<? extends NetworkMessageRecord>>
      getClientMessages() {
    return this.clientMessages;
  }

  @Override
  public Map<CustomPacketPayload.Type<?>, Class<? extends NetworkMessageRecord>>
      getServerMessages() {
    return this.serverMessages;
  }

  @Override
  public <M extends NetworkMessageRecord> void addRegisteredClientMessage(
      final CustomPacketPayload.Type<M> messageID, final Class<M> networkMessage) {
    this.registeredClientMessages.put(messageID, networkMessage);
  }

  @Override
  public <M extends NetworkMessageRecord> void addRegisteredServerMessage(
      final CustomPacketPayload.Type<M> messageID, final Class<M> networkMessage) {
    this.registeredServerMessages.put(messageID, networkMessage);
  }

  @Override
  public Map<CustomPacketPayload.Type<?>, Class<? extends NetworkMessageRecord>>
      getRegisteredClientMessages() {
    return this.registeredClientMessages;
  }

  @Override
  public Map<CustomPacketPayload.Type<?>, Class<? extends NetworkMessageRecord>>
      getRegisteredServerMessages() {
    return this.registeredServerMessages;
  }
}
