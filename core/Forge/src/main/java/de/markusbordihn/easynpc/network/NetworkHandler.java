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
import java.util.Optional;
import java.util.function.BooleanSupplier;
import java.util.function.Function;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.network.NetworkDirection;
import net.minecraftforge.network.NetworkEvent;
import net.minecraftforge.network.NetworkRegistry;
import net.minecraftforge.network.PacketDistributor;
import net.minecraftforge.network.simple.SimpleChannel;

public class NetworkHandler implements NetworkHandlerInterface {

  private final SimpleChannel channel;
  private final BooleanSupplier clientNetworkHandler;
  private final BooleanSupplier serverNetworkHandler;
  private final Map<ResourceLocation, Class<? extends NetworkMessageRecord>> clientMessages =
      new LinkedHashMap<>();
  private final Map<ResourceLocation, Class<? extends NetworkMessageRecord>> serverMessages =
      new LinkedHashMap<>();
  private final Map<ResourceLocation, Class<? extends NetworkMessageRecord>>
      registeredClientMessages = new LinkedHashMap<>();
  private final Map<ResourceLocation, Class<? extends NetworkMessageRecord>>
      registeredServerMessages = new LinkedHashMap<>();
  private int id = 0;

  public NetworkHandler(
      final String modId,
      final BooleanSupplier clientNetworkHandler,
      final BooleanSupplier serverNetworkHandler) {
    this.channel =
        NetworkRegistry.newSimpleChannel(
            new ResourceLocation(modId, "network"),
            () -> String.valueOf(PROTOCOL_VERSION),
            String.valueOf(PROTOCOL_VERSION)::equals,
            String.valueOf(PROTOCOL_VERSION)::equals);
    this.clientNetworkHandler = clientNetworkHandler;
    this.serverNetworkHandler = serverNetworkHandler;
    log.info(
        "{} Network Handler for {} with version {}",
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
  public void sendToServer(final NetworkMessageRecord networkMessageRecord) {
    DistExecutor.unsafeRunWhenOn(
        Dist.CLIENT, () -> () -> this.channel.sendToServer(networkMessageRecord));
  }

  @Override
  public void sendToPlayer(
      final NetworkMessageRecord networkMessageRecord, final ServerPlayer serverPlayer) {
    this.channel.send(PacketDistributor.PLAYER.with(() -> serverPlayer), networkMessageRecord);
  }

  @Override
  public <M extends NetworkMessageRecord> void registerClientNetworkMessageHandler(
      final ResourceLocation messageID,
      final Class<M> networkMessage,
      final Function<FriendlyByteBuf, M> creator) {
    int registrationID = this.id++;
    this.logRegisterClientNetworkMessageHandler(messageID, networkMessage, registrationID);
    this.channel.registerMessage(
        registrationID,
        networkMessage,
        M::write,
        NetworkMessageRecord.guardedDecoder(messageID, creator),
        (message, contextSupplier) -> {
          NetworkEvent.Context context = contextSupplier.get();
          context.enqueueWork(
              NetworkMessageRecord.guardedHandler(
                  messageID,
                  () -> DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> message::handleClient)));
          context.setPacketHandled(true);
        },
        Optional.of(NetworkDirection.PLAY_TO_CLIENT));
  }

  @Override
  public <M extends NetworkMessageRecord> void registerServerNetworkMessageHandler(
      final ResourceLocation messageID,
      final Class<M> networkMessage,
      final Function<FriendlyByteBuf, M> creator) {
    int registrationID = this.id++;
    this.logRegisterServerNetworkMessageHandler(messageID, networkMessage, registrationID);
    this.channel.registerMessage(
        registrationID,
        networkMessage,
        M::write,
        NetworkMessageRecord.guardedDecoder(messageID, creator),
        (message, contextSupplier) -> {
          NetworkEvent.Context context = contextSupplier.get();
          context.enqueueWork(
              NetworkMessageRecord.guardedHandler(
                  messageID, () -> message.handleServer(context.getSender())));
          context.setPacketHandled(true);
        },
        Optional.of(NetworkDirection.PLAY_TO_SERVER));
  }

  @Override
  public <M extends NetworkMessageRecord> void addClientMessage(
      final ResourceLocation messageID, final Class<M> networkMessage) {
    this.clientMessages.put(messageID, networkMessage);
  }

  @Override
  public <M extends NetworkMessageRecord> void addServerMessage(
      final ResourceLocation messageID, final Class<M> networkMessage) {
    this.serverMessages.put(messageID, networkMessage);
  }

  @Override
  public Map<ResourceLocation, Class<? extends NetworkMessageRecord>> getClientMessages() {
    return this.clientMessages;
  }

  @Override
  public Map<ResourceLocation, Class<? extends NetworkMessageRecord>> getServerMessages() {
    return this.serverMessages;
  }

  @Override
  public <M extends NetworkMessageRecord> void addRegisteredClientMessage(
      final ResourceLocation messageID, final Class<M> networkMessage) {
    this.registeredClientMessages.put(messageID, networkMessage);
  }

  @Override
  public <M extends NetworkMessageRecord> void addRegisteredServerMessage(
      final ResourceLocation messageID, final Class<M> networkMessage) {
    this.registeredServerMessages.put(messageID, networkMessage);
  }

  @Override
  public Map<ResourceLocation, Class<? extends NetworkMessageRecord>>
      getRegisteredClientMessages() {
    return this.registeredClientMessages;
  }

  @Override
  public Map<ResourceLocation, Class<? extends NetworkMessageRecord>>
      getRegisteredServerMessages() {
    return this.registeredServerMessages;
  }
}
