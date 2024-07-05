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
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.network.NetworkDirection;
import net.minecraftforge.network.NetworkEvent;
import net.minecraftforge.network.NetworkRegistry;
import net.minecraftforge.network.PacketDistributor;
import net.minecraftforge.network.simple.SimpleChannel;

@EventBusSubscriber
public class NetworkHandler implements NetworkHandlerInterface {

  private static final String PROTOCOL_VERSION = "21";
  public static final SimpleChannel INSTANCE =
      NetworkRegistry.newSimpleChannel(
          new ResourceLocation(Constants.MOD_ID, "network"),
          () -> PROTOCOL_VERSION,
          PROTOCOL_VERSION::equals,
          PROTOCOL_VERSION::equals);

  private static int id = 0;

  public NetworkHandler() {
    log.info("{} NetworkHandler ...", Constants.LOG_REGISTER_PREFIX);
  }

  public static void registerNetworkHandler(final FMLCommonSetupEvent event) {
    log.info(
        "{} Network Handler for {} with version {} ...",
        Constants.LOG_REGISTER_PREFIX,
        INSTANCE,
        PROTOCOL_VERSION);

    event.enqueueWork(
        () -> {
          NetworkHandlerManager.registerClientNetworkHandler();
          NetworkHandlerManager.registerServerNetworkHandler();
        });
  }

  @Override
  public <M extends NetworkMessage> void sendToServer(
      ResourceLocation messageId, M networkMessage) {
    try {
      INSTANCE.sendToServer(networkMessage);
    } catch (Exception e) {
      log.error("Failed to send {} to server, got error: {}", networkMessage, e.getMessage());
    }
  }

  @Override
  public <M extends NetworkMessage> void sendToPlayer(
      ResourceLocation messageId, M networkMessage, ServerPlayer serverPlayer) {
    try {
      INSTANCE.send(PacketDistributor.PLAYER.with(() -> serverPlayer), networkMessage);
    } catch (Exception e) {
      log.error(
          "Failed to send {} to player {}, got error: {}",
          networkMessage,
          serverPlayer.getName().getString(),
          e.getMessage());
    }
  }

  @Override
  public <M> void registerClientNetworkMessageHandler(
      ResourceLocation messageID,
      Class<M> networkMessage,
      BiConsumer<M, FriendlyByteBuf> encoder,
      Function<FriendlyByteBuf, M> decoder,
      Consumer<M> handler) {
    INSTANCE.registerMessage(
        id++,
        networkMessage,
        encoder,
        decoder,
        (message, contextSupplier) -> {
          NetworkEvent.Context context = contextSupplier.get();
          context.enqueueWork(
              () -> {
                handler.accept(message);
                context.setPacketHandled(true);
              });
        },
        Optional.of(NetworkDirection.PLAY_TO_CLIENT));
  }

  @Override
  public <M> void registerServerNetworkMessageHandler(
      ResourceLocation messageID,
      Class<M> networkMessage,
      BiConsumer<M, FriendlyByteBuf> encoder,
      Function<FriendlyByteBuf, M> decoder,
      BiConsumer<M, ServerPlayer> handler) {
    INSTANCE.registerMessage(
        id++,
        networkMessage,
        encoder,
        decoder,
        (message, contextSupplier) -> {
          NetworkEvent.Context context = contextSupplier.get();
          context.enqueueWork(
              () -> {
                handler.accept(message, context.getSender());
                context.setPacketHandled(true);
              });
        },
        Optional.of(NetworkDirection.PLAY_TO_SERVER));
  }
}
