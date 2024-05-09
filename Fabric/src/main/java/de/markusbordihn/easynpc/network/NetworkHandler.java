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
import de.markusbordihn.easynpc.network.message.CacheDataSyncMessage;
import de.markusbordihn.easynpc.network.message.ChangeSpawnerSettingMessage;
import de.markusbordihn.easynpc.network.message.DialogButtonActionMessage;
import de.markusbordihn.easynpc.network.message.TriggerActionEventMessage;
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking;
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NetworkHandler {
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private NetworkHandler() {}

  public static void registerClientNetworkHandler() {

    ClientPlayNetworking.registerGlobalReceiver(
        CacheDataSyncMessage.MESSAGE_ID,
        (client, handler, buffer, responseSender) -> CacheDataSyncMessage.handle(buffer, null));
  }

  public static void registerServerNetworkHandler() {

    ServerPlayNetworking.registerGlobalReceiver(
        TriggerActionEventMessage.MESSAGE_ID,
        (server, serverPlayer, handler, buffer, responseSender) ->
            TriggerActionEventMessage.handle(buffer, serverPlayer));

    ServerPlayNetworking.registerGlobalReceiver(
        ChangeSpawnerSettingMessage.MESSAGE_ID,
        (server, serverPlayer, handler, buffer, responseSender) ->
            ChangeSpawnerSettingMessage.handle(buffer, serverPlayer));

    ServerPlayNetworking.registerGlobalReceiver(
        DialogButtonActionMessage.MESSAGE_ID,
        (server, serverPlayer, handler, buffer, responseSender) ->
            DialogButtonActionMessage.handle(buffer, serverPlayer));
  }

  public static void sendToServer(ResourceLocation messageId, FriendlyByteBuf buffer) {
    ClientPlayNetworking.send(messageId, buffer);
  }

  public static void sendToPlayer(
      ServerPlayer serverPlayer, ResourceLocation messageId, FriendlyByteBuf buffer) {
    ServerPlayNetworking.send(serverPlayer, messageId, buffer);
  }
}
