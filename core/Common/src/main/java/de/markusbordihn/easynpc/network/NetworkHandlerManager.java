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
import de.markusbordihn.easynpc.network.message.client.OpenMenuCallbackMessage;
import de.markusbordihn.easynpc.network.message.client.SyncDataMessage;
import de.markusbordihn.easynpc.network.message.server.ExecuteActionEventMessage;
import de.markusbordihn.easynpc.network.message.server.ExecuteDialogButtonActionMessage;
import de.markusbordihn.easynpc.network.message.server.OpenMenuMessage;
import de.markusbordihn.easynpc.network.message.server.RequestDataSyncMessage;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NetworkHandlerManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static volatile NetworkHandlerInterface networkHandler;
  private static volatile NetworkHandlerManagerType networkHandlerManagerType =
      NetworkHandlerManagerType.BOTH;

  private NetworkHandlerManager() {}

  public static void registerHandler(final NetworkHandlerInterface networkHandler) {
    log.info("{} Network Handler ...", Constants.LOG_REGISTER_PREFIX);
    NetworkHandlerManager.networkHandler = networkHandler;
  }

  public static NetworkHandlerInterface getHandler() {
    return networkHandler;
  }

  public static void registerNetworkMessages(NetworkHandlerManagerType networkHandlerType) {
    log.info("Registering network messages for {} side ...", networkHandlerType);
    networkHandlerManagerType = networkHandlerType;
    registerClientNetworkHandler();
    registerServerNetworkHandler();
  }

  public static boolean isClientNetworkHandler() {
    return networkHandlerManagerType == NetworkHandlerManagerType.CLIENT
        || networkHandlerManagerType == NetworkHandlerManagerType.BOTH;
  }

  public static boolean isServerNetworkHandler() {
    return networkHandlerManagerType == NetworkHandlerManagerType.SERVER
        || networkHandlerManagerType == NetworkHandlerManagerType.BOTH;
  }

  public static void sendMessageToServer(NetworkMessageRecord networkMessageRecord) {
    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler != null) {
      networkHandler.sendMessageToServer(networkMessageRecord);
    }
  }

  public static void sendMessageToPlayer(
      NetworkMessageRecord networkMessageRecord, ServerPlayer serverPlayer) {
    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler != null) {
      networkHandler.sendMessageToPlayer(networkMessageRecord, serverPlayer);
    }
  }

  public static void registerNetworkHandler() {
    registerClientNetworkHandler();
    registerServerNetworkHandler();
  }

  public static void registerClientNetworkHandler() {

    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler == null) {
      log.error("Failed to register client network handler!");
      return;
    }
    log.info("Registering client network handler ...");

    networkHandler.registerClientNetworkMessage(
        OpenMenuCallbackMessage.PAYLOAD_TYPE,
        OpenMenuCallbackMessage.STREAM_CODEC,
        OpenMenuCallbackMessage.class,
        OpenMenuCallbackMessage::create);

    networkHandler.registerClientNetworkMessage(
        SyncDataMessage.PAYLOAD_TYPE,
        SyncDataMessage.STREAM_CODEC,
        SyncDataMessage.class,
        SyncDataMessage::create);
  }

  public static void registerServerNetworkHandler() {

    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler == null) {
      log.error("Failed to register server network handler!");
      return;
    }
    log.info("Registering server network handler ...");

    networkHandler.registerServerNetworkMessage(
        ExecuteActionEventMessage.PAYLOAD_TYPE,
        ExecuteActionEventMessage.STREAM_CODEC,
        ExecuteActionEventMessage.class,
        ExecuteActionEventMessage::create);

    networkHandler.registerServerNetworkMessage(
        ExecuteDialogButtonActionMessage.PAYLOAD_TYPE,
        ExecuteDialogButtonActionMessage.STREAM_CODEC,
        ExecuteDialogButtonActionMessage.class,
        ExecuteDialogButtonActionMessage::create);

    networkHandler.registerServerNetworkMessage(
        OpenMenuMessage.PAYLOAD_TYPE,
        OpenMenuMessage.STREAM_CODEC,
        OpenMenuMessage.class,
        OpenMenuMessage::create);

    networkHandler.registerServerNetworkMessage(
        RequestDataSyncMessage.PAYLOAD_TYPE,
        RequestDataSyncMessage.STREAM_CODEC,
        RequestDataSyncMessage.class,
        RequestDataSyncMessage::create);
  }
}
