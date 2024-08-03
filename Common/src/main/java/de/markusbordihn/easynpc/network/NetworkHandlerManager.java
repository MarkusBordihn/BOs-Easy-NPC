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
import de.markusbordihn.easynpc.network.message.NetworkHandlerInterface;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import de.markusbordihn.easynpc.network.message.client.ExportClientPresetMessage;
import de.markusbordihn.easynpc.network.message.client.OpenMenuCallbackMessage;
import de.markusbordihn.easynpc.network.message.client.SyncDataMessage;
import de.markusbordihn.easynpc.network.message.server.AddObjectiveMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeActionEventMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeAdvancedTradingMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeBasicTradingMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeEntityAttributeMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeEntityBaseAttributeMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeModelEquipmentVisibilityMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeModelLockRotationMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeModelPoseMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeModelPositionMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeModelRotationMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeModelVisibilityMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeNameMessage;
import de.markusbordihn.easynpc.network.message.server.ChangePoseMessage;
import de.markusbordihn.easynpc.network.message.server.ChangePositionMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeProfessionMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeRendererMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeScaleMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeSkinMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeSpawnerSettingMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeTradingTypeMessage;
import de.markusbordihn.easynpc.network.message.server.ExecuteActionEventMessage;
import de.markusbordihn.easynpc.network.message.server.ExecuteDialogButtonActionMessage;
import de.markusbordihn.easynpc.network.message.server.ExportPresetMessage;
import de.markusbordihn.easynpc.network.message.server.ExportWorldPresetMessage;
import de.markusbordihn.easynpc.network.message.server.ImportPresetMessage;
import de.markusbordihn.easynpc.network.message.server.OpenActionDataEditorMessage;
import de.markusbordihn.easynpc.network.message.server.OpenActionDataEntryEditorMessage;
import de.markusbordihn.easynpc.network.message.server.OpenConfigurationMessage;
import de.markusbordihn.easynpc.network.message.server.OpenDialogButtonEditorMessage;
import de.markusbordihn.easynpc.network.message.server.OpenDialogEditorMessage;
import de.markusbordihn.easynpc.network.message.server.OpenDialogTextEditorMessage;
import de.markusbordihn.easynpc.network.message.server.OpenMenuMessage;
import de.markusbordihn.easynpc.network.message.server.RemoveDialogButtonMessage;
import de.markusbordihn.easynpc.network.message.server.RemoveDialogMessage;
import de.markusbordihn.easynpc.network.message.server.RemoveNPCMessage;
import de.markusbordihn.easynpc.network.message.server.RemoveObjectiveMessage;
import de.markusbordihn.easynpc.network.message.server.RequestDataSyncMessage;
import de.markusbordihn.easynpc.network.message.server.RespawnNPCMessage;
import de.markusbordihn.easynpc.network.message.server.SaveDialogButtonMessage;
import de.markusbordihn.easynpc.network.message.server.SaveDialogMessage;
import de.markusbordihn.easynpc.network.message.server.SaveDialogSetMessage;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NetworkHandlerManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static NetworkHandlerInterface networkHandler;

  private NetworkHandlerManager() {}

  public static void registerHandler(final NetworkHandlerInterface networkHandler) {
    log.info("{} Network Handler ...", Constants.LOG_REGISTER_PREFIX);
    NetworkHandlerManager.networkHandler = networkHandler;
  }

  public static NetworkHandlerInterface getHandler() {
    return networkHandler;
  }

  public static void sendToServer(NetworkMessageRecord networkMessageRecord) {
    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler != null) {
      networkHandler.sendToServer(networkMessageRecord);
    }
  }

  public static void sendToPlayer(
      NetworkMessageRecord networkMessageRecord, ServerPlayer serverPlayer) {
    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler != null) {
      networkHandler.sendToPlayer(networkMessageRecord, serverPlayer);
    }
  }

  public static void sendToAllPlayers(NetworkMessageRecord networkMessageRecord) {
    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler != null) {
      networkHandler.sendToAllPlayers(networkMessageRecord);
    }
  }

  public static void registerClientNetworkHandler() {

    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler == null) {
      log.error("Failed to register client network handler!");
      return;
    }
    log.info("Registering client network handler ...");

    networkHandler.registerClientNetworkMessageHandler(
        ExportClientPresetMessage.PAYLOAD_TYPE,
        ExportClientPresetMessage.STREAM_CODEC,
        ExportClientPresetMessage.class,
        ExportClientPresetMessage::create);

    networkHandler.registerClientNetworkMessageHandler(
        OpenMenuCallbackMessage.PAYLOAD_TYPE,
        OpenMenuCallbackMessage.STREAM_CODEC,
        OpenMenuCallbackMessage.class,
        OpenMenuCallbackMessage::create);

    networkHandler.registerClientNetworkMessageHandler(
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

    networkHandler.registerServerNetworkMessageHandler(
        AddObjectiveMessage.PAYLOAD_TYPE,
        AddObjectiveMessage.STREAM_CODEC,
        AddObjectiveMessage.class,
        AddObjectiveMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeActionEventMessage.PAYLOAD_TYPE,
        ChangeActionEventMessage.STREAM_CODEC,
        ChangeActionEventMessage.class,
        ChangeActionEventMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeAdvancedTradingMessage.PAYLOAD_TYPE,
        ChangeAdvancedTradingMessage.STREAM_CODEC,
        ChangeAdvancedTradingMessage.class,
        ChangeAdvancedTradingMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeBasicTradingMessage.PAYLOAD_TYPE,
        ChangeBasicTradingMessage.STREAM_CODEC,
        ChangeBasicTradingMessage.class,
        ChangeBasicTradingMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeEntityAttributeMessage.PAYLOAD_TYPE,
        ChangeEntityAttributeMessage.STREAM_CODEC,
        ChangeEntityAttributeMessage.class,
        ChangeEntityAttributeMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeEntityBaseAttributeMessage.PAYLOAD_TYPE,
        ChangeEntityBaseAttributeMessage.STREAM_CODEC,
        ChangeEntityBaseAttributeMessage.class,
        ChangeEntityBaseAttributeMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelEquipmentVisibilityMessage.PAYLOAD_TYPE,
        ChangeModelEquipmentVisibilityMessage.STREAM_CODEC,
        ChangeModelEquipmentVisibilityMessage.class,
        ChangeModelEquipmentVisibilityMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelLockRotationMessage.PAYLOAD_TYPE,
        ChangeModelLockRotationMessage.STREAM_CODEC,
        ChangeModelLockRotationMessage.class,
        ChangeModelLockRotationMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelPoseMessage.PAYLOAD_TYPE,
        ChangeModelPoseMessage.STREAM_CODEC,
        ChangeModelPoseMessage.class,
        ChangeModelPoseMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelPositionMessage.PAYLOAD_TYPE,
        ChangeModelPositionMessage.STREAM_CODEC,
        ChangeModelPositionMessage.class,
        ChangeModelPositionMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelRotationMessage.PAYLOAD_TYPE,
        ChangeModelRotationMessage.STREAM_CODEC,
        ChangeModelRotationMessage.class,
        ChangeModelRotationMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelVisibilityMessage.PAYLOAD_TYPE,
        ChangeModelVisibilityMessage.STREAM_CODEC,
        ChangeModelVisibilityMessage.class,
        ChangeModelVisibilityMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeNameMessage.PAYLOAD_TYPE,
        ChangeNameMessage.STREAM_CODEC,
        ChangeNameMessage.class,
        ChangeNameMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangePoseMessage.PAYLOAD_TYPE,
        ChangePoseMessage.STREAM_CODEC,
        ChangePoseMessage.class,
        ChangePoseMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangePositionMessage.PAYLOAD_TYPE,
        ChangePositionMessage.STREAM_CODEC,
        ChangePositionMessage.class,
        ChangePositionMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeProfessionMessage.PAYLOAD_TYPE,
        ChangeProfessionMessage.STREAM_CODEC,
        ChangeProfessionMessage.class,
        ChangeProfessionMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeRendererMessage.PAYLOAD_TYPE,
        ChangeRendererMessage.STREAM_CODEC,
        ChangeRendererMessage.class,
        ChangeRendererMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeScaleMessage.PAYLOAD_TYPE,
        ChangeScaleMessage.STREAM_CODEC,
        ChangeScaleMessage.class,
        ChangeScaleMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeSkinMessage.PAYLOAD_TYPE,
        ChangeSkinMessage.STREAM_CODEC,
        ChangeSkinMessage.class,
        ChangeSkinMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeSpawnerSettingMessage.PAYLOAD_TYPE,
        ChangeSpawnerSettingMessage.STREAM_CODEC,
        ChangeSpawnerSettingMessage.class,
        ChangeSpawnerSettingMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeTradingTypeMessage.PAYLOAD_TYPE,
        ChangeTradingTypeMessage.STREAM_CODEC,
        ChangeTradingTypeMessage.class,
        ChangeTradingTypeMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ExecuteActionEventMessage.PAYLOAD_TYPE,
        ExecuteActionEventMessage.STREAM_CODEC,
        ExecuteActionEventMessage.class,
        ExecuteActionEventMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ExecuteDialogButtonActionMessage.PAYLOAD_TYPE,
        ExecuteDialogButtonActionMessage.STREAM_CODEC,
        ExecuteDialogButtonActionMessage.class,
        ExecuteDialogButtonActionMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ExportPresetMessage.PAYLOAD_TYPE,
        ExportPresetMessage.STREAM_CODEC,
        ExportPresetMessage.class,
        ExportPresetMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ExportWorldPresetMessage.PAYLOAD_TYPE,
        ExportWorldPresetMessage.STREAM_CODEC,
        ExportWorldPresetMessage.class,
        ExportWorldPresetMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ImportPresetMessage.PAYLOAD_TYPE,
        ImportPresetMessage.STREAM_CODEC,
        ImportPresetMessage.class,
        ImportPresetMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenActionDataEditorMessage.PAYLOAD_TYPE,
        OpenActionDataEditorMessage.STREAM_CODEC,
        OpenActionDataEditorMessage.class,
        OpenActionDataEditorMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenActionDataEntryEditorMessage.PAYLOAD_TYPE,
        OpenActionDataEntryEditorMessage.STREAM_CODEC,
        OpenActionDataEntryEditorMessage.class,
        OpenActionDataEntryEditorMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenConfigurationMessage.PAYLOAD_TYPE,
        OpenConfigurationMessage.STREAM_CODEC,
        OpenConfigurationMessage.class,
        OpenConfigurationMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenDialogButtonEditorMessage.PAYLOAD_TYPE,
        OpenDialogButtonEditorMessage.STREAM_CODEC,
        OpenDialogButtonEditorMessage.class,
        OpenDialogButtonEditorMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenDialogEditorMessage.PAYLOAD_TYPE,
        OpenDialogEditorMessage.STREAM_CODEC,
        OpenDialogEditorMessage.class,
        OpenDialogEditorMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenMenuMessage.PAYLOAD_TYPE,
        OpenMenuMessage.STREAM_CODEC,
        OpenMenuMessage.class,
        OpenMenuMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenDialogTextEditorMessage.PAYLOAD_TYPE,
        OpenDialogTextEditorMessage.STREAM_CODEC,
        OpenDialogTextEditorMessage.class,
        OpenDialogTextEditorMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        RemoveDialogButtonMessage.PAYLOAD_TYPE,
        RemoveDialogButtonMessage.STREAM_CODEC,
        RemoveDialogButtonMessage.class,
        RemoveDialogButtonMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        RemoveDialogMessage.PAYLOAD_TYPE,
        RemoveDialogMessage.STREAM_CODEC,
        RemoveDialogMessage.class,
        RemoveDialogMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        RemoveNPCMessage.PAYLOAD_TYPE,
        RemoveNPCMessage.STREAM_CODEC,
        RemoveNPCMessage.class,
        RemoveNPCMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        RemoveObjectiveMessage.PAYLOAD_TYPE,
        RemoveObjectiveMessage.STREAM_CODEC,
        RemoveObjectiveMessage.class,
        RemoveObjectiveMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        RequestDataSyncMessage.PAYLOAD_TYPE,
        RequestDataSyncMessage.STREAM_CODEC,
        RequestDataSyncMessage.class,
        RequestDataSyncMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        RespawnNPCMessage.PAYLOAD_TYPE,
        RespawnNPCMessage.STREAM_CODEC,
        RespawnNPCMessage.class,
        RespawnNPCMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        SaveDialogButtonMessage.PAYLOAD_TYPE,
        SaveDialogButtonMessage.STREAM_CODEC,
        SaveDialogButtonMessage.class,
        SaveDialogButtonMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        SaveDialogMessage.PAYLOAD_TYPE,
        SaveDialogMessage.STREAM_CODEC,
        SaveDialogMessage.class,
        SaveDialogMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        SaveDialogSetMessage.PAYLOAD_TYPE,
        SaveDialogSetMessage.STREAM_CODEC,
        SaveDialogSetMessage.class,
        SaveDialogSetMessage::create);
  }
}
