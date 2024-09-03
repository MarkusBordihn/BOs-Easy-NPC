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
import de.markusbordihn.easynpc.network.message.client.ExportClientPresetMessage;
import de.markusbordihn.easynpc.network.message.client.OpenMenuCallbackMessage;
import de.markusbordihn.easynpc.network.message.server.AddOrUpdateObjectiveMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeActionEventMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeAdvancedTradingMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeBasicTradingMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeDisplayAttributeMessage;
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

  public static void registerClientNetworkHandler() {

    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler == null) {
      log.error("Failed to register client network handler!");
      return;
    }
    log.info("Registering client network handler ...");

    networkHandler.registerClientNetworkMessageHandler(
        ExportClientPresetMessage.MESSAGE_ID,
        ExportClientPresetMessage.class,
        ExportClientPresetMessage::create);
    networkHandler.registerClientNetworkMessageHandler(
        OpenMenuCallbackMessage.MESSAGE_ID,
        OpenMenuCallbackMessage.class,
        OpenMenuCallbackMessage::create);
  }

  public static void registerServerNetworkHandler() {

    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler == null) {
      log.error("Failed to register server network handler!");
      return;
    }
    log.info("Registering server network handler ...");

    networkHandler.registerServerNetworkMessageHandler(
        AddOrUpdateObjectiveMessage.MESSAGE_ID,
        AddOrUpdateObjectiveMessage.class,
        AddOrUpdateObjectiveMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeActionEventMessage.MESSAGE_ID,
        ChangeActionEventMessage.class,
        ChangeActionEventMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeAdvancedTradingMessage.MESSAGE_ID,
        ChangeAdvancedTradingMessage.class,
        ChangeAdvancedTradingMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeBasicTradingMessage.MESSAGE_ID,
        ChangeBasicTradingMessage.class,
        ChangeBasicTradingMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeDisplayAttributeMessage.MESSAGE_ID,
        ChangeDisplayAttributeMessage.class,
        ChangeDisplayAttributeMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeEntityAttributeMessage.MESSAGE_ID,
        ChangeEntityAttributeMessage.class,
        ChangeEntityAttributeMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeEntityBaseAttributeMessage.MESSAGE_ID,
        ChangeEntityBaseAttributeMessage.class,
        ChangeEntityBaseAttributeMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelEquipmentVisibilityMessage.MESSAGE_ID,
        ChangeModelEquipmentVisibilityMessage.class,
        ChangeModelEquipmentVisibilityMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelLockRotationMessage.MESSAGE_ID,
        ChangeModelLockRotationMessage.class,
        ChangeModelLockRotationMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelPoseMessage.MESSAGE_ID,
        ChangeModelPoseMessage.class,
        ChangeModelPoseMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelPositionMessage.MESSAGE_ID,
        ChangeModelPositionMessage.class,
        ChangeModelPositionMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelRotationMessage.MESSAGE_ID,
        ChangeModelRotationMessage.class,
        ChangeModelRotationMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelVisibilityMessage.MESSAGE_ID,
        ChangeModelVisibilityMessage.class,
        ChangeModelVisibilityMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeNameMessage.MESSAGE_ID, ChangeNameMessage.class, ChangeNameMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangePoseMessage.MESSAGE_ID, ChangePoseMessage.class, ChangePoseMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangePositionMessage.MESSAGE_ID,
        ChangePositionMessage.class,
        ChangePositionMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeProfessionMessage.MESSAGE_ID,
        ChangeProfessionMessage.class,
        ChangeProfessionMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeRendererMessage.MESSAGE_ID,
        ChangeRendererMessage.class,
        ChangeRendererMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeScaleMessage.MESSAGE_ID, ChangeScaleMessage.class, ChangeScaleMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeSkinMessage.MESSAGE_ID, ChangeSkinMessage.class, ChangeSkinMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeSpawnerSettingMessage.MESSAGE_ID,
        ChangeSpawnerSettingMessage.class,
        ChangeSpawnerSettingMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeTradingTypeMessage.MESSAGE_ID,
        ChangeTradingTypeMessage.class,
        ChangeTradingTypeMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ExecuteActionEventMessage.MESSAGE_ID,
        ExecuteActionEventMessage.class,
        ExecuteActionEventMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ExecuteDialogButtonActionMessage.MESSAGE_ID,
        ExecuteDialogButtonActionMessage.class,
        ExecuteDialogButtonActionMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ExportPresetMessage.MESSAGE_ID, ExportPresetMessage.class, ExportPresetMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ExportWorldPresetMessage.MESSAGE_ID,
        ExportWorldPresetMessage.class,
        ExportWorldPresetMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        ImportPresetMessage.MESSAGE_ID, ImportPresetMessage.class, ImportPresetMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenActionDataEditorMessage.MESSAGE_ID,
        OpenActionDataEditorMessage.class,
        OpenActionDataEditorMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenActionDataEntryEditorMessage.MESSAGE_ID,
        OpenActionDataEntryEditorMessage.class,
        OpenActionDataEntryEditorMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenConfigurationMessage.MESSAGE_ID,
        OpenConfigurationMessage.class,
        OpenConfigurationMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenDialogButtonEditorMessage.MESSAGE_ID,
        OpenDialogButtonEditorMessage.class,
        OpenDialogButtonEditorMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenDialogEditorMessage.MESSAGE_ID,
        OpenDialogEditorMessage.class,
        OpenDialogEditorMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenMenuMessage.MESSAGE_ID, OpenMenuMessage.class, OpenMenuMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        OpenDialogTextEditorMessage.MESSAGE_ID,
        OpenDialogTextEditorMessage.class,
        OpenDialogTextEditorMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        RemoveDialogButtonMessage.MESSAGE_ID,
        RemoveDialogButtonMessage.class,
        RemoveDialogButtonMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        RemoveDialogMessage.MESSAGE_ID, RemoveDialogMessage.class, RemoveDialogMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        RemoveNPCMessage.MESSAGE_ID, RemoveNPCMessage.class, RemoveNPCMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        RemoveObjectiveMessage.MESSAGE_ID,
        RemoveObjectiveMessage.class,
        RemoveObjectiveMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        RequestDataSyncMessage.MESSAGE_ID,
        RequestDataSyncMessage.class,
        RequestDataSyncMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        RespawnNPCMessage.MESSAGE_ID, RespawnNPCMessage.class, RespawnNPCMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        SaveDialogButtonMessage.MESSAGE_ID,
        SaveDialogButtonMessage.class,
        SaveDialogButtonMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        SaveDialogMessage.MESSAGE_ID, SaveDialogMessage.class, SaveDialogMessage::create);

    networkHandler.registerServerNetworkMessageHandler(
        SaveDialogSetMessage.MESSAGE_ID, SaveDialogSetMessage.class, SaveDialogSetMessage::create);
  }
}
