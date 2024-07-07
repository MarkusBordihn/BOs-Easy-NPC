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
import de.markusbordihn.easynpc.network.message.client.ExportClientPresetMessage;
import de.markusbordihn.easynpc.network.message.client.OpenMenuCallbackMessage;
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
import de.markusbordihn.easynpc.network.message.server.RespawnNPCMessage;
import de.markusbordihn.easynpc.network.message.server.SaveDialogButtonMessage;
import de.markusbordihn.easynpc.network.message.server.SaveDialogMessage;
import de.markusbordihn.easynpc.network.message.server.SaveDialogSetMessage;
import net.minecraft.resources.ResourceLocation;
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

  public static <M extends NetworkMessage> void sendToServer(
      ResourceLocation messageId, M networkMessage) {
    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler != null) {
      networkHandler.sendToServer(messageId, networkMessage);
    }
  }

  public static <M extends NetworkMessage> void sendToPlayer(
      ResourceLocation messageId, M networkMessage, ServerPlayer serverPlayer) {
    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler != null) {
      networkHandler.sendToPlayer(messageId, networkMessage, serverPlayer);
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
        ExportClientPresetMessage::encode,
        ExportClientPresetMessage::decode,
        ExportClientPresetMessage::handle);

    networkHandler.registerClientNetworkMessageHandler(
        OpenMenuCallbackMessage.MESSAGE_ID,
        OpenMenuCallbackMessage.class,
        OpenMenuCallbackMessage::encode,
        OpenMenuCallbackMessage::decode,
        OpenMenuCallbackMessage::handle);
  }

  public static void registerServerNetworkHandler() {

    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler == null) {
      log.error("Failed to register server network handler!");
      return;
    }
    log.info("Registering server network handler ...");

    networkHandler.registerServerNetworkMessageHandler(
        AddObjectiveMessage.MESSAGE_ID,
        AddObjectiveMessage.class,
        AddObjectiveMessage::encode,
        AddObjectiveMessage::decode,
        AddObjectiveMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeActionEventMessage.MESSAGE_ID,
        ChangeActionEventMessage.class,
        ChangeActionEventMessage::encode,
        ChangeActionEventMessage::decode,
        ChangeActionEventMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeAdvancedTradingMessage.MESSAGE_ID,
        ChangeAdvancedTradingMessage.class,
        ChangeAdvancedTradingMessage::encode,
        ChangeAdvancedTradingMessage::decode,
        ChangeAdvancedTradingMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeBasicTradingMessage.MESSAGE_ID,
        ChangeBasicTradingMessage.class,
        ChangeBasicTradingMessage::encode,
        ChangeBasicTradingMessage::decode,
        ChangeBasicTradingMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeEntityAttributeMessage.MESSAGE_ID,
        ChangeEntityAttributeMessage.class,
        ChangeEntityAttributeMessage::encode,
        ChangeEntityAttributeMessage::decode,
        ChangeEntityAttributeMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeEntityBaseAttributeMessage.MESSAGE_ID,
        ChangeEntityBaseAttributeMessage.class,
        ChangeEntityBaseAttributeMessage::encode,
        ChangeEntityBaseAttributeMessage::decode,
        ChangeEntityBaseAttributeMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelEquipmentVisibilityMessage.MESSAGE_ID,
        ChangeModelEquipmentVisibilityMessage.class,
        ChangeModelEquipmentVisibilityMessage::encode,
        ChangeModelEquipmentVisibilityMessage::decode,
        ChangeModelEquipmentVisibilityMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelLockRotationMessage.MESSAGE_ID,
        ChangeModelLockRotationMessage.class,
        ChangeModelLockRotationMessage::encode,
        ChangeModelLockRotationMessage::decode,
        ChangeModelLockRotationMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelPoseMessage.MESSAGE_ID,
        ChangeModelPoseMessage.class,
        ChangeModelPoseMessage::encode,
        ChangeModelPoseMessage::decode,
        ChangeModelPoseMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelPositionMessage.MESSAGE_ID,
        ChangeModelPositionMessage.class,
        ChangeModelPositionMessage::encode,
        ChangeModelPositionMessage::decode,
        ChangeModelPositionMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelRotationMessage.MESSAGE_ID,
        ChangeModelRotationMessage.class,
        ChangeModelRotationMessage::encode,
        ChangeModelRotationMessage::decode,
        ChangeModelRotationMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeModelVisibilityMessage.MESSAGE_ID,
        ChangeModelVisibilityMessage.class,
        ChangeModelVisibilityMessage::encode,
        ChangeModelVisibilityMessage::decode,
        ChangeModelVisibilityMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeNameMessage.MESSAGE_ID,
        ChangeNameMessage.class,
        ChangeNameMessage::encode,
        ChangeNameMessage::decode,
        ChangeNameMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangePoseMessage.MESSAGE_ID,
        ChangePoseMessage.class,
        ChangePoseMessage::encode,
        ChangePoseMessage::decode,
        ChangePoseMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangePositionMessage.MESSAGE_ID,
        ChangePositionMessage.class,
        ChangePositionMessage::encode,
        ChangePositionMessage::decode,
        ChangePositionMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeProfessionMessage.MESSAGE_ID,
        ChangeProfessionMessage.class,
        ChangeProfessionMessage::encode,
        ChangeProfessionMessage::decode,
        ChangeProfessionMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeScaleMessage.MESSAGE_ID,
        ChangeScaleMessage.class,
        ChangeScaleMessage::encode,
        ChangeScaleMessage::decode,
        ChangeScaleMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeSkinMessage.MESSAGE_ID,
        ChangeSkinMessage.class,
        ChangeSkinMessage::encode,
        ChangeSkinMessage::decode,
        ChangeSkinMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeSpawnerSettingMessage.MESSAGE_ID,
        ChangeSpawnerSettingMessage.class,
        ChangeSpawnerSettingMessage::encode,
        ChangeSpawnerSettingMessage::decode,
        ChangeSpawnerSettingMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ChangeTradingTypeMessage.MESSAGE_ID,
        ChangeTradingTypeMessage.class,
        ChangeTradingTypeMessage::encode,
        ChangeTradingTypeMessage::decode,
        ChangeTradingTypeMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ExecuteActionEventMessage.MESSAGE_ID,
        ExecuteActionEventMessage.class,
        ExecuteActionEventMessage::encode,
        ExecuteActionEventMessage::decode,
        ExecuteActionEventMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ExecuteDialogButtonActionMessage.MESSAGE_ID,
        ExecuteDialogButtonActionMessage.class,
        ExecuteDialogButtonActionMessage::encode,
        ExecuteDialogButtonActionMessage::decode,
        ExecuteDialogButtonActionMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ExportPresetMessage.MESSAGE_ID,
        ExportPresetMessage.class,
        ExportPresetMessage::encode,
        ExportPresetMessage::decode,
        ExportPresetMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ExportWorldPresetMessage.MESSAGE_ID,
        ExportWorldPresetMessage.class,
        ExportWorldPresetMessage::encode,
        ExportWorldPresetMessage::decode,
        ExportWorldPresetMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        ImportPresetMessage.MESSAGE_ID,
        ImportPresetMessage.class,
        ImportPresetMessage::encode,
        ImportPresetMessage::decode,
        ImportPresetMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        OpenActionDataEditorMessage.MESSAGE_ID,
        OpenActionDataEditorMessage.class,
        OpenActionDataEditorMessage::encode,
        OpenActionDataEditorMessage::decode,
        OpenActionDataEditorMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        OpenActionDataEntryEditorMessage.MESSAGE_ID,
        OpenActionDataEntryEditorMessage.class,
        OpenActionDataEntryEditorMessage::encode,
        OpenActionDataEntryEditorMessage::decode,
        OpenActionDataEntryEditorMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        OpenConfigurationMessage.MESSAGE_ID,
        OpenConfigurationMessage.class,
        OpenConfigurationMessage::encode,
        OpenConfigurationMessage::decode,
        OpenConfigurationMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        OpenDialogButtonEditorMessage.MESSAGE_ID,
        OpenDialogButtonEditorMessage.class,
        OpenDialogButtonEditorMessage::encode,
        OpenDialogButtonEditorMessage::decode,
        OpenDialogButtonEditorMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        OpenDialogEditorMessage.MESSAGE_ID,
        OpenDialogEditorMessage.class,
        OpenDialogEditorMessage::encode,
        OpenDialogEditorMessage::decode,
        OpenDialogEditorMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        OpenMenuMessage.MESSAGE_ID,
        OpenMenuMessage.class,
        OpenMenuMessage::encode,
        OpenMenuMessage::decode,
        OpenMenuMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        OpenDialogTextEditorMessage.MESSAGE_ID,
        OpenDialogTextEditorMessage.class,
        OpenDialogTextEditorMessage::encode,
        OpenDialogTextEditorMessage::decode,
        OpenDialogTextEditorMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        RemoveDialogButtonMessage.MESSAGE_ID,
        RemoveDialogButtonMessage.class,
        RemoveDialogButtonMessage::encode,
        RemoveDialogButtonMessage::decode,
        RemoveDialogButtonMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        RemoveDialogMessage.MESSAGE_ID,
        RemoveDialogMessage.class,
        RemoveDialogMessage::encode,
        RemoveDialogMessage::decode,
        RemoveDialogMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        RemoveNPCMessage.MESSAGE_ID,
        RemoveNPCMessage.class,
        RemoveNPCMessage::encode,
        RemoveNPCMessage::decode,
        RemoveNPCMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        RemoveObjectiveMessage.MESSAGE_ID,
        RemoveObjectiveMessage.class,
        RemoveObjectiveMessage::encode,
        RemoveObjectiveMessage::decode,
        RemoveObjectiveMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        RespawnNPCMessage.MESSAGE_ID,
        RespawnNPCMessage.class,
        RespawnNPCMessage::encode,
        RespawnNPCMessage::decode,
        RespawnNPCMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        SaveDialogButtonMessage.MESSAGE_ID,
        SaveDialogButtonMessage.class,
        SaveDialogButtonMessage::encode,
        SaveDialogButtonMessage::decode,
        SaveDialogButtonMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        SaveDialogMessage.MESSAGE_ID,
        SaveDialogMessage.class,
        SaveDialogMessage::encode,
        SaveDialogMessage::decode,
        SaveDialogMessage::handle);

    networkHandler.registerServerNetworkMessageHandler(
        SaveDialogSetMessage.MESSAGE_ID,
        SaveDialogSetMessage.class,
        SaveDialogSetMessage::encode,
        SaveDialogSetMessage::decode,
        SaveDialogSetMessage::handle);
  }
}
