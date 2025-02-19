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
import de.markusbordihn.easynpc.debug.Logger;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import de.markusbordihn.easynpc.network.message.client.ExportClientPresetMessage;
import de.markusbordihn.easynpc.network.message.client.OpenMenuCallbackMessage;
import de.markusbordihn.easynpc.network.message.client.SyncDataMessage;
import de.markusbordihn.easynpc.network.message.server.*;
import net.minecraft.server.level.ServerPlayer;

public class NetworkHandlerManager {

  private static NetworkHandlerInterface networkHandler;
  private static NetworkHandlerManagerType networkHandlerManagerType =
      NetworkHandlerManagerType.BOTH;

  private NetworkHandlerManager() {}

  public static void registerHandler(final NetworkHandlerInterface networkHandler) {
    Logger.INSTANCE.info("{} Network Handler ...", Constants.LOG_REGISTER_PREFIX);
    NetworkHandlerManager.networkHandler = networkHandler;
  }

  public static NetworkHandlerInterface getHandler() {
    return networkHandler;
  }

  public static void registerNetworkMessages(NetworkHandlerManagerType networkHandlerType) {
    Logger.INSTANCE.info("Registering network messages for {} side ...", networkHandlerType);
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

  public static void registerClientNetworkHandler() {
    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler == null) {
      Logger.INSTANCE.error("Failed to register client network handler!");
      return;
    }

    networkHandler.registerClientNetworkMessage(
        ExportClientPresetMessage.MESSAGE_ID,
        ExportClientPresetMessage.class,
        ExportClientPresetMessage::create);
    networkHandler.registerClientNetworkMessage(
        OpenMenuCallbackMessage.MESSAGE_ID,
        OpenMenuCallbackMessage.class,
        OpenMenuCallbackMessage::create);
    networkHandler.registerClientNetworkMessage(
        SyncDataMessage.MESSAGE_ID, SyncDataMessage.class, SyncDataMessage::create);
  }

  public static void registerServerNetworkHandler() {
    NetworkHandlerInterface networkHandler = getHandler();
    if (networkHandler == null) {
      Logger.INSTANCE.error("Failed to register server network handler!");
      return;
    }

    networkHandler.registerServerNetworkMessage(
        AddOrUpdateObjectiveMessage.MESSAGE_ID,
        AddOrUpdateObjectiveMessage.class,
        AddOrUpdateObjectiveMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeActionEventMessage.MESSAGE_ID,
        ChangeActionEventMessage.class,
        ChangeActionEventMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeAdvancedTradingMessage.MESSAGE_ID,
        ChangeAdvancedTradingMessage.class,
        ChangeAdvancedTradingMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeBasicTradingMessage.MESSAGE_ID,
        ChangeBasicTradingMessage.class,
        ChangeBasicTradingMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeCombatAttributeMessage.MESSAGE_ID,
        ChangeCombatAttributeMessage.class,
        ChangeCombatAttributeMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeDisplayAttributeMessage.MESSAGE_ID,
        ChangeDisplayAttributeMessage.class,
        ChangeDisplayAttributeMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeEntityAttributeMessage.MESSAGE_ID,
        ChangeEntityAttributeMessage.class,
        ChangeEntityAttributeMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeEntityBaseAttributeMessage.MESSAGE_ID,
        ChangeEntityBaseAttributeMessage.class,
        ChangeEntityBaseAttributeMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeEnvironmentalAttributeMessage.MESSAGE_ID,
        ChangeEnvironmentalAttributeMessage.class,
        ChangeEnvironmentalAttributeMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeInteractionAttributeMessage.MESSAGE_ID,
        ChangeInteractionAttributeMessage.class,
        ChangeInteractionAttributeMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeModelEquipmentVisibilityMessage.MESSAGE_ID,
        ChangeModelEquipmentVisibilityMessage.class,
        ChangeModelEquipmentVisibilityMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeModelLockRotationMessage.MESSAGE_ID,
        ChangeModelLockRotationMessage.class,
        ChangeModelLockRotationMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeModelPoseMessage.MESSAGE_ID,
        ChangeModelPoseMessage.class,
        ChangeModelPoseMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeModelPositionMessage.MESSAGE_ID,
        ChangeModelPositionMessage.class,
        ChangeModelPositionMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeModelRotationMessage.MESSAGE_ID,
        ChangeModelRotationMessage.class,
        ChangeModelRotationMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeModelVisibilityMessage.MESSAGE_ID,
        ChangeModelVisibilityMessage.class,
        ChangeModelVisibilityMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeMovementAttributeMessage.MESSAGE_ID,
        ChangeMovementAttributeMessage.class,
        ChangeMovementAttributeMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeNameMessage.MESSAGE_ID, ChangeNameMessage.class, ChangeNameMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangePoseMessage.MESSAGE_ID, ChangePoseMessage.class, ChangePoseMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangePositionMessage.MESSAGE_ID,
        ChangePositionMessage.class,
        ChangePositionMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeProfessionMessage.MESSAGE_ID,
        ChangeProfessionMessage.class,
        ChangeProfessionMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeRendererMessage.MESSAGE_ID,
        ChangeRendererMessage.class,
        ChangeRendererMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeScaleMessage.MESSAGE_ID, ChangeScaleMessage.class, ChangeScaleMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeSkinMessage.MESSAGE_ID, ChangeSkinMessage.class, ChangeSkinMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeSpawnerSettingMessage.MESSAGE_ID,
        ChangeSpawnerSettingMessage.class,
        ChangeSpawnerSettingMessage::create);

    networkHandler.registerServerNetworkMessage(
        ChangeTradingTypeMessage.MESSAGE_ID,
        ChangeTradingTypeMessage.class,
        ChangeTradingTypeMessage::create);

    networkHandler.registerServerNetworkMessage(
        ExecuteActionEventMessage.MESSAGE_ID,
        ExecuteActionEventMessage.class,
        ExecuteActionEventMessage::create);

    networkHandler.registerServerNetworkMessage(
        ExecuteDialogButtonActionMessage.MESSAGE_ID,
        ExecuteDialogButtonActionMessage.class,
        ExecuteDialogButtonActionMessage::create);

    networkHandler.registerServerNetworkMessage(
        ExportPresetMessage.MESSAGE_ID, ExportPresetMessage.class, ExportPresetMessage::create);

    networkHandler.registerServerNetworkMessage(
        ExportWorldPresetMessage.MESSAGE_ID,
        ExportWorldPresetMessage.class,
        ExportWorldPresetMessage::create);

    networkHandler.registerServerNetworkMessage(
        ImportPresetMessage.MESSAGE_ID, ImportPresetMessage.class, ImportPresetMessage::create);

    networkHandler.registerServerNetworkMessage(
        OpenActionDataEditorMessage.MESSAGE_ID,
        OpenActionDataEditorMessage.class,
        OpenActionDataEditorMessage::create);

    networkHandler.registerServerNetworkMessage(
        OpenActionDataEntryEditorMessage.MESSAGE_ID,
        OpenActionDataEntryEditorMessage.class,
        OpenActionDataEntryEditorMessage::create);

    networkHandler.registerServerNetworkMessage(
        OpenConfigurationMessage.MESSAGE_ID,
        OpenConfigurationMessage.class,
        OpenConfigurationMessage::create);

    networkHandler.registerServerNetworkMessage(
        OpenDialogButtonEditorMessage.MESSAGE_ID,
        OpenDialogButtonEditorMessage.class,
        OpenDialogButtonEditorMessage::create);

    networkHandler.registerServerNetworkMessage(
        OpenDialogEditorMessage.MESSAGE_ID,
        OpenDialogEditorMessage.class,
        OpenDialogEditorMessage::create);

    networkHandler.registerServerNetworkMessage(
        OpenMenuMessage.MESSAGE_ID, OpenMenuMessage.class, OpenMenuMessage::create);

    networkHandler.registerServerNetworkMessage(
        OpenDialogTextEditorMessage.MESSAGE_ID,
        OpenDialogTextEditorMessage.class,
        OpenDialogTextEditorMessage::create);

    networkHandler.registerServerNetworkMessage(
        RemoveDialogButtonMessage.MESSAGE_ID,
        RemoveDialogButtonMessage.class,
        RemoveDialogButtonMessage::create);

    networkHandler.registerServerNetworkMessage(
        RemoveDialogMessage.MESSAGE_ID, RemoveDialogMessage.class, RemoveDialogMessage::create);

    networkHandler.registerServerNetworkMessage(
        RemoveNPCMessage.MESSAGE_ID, RemoveNPCMessage.class, RemoveNPCMessage::create);

    networkHandler.registerServerNetworkMessage(
        RemoveObjectiveMessage.MESSAGE_ID,
        RemoveObjectiveMessage.class,
        RemoveObjectiveMessage::create);

    networkHandler.registerServerNetworkMessage(
        RequestDataSyncMessage.MESSAGE_ID,
        RequestDataSyncMessage.class,
        RequestDataSyncMessage::create);

    networkHandler.registerServerNetworkMessage(
        RespawnNPCMessage.MESSAGE_ID, RespawnNPCMessage.class, RespawnNPCMessage::create);

    networkHandler.registerServerNetworkMessage(
        SaveDialogButtonMessage.MESSAGE_ID,
        SaveDialogButtonMessage.class,
        SaveDialogButtonMessage::create);

    networkHandler.registerServerNetworkMessage(
        SaveDialogMessage.MESSAGE_ID, SaveDialogMessage.class, SaveDialogMessage::create);

    networkHandler.registerServerNetworkMessage(
        SaveDialogSetMessage.MESSAGE_ID, SaveDialogSetMessage.class, SaveDialogSetMessage::create);
  }
}
