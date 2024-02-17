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
import de.markusbordihn.easynpc.network.message.ChangeSpawnerSettingMessage;
import de.markusbordihn.easynpc.network.message.MessageActionEventChange;
import de.markusbordihn.easynpc.network.message.MessageAdvancedTrading;
import de.markusbordihn.easynpc.network.message.MessageBasicTrading;
import de.markusbordihn.easynpc.network.message.MessageDialogButtonAction;
import de.markusbordihn.easynpc.network.message.MessageEntityAttributeChange;
import de.markusbordihn.easynpc.network.message.MessageEntityBaseAttributeChange;
import de.markusbordihn.easynpc.network.message.MessageModelLockRotationChange;
import de.markusbordihn.easynpc.network.message.MessageModelPoseChange;
import de.markusbordihn.easynpc.network.message.MessageModelPositionChange;
import de.markusbordihn.easynpc.network.message.MessageModelRotationChange;
import de.markusbordihn.easynpc.network.message.MessageModelVisibilityChange;
import de.markusbordihn.easynpc.network.message.MessageNameChange;
import de.markusbordihn.easynpc.network.message.MessageObjectiveAdd;
import de.markusbordihn.easynpc.network.message.MessageObjectiveRemove;
import de.markusbordihn.easynpc.network.message.MessageOpenConfiguration;
import de.markusbordihn.easynpc.network.message.MessageOpenDialog;
import de.markusbordihn.easynpc.network.message.MessageOpenDialogButtonEditor;
import de.markusbordihn.easynpc.network.message.MessageOpenDialogEditor;
import de.markusbordihn.easynpc.network.message.MessageOpenDialogTextEditor;
import de.markusbordihn.easynpc.network.message.MessagePoseChange;
import de.markusbordihn.easynpc.network.message.MessagePositionChange;
import de.markusbordihn.easynpc.network.message.MessagePresetExport;
import de.markusbordihn.easynpc.network.message.MessagePresetExportClient;
import de.markusbordihn.easynpc.network.message.MessagePresetExportWorld;
import de.markusbordihn.easynpc.network.message.MessagePresetImport;
import de.markusbordihn.easynpc.network.message.MessagePresetImportWorld;
import de.markusbordihn.easynpc.network.message.MessageProfessionChange;
import de.markusbordihn.easynpc.network.message.MessageRemoveDialog;
import de.markusbordihn.easynpc.network.message.MessageRemoveDialogButton;
import de.markusbordihn.easynpc.network.message.MessageRemoveNPC;
import de.markusbordihn.easynpc.network.message.MessageRespawnNPC;
import de.markusbordihn.easynpc.network.message.MessageSaveDialog;
import de.markusbordihn.easynpc.network.message.MessageSaveDialogButton;
import de.markusbordihn.easynpc.network.message.MessageSaveDialogSet;
import de.markusbordihn.easynpc.network.message.MessageScaleChange;
import de.markusbordihn.easynpc.network.message.MessageSkinChange;
import de.markusbordihn.easynpc.network.message.MessageSkinTypeChange;
import de.markusbordihn.easynpc.network.message.MessageTradingTypeChange;
import de.markusbordihn.easynpc.network.message.MessageTriggerActionEvent;
import de.markusbordihn.easynpc.network.message.MessageVariantChange;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.network.NetworkEvent;
import net.minecraftforge.network.NetworkRegistry;
import net.minecraftforge.network.PacketDistributor;
import net.minecraftforge.network.simple.SimpleChannel;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber
public class NetworkHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String PROTOCOL_VERSION = "17";
  public static final SimpleChannel INSTANCE =
      NetworkRegistry.newSimpleChannel(
          new ResourceLocation(Constants.MOD_ID, "network"),
          () -> PROTOCOL_VERSION,
          PROTOCOL_VERSION::equals,
          PROTOCOL_VERSION::equals);

  private static int id = 0;

  public static void registerNetworkHandler(final FMLCommonSetupEvent event) {

    log.info(
        "{} Network Handler for {} with version {} ...",
        Constants.LOG_REGISTER_PREFIX,
        INSTANCE,
        PROTOCOL_VERSION);

    event.enqueueWork(
        () -> {
          INSTANCE.registerMessage(
              id++,
              ChangeSpawnerSettingMessage.class,
              ChangeSpawnerSettingMessage::encode,
              ChangeSpawnerSettingMessage::decode,
              (message, contextSupplier) -> {
                NetworkEvent.Context context = contextSupplier.get();
                context.enqueueWork(
                    () -> ChangeSpawnerSettingMessage.handle(message, context.getSender()));
                context.setPacketHandled(true);
              });

          // Action Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageActionEventChange.class,
              MessageActionEventChange::encode,
              MessageActionEventChange::decode,
              MessageActionEventChange::handle);

          // Advanced Trading: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageAdvancedTrading.class,
              MessageAdvancedTrading::encode,
              MessageAdvancedTrading::decode,
              MessageAdvancedTrading::handle);

          // Basic Trading: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageBasicTrading.class,
              MessageBasicTrading::encode,
              MessageBasicTrading::decode,
              MessageBasicTrading::handle);

          // Dialog Button Action: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageDialogButtonAction.class,
              MessageDialogButtonAction::encode,
              MessageDialogButtonAction::decode,
              MessageDialogButtonAction::handle);

          // Entity Attribute Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageEntityAttributeChange.class,
              MessageEntityAttributeChange::encode,
              MessageEntityAttributeChange::decode,
              MessageEntityAttributeChange::handle);

          // Entity Base Attribute Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageEntityBaseAttributeChange.class,
              MessageEntityBaseAttributeChange::encode,
              MessageEntityBaseAttributeChange::decode,
              MessageEntityBaseAttributeChange::handle);

          // Model Local Rotation Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageModelLockRotationChange.class,
              MessageModelLockRotationChange::encode,
              MessageModelLockRotationChange::decode,
              MessageModelLockRotationChange::handle);

          // Model Pose Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageModelPoseChange.class,
              MessageModelPoseChange::encode,
              MessageModelPoseChange::decode,
              MessageModelPoseChange::handle);

          // Model Position Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageModelPositionChange.class,
              MessageModelPositionChange::encode,
              MessageModelPositionChange::decode,
              MessageModelPositionChange::handle);

          // Model Visibility Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageModelVisibilityChange.class,
              MessageModelVisibilityChange::encode,
              MessageModelVisibilityChange::decode,
              MessageModelVisibilityChange::handle);

          // Name Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageNameChange.class,
              MessageNameChange::encode,
              MessageNameChange::decode,
              MessageNameChange::handle);

          // Objective Add: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageObjectiveAdd.class,
              MessageObjectiveAdd::encode,
              MessageObjectiveAdd::decode,
              MessageObjectiveAdd::handle);

          // Objective Add: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageObjectiveRemove.class,
              MessageObjectiveRemove::encode,
              MessageObjectiveRemove::decode,
              MessageObjectiveRemove::handle);

          // Open Configuration Screen: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageOpenConfiguration.class,
              MessageOpenConfiguration::encode,
              MessageOpenConfiguration::decode,
              MessageOpenConfiguration::handle);

          // Open Dialog Screen: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageOpenDialog.class,
              MessageOpenDialog::encode,
              MessageOpenDialog::decode,
              MessageOpenDialog::handle);

          // Open Dialog Editor Screen: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageOpenDialogEditor.class,
              MessageOpenDialogEditor::encode,
              MessageOpenDialogEditor::decode,
              MessageOpenDialogEditor::handle);

          // Open Dialog Button Editor Screen: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageOpenDialogButtonEditor.class,
              MessageOpenDialogButtonEditor::encode,
              MessageOpenDialogButtonEditor::decode,
              MessageOpenDialogButtonEditor::handle);

          // Open Dialog Text Editor Screen: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageOpenDialogTextEditor.class,
              MessageOpenDialogTextEditor::encode,
              MessageOpenDialogTextEditor::decode,
              MessageOpenDialogTextEditor::handle);

          // Pose Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessagePoseChange.class,
              MessagePoseChange::encode,
              MessagePoseChange::decode,
              MessagePoseChange::handle);

          // Position Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessagePositionChange.class,
              MessagePositionChange::encode,
              MessagePositionChange::decode,
              MessagePositionChange::handle);

          // Export Preset: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessagePresetExport.class,
              MessagePresetExport::encode,
              MessagePresetExport::decode,
              MessagePresetExport::handle);

          // Export Preset: Server -> Client
          INSTANCE.registerMessage(
              id++,
              MessagePresetExportClient.class,
              MessagePresetExportClient::encode,
              MessagePresetExportClient::decode,
              MessagePresetExportClient::handle);

          // Export Preset World: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessagePresetExportWorld.class,
              MessagePresetExportWorld::encode,
              MessagePresetExportWorld::decode,
              MessagePresetExportWorld::handle);

          // Import Preset: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessagePresetImport.class,
              MessagePresetImport::encode,
              MessagePresetImport::decode,
              MessagePresetImport::handle);

          // Import Preset World: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessagePresetImportWorld.class,
              MessagePresetImportWorld::encode,
              MessagePresetImportWorld::decode,
              MessagePresetImportWorld::handle);

          // Profession Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageProfessionChange.class,
              MessageProfessionChange::encode,
              MessageProfessionChange::decode,
              MessageProfessionChange::handle);

          // Remove Dialog: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageRemoveDialog.class,
              MessageRemoveDialog::encode,
              MessageRemoveDialog::decode,
              MessageRemoveDialog::handle);

          // Remove Dialog Button: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageRemoveDialogButton.class,
              MessageRemoveDialogButton::encode,
              MessageRemoveDialogButton::decode,
              MessageRemoveDialogButton::handle);

          // Remove NPC: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageRemoveNPC.class,
              MessageRemoveNPC::encode,
              MessageRemoveNPC::decode,
              MessageRemoveNPC::handle);

          // Respawn NPC: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageRespawnNPC.class,
              MessageRespawnNPC::encode,
              MessageRespawnNPC::decode,
              MessageRespawnNPC::handle);

          // Rotation Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageModelRotationChange.class,
              MessageModelRotationChange::encode,
              MessageModelRotationChange::decode,
              MessageModelRotationChange::handle);

          // Save Dialog: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageSaveDialog.class,
              MessageSaveDialog::encode,
              MessageSaveDialog::decode,
              MessageSaveDialog::handle);

          // Save Dialog: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageSaveDialogSet.class,
              MessageSaveDialogSet::encode,
              MessageSaveDialogSet::decode,
              MessageSaveDialogSet::handle);

          // Save Dialog Button: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageSaveDialogButton.class,
              MessageSaveDialogButton::encode,
              MessageSaveDialogButton::decode,
              MessageSaveDialogButton::handle);

          // Scale Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageScaleChange.class,
              MessageScaleChange::encode,
              MessageScaleChange::decode,
              MessageScaleChange::handle);

          // Skin Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageSkinChange.class,
              MessageSkinChange::encode,
              MessageSkinChange::decode,
              MessageSkinChange::handle);

          // Skin Type Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageSkinTypeChange.class,
              MessageSkinTypeChange::encode,
              MessageSkinTypeChange::decode,
              MessageSkinTypeChange::handle);

          // Trading Type: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageTradingTypeChange.class,
              MessageTradingTypeChange::encode,
              MessageTradingTypeChange::decode,
              MessageTradingTypeChange::handle);

          // Action Trigger: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageTriggerActionEvent.class,
              MessageTriggerActionEvent::encode,
              MessageTriggerActionEvent::decode,
              MessageTriggerActionEvent::handle);

          // Variant Change: Client -> Server
          INSTANCE.registerMessage(
              id++,
              MessageVariantChange.class,
              MessageVariantChange::encode,
              MessageVariantChange::decode,
              MessageVariantChange::handle);
        });
  }

  public static <M> void sendToServer(M message) {
    try {
      INSTANCE.sendToServer(message);
    } catch (Exception e) {
      log.error("Failed to send {} to server, got error: {}", message, e.getMessage());
    }
  }

  public static <M> void sendToPlayer(M message, ServerPlayer serverPlayer) {
    try {
      INSTANCE.send(PacketDistributor.PLAYER.with(() -> serverPlayer), message);
    } catch (Exception e) {
      log.error(
          "Failed to send {} to player {}, got error: {}",
          message,
          serverPlayer.getName().getString(),
          e.getMessage());
    }
  }
}
