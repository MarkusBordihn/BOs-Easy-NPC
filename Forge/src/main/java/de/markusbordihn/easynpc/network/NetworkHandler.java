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
import de.markusbordihn.easynpc.network.message.MessageTradingTypeChange;
import de.markusbordihn.easynpc.network.message.MessageTriggerActionEvent;
import de.markusbordihn.easynpc.network.message.MessageVariantChange;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.network.ChannelBuilder;
import net.minecraftforge.network.NetworkDirection;
import net.minecraftforge.network.PacketDistributor;
import net.minecraftforge.network.SimpleChannel;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber
public class NetworkHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final int PROTOCOL_VERSION = 16;
  private static final SimpleChannel SIMPLE_CHANNEL =
      ChannelBuilder.named(new ResourceLocation(Constants.MOD_ID, "network"))
          .networkProtocolVersion(PROTOCOL_VERSION)
          .simpleChannel();

  private static int id = 0;

  public static void registerNetworkHandler(final FMLCommonSetupEvent event) {

    log.info(
        "{} Network Handler for {} with version {} ...",
        Constants.LOG_REGISTER_PREFIX,
        SIMPLE_CHANNEL,
        PROTOCOL_VERSION);

    event.enqueueWork(
        () -> {

          // Action Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageActionEventChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageActionEventChange::encode)
              .decoder(MessageActionEventChange::decode)
              .consumerNetworkThread(MessageActionEventChange::handle)
              .add();

          // Advanced Trading: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageAdvancedTrading.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageAdvancedTrading::encode)
              .decoder(MessageAdvancedTrading::decode)
              .consumerNetworkThread(MessageAdvancedTrading::handle)
              .add();

          // Basic Trading: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageBasicTrading.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageBasicTrading::encode)
              .decoder(MessageBasicTrading::decode)
              .consumerNetworkThread(MessageBasicTrading::handle)
              .add();

          // Dialog Button Action: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(
                  MessageDialogButtonAction.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageDialogButtonAction::encode)
              .decoder(MessageDialogButtonAction::decode)
              .consumerNetworkThread(MessageDialogButtonAction::handle)
              .add();

          // Entity Attribute Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(
                  MessageEntityAttributeChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageEntityAttributeChange::encode)
              .decoder(MessageEntityAttributeChange::decode)
              .consumerNetworkThread(MessageEntityAttributeChange::handle)
              .add();

          // Entity Base Attribute Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(
                  MessageEntityBaseAttributeChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageEntityBaseAttributeChange::encode)
              .decoder(MessageEntityBaseAttributeChange::decode)
              .consumerNetworkThread(MessageEntityBaseAttributeChange::handle)
              .add();

          // Model Local Rotation Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(
                  MessageModelLockRotationChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageModelLockRotationChange::encode)
              .decoder(MessageModelLockRotationChange::decode)
              .consumerNetworkThread(MessageModelLockRotationChange::handle)
              .add();

          // Open Dialog Text Editor Screen: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(
                  MessageOpenDialogTextEditor.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageOpenDialogTextEditor::encode)
              .decoder(MessageOpenDialogTextEditor::decode)
              .consumerNetworkThread(MessageOpenDialogTextEditor::handle)
              .add();

          // Model Pose Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageModelPoseChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageModelPoseChange::encode)
              .decoder(MessageModelPoseChange::decode)
              .consumerNetworkThread(MessageModelPoseChange::handle)
              .add();

          // Model Position Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(
                  MessageModelPositionChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageModelPositionChange::encode)
              .decoder(MessageModelPositionChange::decode)
              .consumerNetworkThread(MessageModelPositionChange::handle)
              .add();

          // Model Visibility Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(
                  MessageModelVisibilityChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageModelVisibilityChange::encode)
              .decoder(MessageModelVisibilityChange::decode)
              .consumerNetworkThread(MessageModelVisibilityChange::handle)
              .add();

          // Name Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageNameChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageNameChange::encode)
              .decoder(MessageNameChange::decode)
              .consumerNetworkThread(MessageNameChange::handle)
              .add();

          // Objective Add: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageObjectiveAdd.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageObjectiveAdd::encode)
              .decoder(MessageObjectiveAdd::decode)
              .consumerNetworkThread(MessageObjectiveAdd::handle)
              .add();

          // Objective Add: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageObjectiveRemove.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageObjectiveRemove::encode)
              .decoder(MessageObjectiveRemove::decode)
              .consumerNetworkThread(MessageObjectiveRemove::handle)
              .add();

          // Open Configuration Screen: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageOpenConfiguration.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageOpenConfiguration::encode)
              .decoder(MessageOpenConfiguration::decode)
              .consumerNetworkThread(MessageOpenConfiguration::handle)
              .add();

          // Open Dialog Screen: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageOpenDialog.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageOpenDialog::encode)
              .decoder(MessageOpenDialog::decode)
              .consumerNetworkThread(MessageOpenDialog::handle)
              .add();

          // Open Dialog Editor Screen: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageOpenDialogEditor.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageOpenDialogEditor::encode)
              .decoder(MessageOpenDialogEditor::decode)
              .consumerNetworkThread(MessageOpenDialogEditor::handle)
              .add();

          // Open Dialog Button Editor Screen: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(
                  MessageOpenDialogButtonEditor.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageOpenDialogButtonEditor::encode)
              .decoder(MessageOpenDialogButtonEditor::decode)
              .consumerNetworkThread(MessageOpenDialogButtonEditor::handle)
              .add();

          // Pose Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessagePoseChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessagePoseChange::encode)
              .decoder(MessagePoseChange::decode)
              .consumerNetworkThread(MessagePoseChange::handle)
              .add();

          // Position Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessagePositionChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessagePositionChange::encode)
              .decoder(MessagePositionChange::decode)
              .consumerNetworkThread(MessagePositionChange::handle)
              .add();

          // Export Preset: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessagePresetExport.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessagePresetExport::encode)
              .decoder(MessagePresetExport::decode)
              .consumerNetworkThread(MessagePresetExport::handle)
              .add();

          // Export Preset: Server -> Client
          SIMPLE_CHANNEL
              .messageBuilder(
                  MessagePresetExportClient.class, id++, NetworkDirection.PLAY_TO_CLIENT)
              .encoder(MessagePresetExportClient::encode)
              .decoder(MessagePresetExportClient::decode)
              .consumerNetworkThread(MessagePresetExportClient::handle)
              .add();

          // Export Preset World: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessagePresetExportWorld.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessagePresetExportWorld::encode)
              .decoder(MessagePresetExportWorld::decode)
              .consumerNetworkThread(MessagePresetExportWorld::handle)
              .add();

          // Import Preset: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessagePresetImport.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessagePresetImport::encode)
              .decoder(MessagePresetImport::decode)
              .consumerNetworkThread(MessagePresetImport::handle)
              .add();

          // Import Preset World: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessagePresetImportWorld.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessagePresetImportWorld::encode)
              .decoder(MessagePresetImportWorld::decode)
              .consumerNetworkThread(MessagePresetImportWorld::handle)
              .add();

          // Profession Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageProfessionChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageProfessionChange::encode)
              .decoder(MessageProfessionChange::decode)
              .consumerNetworkThread(MessageProfessionChange::handle)
              .add();

          // Remove Dialog: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageRemoveDialog.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageRemoveDialog::encode)
              .decoder(MessageRemoveDialog::decode)
              .consumerNetworkThread(MessageRemoveDialog::handle)
              .add();

          // Remove Dialog Button: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(
                  MessageRemoveDialogButton.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageRemoveDialogButton::encode)
              .decoder(MessageRemoveDialogButton::decode)
              .consumerNetworkThread(MessageRemoveDialogButton::handle)
              .add();

          // Remove NPC: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageRemoveNPC.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageRemoveNPC::encode)
              .decoder(MessageRemoveNPC::decode)
              .consumerNetworkThread(MessageRemoveNPC::handle)
              .add();

          // Respawn NPC: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageRespawnNPC.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageRespawnNPC::encode)
              .decoder(MessageRespawnNPC::decode)
              .consumerNetworkThread(MessageRespawnNPC::handle)
              .add();

          // Rotation Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(
                  MessageModelRotationChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageModelRotationChange::encode)
              .decoder(MessageModelRotationChange::decode)
              .consumerNetworkThread(MessageModelRotationChange::handle)
              .add();

          // Save Dialog: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageSaveDialog.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageSaveDialog::encode)
              .decoder(MessageSaveDialog::decode)
              .consumerNetworkThread(MessageSaveDialog::handle)
              .add();

          // Save Dialog: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageSaveDialogSet.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageSaveDialogSet::encode)
              .decoder(MessageSaveDialogSet::decode)
              .consumerNetworkThread(MessageSaveDialogSet::handle)
              .add();

          // Save Dialog Button: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageSaveDialogButton.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageSaveDialogButton::encode)
              .decoder(MessageSaveDialogButton::decode)
              .consumerNetworkThread(MessageSaveDialogButton::handle)
              .add();

          // Scale Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageScaleChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageScaleChange::encode)
              .decoder(MessageScaleChange::decode)
              .consumerNetworkThread(MessageScaleChange::handle)
              .add();

          // Skin Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageSkinChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageSkinChange::encode)
              .decoder(MessageSkinChange::decode)
              .consumerNetworkThread(MessageSkinChange::handle)
              .add();

          // Trading Type: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageTradingTypeChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageTradingTypeChange::encode)
              .decoder(MessageTradingTypeChange::decode)
              .consumerNetworkThread(MessageTradingTypeChange::handle)
              .add();

          // Action Trigger: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(
                  MessageTriggerActionEvent.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageTriggerActionEvent::encode)
              .decoder(MessageTriggerActionEvent::decode)
              .consumerNetworkThread(MessageTriggerActionEvent::handle)
              .add();

          // Variant Change: Client -> Server
          SIMPLE_CHANNEL
              .messageBuilder(MessageVariantChange.class, id++, NetworkDirection.PLAY_TO_SERVER)
              .encoder(MessageVariantChange::encode)
              .decoder(MessageVariantChange::decode)
              .consumerNetworkThread(MessageVariantChange::handle)
              .add();
        });
  }

  public static <M> void sendToServer(M message) {
    try {
      SIMPLE_CHANNEL.send(message, PacketDistributor.SERVER.noArg());
    } catch (Exception e) {
      log.error("Failed to send {} to server, got error: {}", message, e.getMessage());
    }
  }

  public static <M> void sendToPlayer(M message, ServerPlayer serverPlayer) {
    try {
      SIMPLE_CHANNEL.send(message, PacketDistributor.PLAYER.with(serverPlayer));
    } catch (Exception e) {
      log.error(
          "Failed to send {} to player {}, got error: {}",
          message,
          serverPlayer.getName().getString(),
          e.getMessage());
    }
  }
}
