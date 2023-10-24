/**
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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.network.NetworkRegistry;
import net.minecraftforge.network.PacketDistributor;
import net.minecraftforge.network.simple.SimpleChannel;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.network.message.MessageActionChange;
import de.markusbordihn.easynpc.network.message.MessageAdvancedTrading;
import de.markusbordihn.easynpc.network.message.MessageBasicTrading;
import de.markusbordihn.easynpc.network.message.MessageDialogTypeChange;
import de.markusbordihn.easynpc.network.message.MessageEntityAttributeChange;
import de.markusbordihn.easynpc.network.message.MessageModelLockRotationChange;
import de.markusbordihn.easynpc.network.message.MessageModelPoseChange;
import de.markusbordihn.easynpc.network.message.MessageModelPositionChange;
import de.markusbordihn.easynpc.network.message.MessageModelVisibilityChange;
import de.markusbordihn.easynpc.network.message.MessageNameChange;
import de.markusbordihn.easynpc.network.message.MessageOpenConfiguration;
import de.markusbordihn.easynpc.network.message.MessagePoseChange;
import de.markusbordihn.easynpc.network.message.MessagePositionChange;
import de.markusbordihn.easynpc.network.message.MessagePresetExport;
import de.markusbordihn.easynpc.network.message.MessagePresetExportClient;
import de.markusbordihn.easynpc.network.message.MessagePresetExportWorld;
import de.markusbordihn.easynpc.network.message.MessagePresetImport;
import de.markusbordihn.easynpc.network.message.MessagePresetImportWorld;
import de.markusbordihn.easynpc.network.message.MessageProfessionChange;
import de.markusbordihn.easynpc.network.message.MessageRemoveNPC;
import de.markusbordihn.easynpc.network.message.MessageRotationChange;
import de.markusbordihn.easynpc.network.message.MessageSaveBasicDialog;
import de.markusbordihn.easynpc.network.message.MessageSaveYesNoDialog;
import de.markusbordihn.easynpc.network.message.MessageScaleChange;
import de.markusbordihn.easynpc.network.message.MessageSkinChange;
import de.markusbordihn.easynpc.network.message.MessageTradingTypeChange;
import de.markusbordihn.easynpc.network.message.MessageTriggerAction;
import de.markusbordihn.easynpc.network.message.MessageVariantChange;

@EventBusSubscriber
public class NetworkHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String PROTOCOL_VERSION = "13";
  public static final SimpleChannel INSTANCE =
      NetworkRegistry.newSimpleChannel(new ResourceLocation(Constants.MOD_ID, "network"),
          () -> PROTOCOL_VERSION, PROTOCOL_VERSION::equals, PROTOCOL_VERSION::equals);

  private static int id = 0;

  public static void registerNetworkHandler(final FMLCommonSetupEvent event) {

    log.info("{} Network Handler for {} with version {} ...", Constants.LOG_REGISTER_PREFIX,
        INSTANCE, PROTOCOL_VERSION);

    event.enqueueWork(() -> {

      // Action Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageActionChange.class, MessageActionChange::encode,
          MessageActionChange::decode, MessageActionChange::handle);

      // Advanced Trading: Client -> Server
      INSTANCE.registerMessage(id++, MessageAdvancedTrading.class, MessageAdvancedTrading::encode,
          MessageAdvancedTrading::decode, MessageAdvancedTrading::handle);

      // Basic Trading: Client -> Server
      INSTANCE.registerMessage(id++, MessageBasicTrading.class, MessageBasicTrading::encode,
          MessageBasicTrading::decode, MessageBasicTrading::handle);

      // Dialog Type: Client -> Server
      INSTANCE.registerMessage(id++, MessageDialogTypeChange.class, MessageDialogTypeChange::encode,
          MessageDialogTypeChange::decode, MessageDialogTypeChange::handle);

      // Entity Attribute Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageEntityAttributeChange.class,
          MessageEntityAttributeChange::encode, MessageEntityAttributeChange::decode,
          MessageEntityAttributeChange::handle);

      // Model Local Rotation Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageModelLockRotationChange.class,
          MessageModelLockRotationChange::encode, MessageModelLockRotationChange::decode,
          MessageModelLockRotationChange::handle);

      // Model Pose Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageModelPoseChange.class, MessageModelPoseChange::encode,
          MessageModelPoseChange::decode, MessageModelPoseChange::handle);

      // Model Position Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageModelPositionChange.class,
          MessageModelPositionChange::encode, MessageModelPositionChange::decode,
          MessageModelPositionChange::handle);

      // Model Visibility Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageModelVisibilityChange.class,
          MessageModelVisibilityChange::encode, MessageModelVisibilityChange::decode,
          MessageModelVisibilityChange::handle);

      // Name Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageNameChange.class, MessageNameChange::encode,
          MessageNameChange::decode, MessageNameChange::handle);

      // Open Configuration Screen: Client -> Server
      INSTANCE.registerMessage(id++, MessageOpenConfiguration.class,
          MessageOpenConfiguration::encode, MessageOpenConfiguration::decode,
          MessageOpenConfiguration::handle);

      // Pose Change: Client -> Server
      INSTANCE.registerMessage(id++, MessagePoseChange.class, MessagePoseChange::encode,
          MessagePoseChange::decode, MessagePoseChange::handle);

      // Position Change: Client -> Server
      INSTANCE.registerMessage(id++, MessagePositionChange.class, MessagePositionChange::encode,
          MessagePositionChange::decode, MessagePositionChange::handle);

      // Export Preset: Client -> Server
      INSTANCE.registerMessage(id++, MessagePresetExport.class, MessagePresetExport::encode,
          MessagePresetExport::decode, MessagePresetExport::handle);

      // Export Preset: Server -> Client
      INSTANCE.registerMessage(id++, MessagePresetExportClient.class,
          MessagePresetExportClient::encode, MessagePresetExportClient::decode,
          MessagePresetExportClient::handle);

      // Export Preset World: Client -> Server
      INSTANCE.registerMessage(id++, MessagePresetExportWorld.class,
          MessagePresetExportWorld::encode, MessagePresetExportWorld::decode,
          MessagePresetExportWorld::handle);

      // Import Preset: Client -> Server
      INSTANCE.registerMessage(id++, MessagePresetImport.class, MessagePresetImport::encode,
          MessagePresetImport::decode, MessagePresetImport::handle);

      // Import Preset World: Client -> Server
      INSTANCE.registerMessage(id++, MessagePresetImportWorld.class,
          MessagePresetImportWorld::encode, MessagePresetImportWorld::decode,
          MessagePresetImportWorld::handle);

      // Profession Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageProfessionChange.class, MessageProfessionChange::encode,
          MessageProfessionChange::decode, MessageProfessionChange::handle);

      // Remove NPC: Client -> Server
      INSTANCE.registerMessage(id++, MessageRemoveNPC.class, MessageRemoveNPC::encode,
          MessageRemoveNPC::decode, MessageRemoveNPC::handle);

      // Rotation Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageRotationChange.class, MessageRotationChange::encode,
          MessageRotationChange::decode, MessageRotationChange::handle);

      // Save Basic Dialog: Client -> Server
      INSTANCE.registerMessage(id++, MessageSaveBasicDialog.class, MessageSaveBasicDialog::encode,
          MessageSaveBasicDialog::decode, MessageSaveBasicDialog::handle);

      // Save Yes/No Dialog: Client -> Server
      INSTANCE.registerMessage(id++, MessageSaveYesNoDialog.class, MessageSaveYesNoDialog::encode,
          MessageSaveYesNoDialog::decode, MessageSaveYesNoDialog::handle);

      // Scale Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageScaleChange.class, MessageScaleChange::encode,
          MessageScaleChange::decode, MessageScaleChange::handle);

      // Skin Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageSkinChange.class, MessageSkinChange::encode,
          MessageSkinChange::decode, MessageSkinChange::handle);

      // Trading Type: Client -> Server
      INSTANCE.registerMessage(id++, MessageTradingTypeChange.class,
          MessageTradingTypeChange::encode, MessageTradingTypeChange::decode,
          MessageTradingTypeChange::handle);

      // Action Trigger: Client -> Server
      INSTANCE.registerMessage(id++, MessageTriggerAction.class, MessageTriggerAction::encode,
          MessageTriggerAction::decode, MessageTriggerAction::handle);

      // Variant Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageVariantChange.class, MessageVariantChange::encode,
          MessageVariantChange::decode, MessageVariantChange::handle);

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
      log.error("Failed to send {} to player {}, got error: {}", message,
          serverPlayer.getName().getString(), e.getMessage());
    }
  }

}
