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

package de.markusbordihn.easynpc.network.message;

import java.util.UUID;
import java.util.function.Supplier;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;

import net.minecraftforge.network.NetworkEvent;

import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EasyNPCEntityMenu;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.NetworkMessage;

public class MessageOpenConfiguration extends NetworkMessage {

  protected final ConfigurationType configurationType;
  protected final int pageIndex;

  public MessageOpenConfiguration(UUID uuid, ConfigurationType configurationType, int pageIndex) {
    super(uuid);
    this.configurationType = configurationType;
    this.pageIndex = pageIndex;
  }

  public ConfigurationType getConfigurationType() {
    return this.configurationType;
  }

  public int getPageIndex() {
    return this.pageIndex;
  }

  public static MessageOpenConfiguration decode(final FriendlyByteBuf buffer) {
    return new MessageOpenConfiguration(buffer.readUUID(),
        buffer.readEnum(ConfigurationType.class), buffer.readInt());
  }

  public static void encode(final MessageOpenConfiguration message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getConfigurationType());
    buffer.writeInt(message.pageIndex);
  }

  public static void handle(MessageOpenConfiguration message,
      Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageOpenConfiguration message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate dialog name.
    ConfigurationType configurationType = message.getConfigurationType();
    if (configurationType == null) {
      log.error("Invalid configuration type {} for {} from {}", configurationType, message,
          serverPlayer);
      return;
    }

    // Validate page index.
    int pageIndex = message.getPageIndex();
    if (pageIndex < 0) {
      log.error("Invalid page index {} for {} from {}", pageIndex, message, serverPlayer);
      return;
    }

    // Perform action.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    switch (configurationType) {
      case ADVANCED_POSE:
        EasyNPCEntityMenu.openAdvancedPoseConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case BASIC_ACTION:
        EasyNPCEntityMenu.openBasicActionConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case DIALOG_ACTION:
        EasyNPCEntityMenu.openDialogActionConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case DISTANCE_ACTION:
        EasyNPCEntityMenu.openDistanceActionConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case BASIC_DIALOG:
        EasyNPCEntityMenu.openBasicDialogConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case CUSTOM_SKIN:
        EasyNPCEntityMenu.openCustomSkinConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case DEFAULT_SKIN:
        EasyNPCEntityMenu.openDefaultSkinConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case EQUIPMENT:
        EasyNPCEntityMenu.openEquipmentConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case MAIN:
        EasyNPCEntityMenu.openMainConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case DEFAULT_POSE:
        EasyNPCEntityMenu.openDefaultPoseConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case CUSTOM_POSE:
        EasyNPCEntityMenu.openCustomPoseConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case DEFAULT_POSITION:
        EasyNPCEntityMenu.openDefaultPositionConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case DEFAULT_ROTATION:
        EasyNPCEntityMenu.openDefaultRotationConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case NONE_DIALOG:
        EasyNPCEntityMenu.openNoneDialogConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case PLAYER_SKIN:
        EasyNPCEntityMenu.openPlayerSkinConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case SCALING:
        EasyNPCEntityMenu.openScalingConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case YES_NO_DIALOG:
        EasyNPCEntityMenu.openYesNoDialogConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case CUSTOM_PRESET_EXPORT:
        EasyNPCEntityMenu.openCustomPresetExportConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case WORLD_PRESET_EXPORT:
        EasyNPCEntityMenu.openWorldPresetExportConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case DEFAULT_PRESET_IMPORT:
        EasyNPCEntityMenu.openDefaultPresetImportConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case WORLD_PRESET_IMPORT:
        EasyNPCEntityMenu.openServerPresetImportConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case CUSTOM_PRESET_IMPORT:
        EasyNPCEntityMenu.openCustomPresetImportConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case BASIC_TRADING:
        EasyNPCEntityMenu.openBasicTradingConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case NONE_TRADING:
        EasyNPCEntityMenu.openNoneTradingConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      case ADVANCED_TRADING:
        EasyNPCEntityMenu.openAdvancedTradingConfigurationMenu(serverPlayer, easyNPCEntity, pageIndex);
        break;
      case CUSTOM_TRADING:
        EasyNPCEntityMenu.openCustomTradingConfigurationMenu(serverPlayer, easyNPCEntity);
        break;
      default:
        log.debug("Unknown dialog {} for {} from {}", configurationType, easyNPCEntity,
            serverPlayer);
    }
  }

}
