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

package de.markusbordihn.easynpc.network.message.server;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;

public record SaveDialogButtonMessage(
    UUID uuid, UUID dialogId, UUID dialogButtonId, DialogButtonEntry dialogButtonEntry)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "save_dialog_button");
  public static final CustomPacketPayload.Type<SaveDialogButtonMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, SaveDialogButtonMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), SaveDialogButtonMessage::create);

  public static SaveDialogButtonMessage create(final FriendlyByteBuf buffer) {
    return new SaveDialogButtonMessage(
        buffer.readUUID(),
        buffer.readUUID(),
        buffer.readUUID(),
        new DialogButtonEntry(buffer.readNbt()));
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUUID(this.dialogId);
    buffer.writeUUID(this.dialogButtonId);
    buffer.writeNbt(this.dialogButtonEntry.createTag());
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public Type<? extends CustomPacketPayload> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public void handleServer(final ServerPlayer serverPlayer) {
    EasyNPC<?> easyNPC = getEasyNPCAndCheckAccess(this.uuid, serverPlayer);
    if (easyNPC == null) {
      return;
    }

    // Validate dialog id.
    if (this.dialogId == null) {
      log.error("Invalid dialog id for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog button data.
    if (this.dialogButtonEntry == null) {
      log.error("Invalid dialog button data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog data.
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    if (dialogData == null) {
      log.error("Invalid dialog data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate action event data.
    ActionEventData<?> actionEventData = easyNPC.getEasyNPCActionEventData();
    if (actionEventData == null) {
      log.error("Invalid action data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog for dialog button.
    if (!dialogData.hasDialog(this.dialogId)) {
      log.error(
          "Unknown dialog button editor request for dialog {} for {} from {}",
          this.dialogId,
          easyNPC,
          serverPlayer);
      return;
    }

    // Validate dialog button id.
    if (this.dialogButtonId != null
        && !dialogData.hasDialogButton(this.dialogId, this.dialogButtonId)) {
      log.error(
          "Invalid dialog button {} for {} from {}", this.dialogButtonId, easyNPC, serverPlayer);
      return;
    }

    // Re-check permission levels for dialog related actions.
    int currentPermissionLevel = actionEventData.getActionPermissionLevel();
    if (currentPermissionLevel == 0) {
      MinecraftServer minecraftServer = serverPlayer.getServer();
      if (minecraftServer != null) {
        int permissionLevel = minecraftServer.getProfilePermissions(serverPlayer.getGameProfile());
        if (permissionLevel > currentPermissionLevel) {
          log.debug(
              "Update owner permission level from {} to {} for {} from {}",
              currentPermissionLevel,
              permissionLevel,
              easyNPC,
              serverPlayer);
          actionEventData.setActionPermissionLevel(permissionLevel);
        }
      }
    }

    // Perform action.
    if (this.dialogButtonId == null) {
      log.info(
          "Add new dialog button {} for dialog {} for {} from {}",
          dialogButtonEntry,
          dialogId,
          easyNPC,
          serverPlayer);
      dialogData
          .getDialogDataSet()
          .getDialog(this.dialogId)
          .setDialogButton(this.dialogButtonEntry);
    } else {
      log.info(
          "Edit existing dialog button {} for dialog {} for {} from {}",
          dialogButtonEntry,
          dialogId,
          easyNPC,
          serverPlayer);
      dialogData
          .getDialogDataSet()
          .getDialog(this.dialogId)
          .setDialogButton(this.dialogButtonId, this.dialogButtonEntry);
    }
  }
}
