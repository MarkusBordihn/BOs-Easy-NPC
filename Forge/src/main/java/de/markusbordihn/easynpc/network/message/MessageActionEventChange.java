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

package de.markusbordihn.easynpc.network.message;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.network.CustomPayloadEvent;

public class MessageActionEventChange extends NetworkMessage {

  protected final ActionDataEntry actionDataEntry;
  protected final ActionEventType actionEventType;

  public MessageActionEventChange(UUID uuid, ActionEventType actionEventType, String action) {
    this(uuid, actionEventType, new ActionDataEntry(ActionType.COMMAND, action));
  }

  public MessageActionEventChange(
      UUID uuid, ActionEventType actionEventType, ActionDataEntry actionDataEntry) {
    super(uuid);
    this.actionDataEntry = actionDataEntry;
    this.actionEventType = actionEventType;
  }

  public static MessageActionEventChange decode(final FriendlyByteBuf buffer) {
    return new MessageActionEventChange(
        buffer.readUUID(),
        buffer.readEnum(ActionEventType.class),
        new ActionDataEntry(buffer.readNbt()));
  }

  public static void encode(final MessageActionEventChange message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getActionEventType());
    buffer.writeNbt(message.actionDataEntry.createTag());
  }

  public static void handle(MessageActionEventChange message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageActionEventChange message, CustomPayloadEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate action event type.
    ActionEventType actionEventType = message.getActionEventType();
    if (actionEventType == null || actionEventType == ActionEventType.NONE) {
      log.error(
          "Invalid action event type {} for {} from {}", actionEventType, message, serverPlayer);
      return;
    }

    // Validate action data.
    ActionDataEntry actionDataEntry = message.getActionData();
    if (actionDataEntry == null || !actionDataEntry.isValid()) {
      log.error("Invalid action data {} for {} from {}", actionDataEntry, message, serverPlayer);
      return;
    }

    // Get Permission level for corresponding action.
    int permissionLevel = 0;
    MinecraftServer minecraftServer = serverPlayer.getServer();
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    ActionEventData<?> actionEventData = easyNPC.getEasyNPCActionEventData();
    if (minecraftServer != null) {
      permissionLevel = minecraftServer.getProfilePermissions(serverPlayer.getGameProfile());
      log.debug(
          "Set action owner permission level {} for {} from {}",
          permissionLevel,
          message,
          serverPlayer);
      actionEventData.setActionPermissionLevel(permissionLevel);
    } else {
      log.warn("Unable to verify permission level from {} for {}", message, serverPlayer);
    }

    // Perform action.
    log.debug(
        "Set action event {} with {} for {} from {} with owner permission level {}.",
        actionEventType,
        actionDataEntry,
        easyNPC,
        serverPlayer,
        permissionLevel);
    actionEventData.getActionEventSet().setActionEvent(actionEventType, actionDataEntry);
  }

  public ActionEventType getActionEventType() {
    return this.actionEventType;
  }

  public ActionDataEntry getActionData() {
    return this.actionDataEntry;
  }
}
