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

import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.Set;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

public class MessageTriggerActionEvent extends NetworkMessage {

  protected final ActionEventType actionEventType;

  public MessageTriggerActionEvent(UUID uuid, ActionEventType actionEventType) {
    super(uuid);
    this.actionEventType = actionEventType;
  }

  public static MessageTriggerActionEvent decode(final FriendlyByteBuf buffer) {
    return new MessageTriggerActionEvent(buffer.readUUID(), buffer.readEnum(ActionEventType.class));
  }

  public static void encode(final MessageTriggerActionEvent message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getActionType());
  }

  public static void handle(
      MessageTriggerActionEvent message, Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageTriggerActionEvent message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || uuid == null) {
      log.error("Unable to trigger action event with message {} from {}", message, context);
      return;
    }

    // Validate action type.
    ActionEventType actionEventType = message.getActionType();
    if (actionEventType == null || actionEventType == ActionEventType.NONE) {
      log.error(
          "Invalid action event type {} for {} from {}", actionEventType, message, serverPlayer);
      return;
    }

    // Validate entity.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPCEntity == null) {
      log.error("Unable to get valid entity with UUID {} for {}", uuid, serverPlayer);
      return;
    }

    // Validate action.
    Set<ActionData> actionDataList =
        easyNPCEntity.getActionEventSet().getActionEvents(actionEventType);
    if (actionDataList == null || actionDataList.isEmpty()) {
      log.error(
          "Unknown trigger action event {} request for UUID {} from {}",
          actionEventType,
          uuid,
          serverPlayer);
      return;
    }

    // Perform action.
    if (actionDataList.size() > 1) {
      log.debug(
          "Trigger multiple actions events for {} from {} with {} actions ...",
          easyNPCEntity,
          serverPlayer,
          actionDataList.size());
    }
    for (ActionData actionData : actionDataList) {
      if (actionData != null) {
        log.debug(
            "Trigger action event {} for {} from {} with permission level {} ...",
            actionData,
            easyNPCEntity,
            serverPlayer,
            actionData.getPermissionLevel());
        easyNPCEntity.executeAction(actionData, serverPlayer);
      }
    }
  }

  public ActionEventType getActionType() {
    return this.actionEventType;
  }
}
