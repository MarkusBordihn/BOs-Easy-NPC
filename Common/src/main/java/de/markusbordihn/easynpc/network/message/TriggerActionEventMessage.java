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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.network.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.Set;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class TriggerActionEventMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "trigger_action_event");

  protected final ActionEventType actionEventType;

  public TriggerActionEventMessage(UUID uuid, ActionEventType actionEventType) {
    super(uuid);
    this.actionEventType = actionEventType;
  }

  public static TriggerActionEventMessage decode(final FriendlyByteBuf buffer) {
    return new TriggerActionEventMessage(buffer.readUUID(), buffer.readEnum(ActionEventType.class));
  }

  public static FriendlyByteBuf encode(
      final TriggerActionEventMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getActionType());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(TriggerActionEventMessage message, ServerPlayer serverPlayer) {
    UUID uuid = message.getUUID();
    if (serverPlayer == null || uuid == null) {
      log.error("Unable to trigger action event with message {} from {}", message, serverPlayer);
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
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      log.error("Unable to get valid entity with UUID {} for {}", uuid, serverPlayer);
      return;
    }

    // Validate action event data.
    ActionEventData<?> actionEventData = easyNPC.getEasyNPCActionEventData();
    if (actionEventData == null) {
      log.error("Unable to get valid action event data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate action handler.
    ActionHandler<?> actionHandler = easyNPC.getEasyNPCActionHandler();
    if (actionHandler == null) {
      log.error("Unable to get valid action handler for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate action.
    Set<ActionDataEntry> actionDataEntryList =
        actionEventData.getActionEventSet().getActionEvents(actionEventType);
    if (actionDataEntryList == null || actionDataEntryList.isEmpty()) {
      log.error(
          "Unknown trigger action event {} request for UUID {} from {}",
          actionEventType,
          uuid,
          serverPlayer);
      return;
    }

    // Perform action.
    if (actionDataEntryList.size() > 1) {
      log.debug(
          "Trigger multiple actions events for {} from {} with {} actions ...",
          easyNPC,
          serverPlayer,
          actionDataEntryList.size());
    }
    for (ActionDataEntry actionDataEntry : actionDataEntryList) {
      if (actionDataEntry != null) {
        log.debug(
            "Trigger action event {} for {} from {} with permission level {} ...",
            actionDataEntry,
            easyNPC,
            serverPlayer,
            actionDataEntry.getPermissionLevel());
        actionHandler.executeAction(actionDataEntry, serverPlayer);
      }
    }
  }

  public FriendlyByteBuf encode() {
    return encode(this, new FriendlyByteBuf(Unpooled.buffer()));
  }

  public ActionEventType getActionType() {
    return this.actionEventType;
  }
}
