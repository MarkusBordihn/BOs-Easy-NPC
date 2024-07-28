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
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record ExecuteActionEventMessage(UUID uuid, ActionEventType actionEventType)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "trigger_action_event");
  public static final CustomPacketPayload.Type<ExecuteActionEventMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ExecuteActionEventMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), ExecuteActionEventMessage::create);

  public static ExecuteActionEventMessage create(final FriendlyByteBuf buffer) {
    return new ExecuteActionEventMessage(buffer.readUUID(), buffer.readEnum(ActionEventType.class));
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeEnum(this.actionEventType);
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
    EasyNPC<?> easyNPC = getEasyNPC(this.uuid, serverPlayer);
    if (easyNPC == null) {
      return;
    }

    // Validate action type.
    if (this.actionEventType == null || this.actionEventType == ActionEventType.NONE) {
      log.error(
          "Invalid action event type {} for {} from {}",
          this.actionEventType,
          easyNPC,
          serverPlayer);
      return;
    }

    // Validate action event data.
    ActionEventData<?> actionEventData = easyNPC.getEasyNPCActionEventData();
    if (actionEventData == null) {
      log.error("Unable to get valid action event data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate action.
    ActionDataSet actionDataSet =
        actionEventData.getActionEventSet().getActionEvents(this.actionEventType);
    if (actionDataSet == null || actionDataSet.isEmpty()) {
      log.error(
          "Empty trigger action event {} request for {} from {}",
          this.actionEventType,
          easyNPC,
          serverPlayer);
      return;
    }

    // Validate action handler.
    ActionHandler<?> actionHandler = easyNPC.getEasyNPCActionHandler();
    if (actionHandler == null) {
      log.error("Unable to get valid action handler for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Perform action.
    actionHandler.executeActions(actionDataSet, serverPlayer);
  }
}
