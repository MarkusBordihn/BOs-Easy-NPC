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

import de.markusbordihn.easynpc.data.objective.ObjectiveData;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.network.CustomPayloadEvent;

public class MessageObjectiveRemove extends NetworkMessage {

  protected final ObjectiveData objectiveData;

  public MessageObjectiveRemove(UUID uuid, ObjectiveData objectiveData) {
    super(uuid);
    this.objectiveData = objectiveData;
  }

  public static MessageObjectiveRemove decode(final FriendlyByteBuf buffer) {
    return new MessageObjectiveRemove(buffer.readUUID(), new ObjectiveData(buffer.readNbt()));
  }

  public static void encode(final MessageObjectiveRemove message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeNbt(message.objectiveData.createTag());
  }

  public static void handle(MessageObjectiveRemove message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageObjectiveRemove message, CustomPayloadEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate objective data
    ObjectiveData objectiveData = message.getObjectiveData();
    if (objectiveData == null) {
      log.error("Unable to add objective data for {} because it is null!", uuid);
      return;
    }

    // Perform action.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPCEntity == null) {
      log.error("Unable to add objective data for {} because it is null!", uuid);
      return;
    }

    // Perform action.
    if (easyNPCEntity.removeCustomObjective(objectiveData)) {
      log.debug("Removed objective {} for {} from {}", objectiveData, easyNPCEntity, serverPlayer);
      log.debug(
          "Available goals for {}: {}",
          easyNPCEntity,
          easyNPCEntity.getEntityGoalSelector().getAvailableGoals());
      log.debug(
          "Available targets for {}: {}",
          easyNPCEntity,
          easyNPCEntity.getEntityTargetSelector().getAvailableGoals());
    }
  }

  public ObjectiveData getObjectiveData() {
    return this.objectiveData;
  }
}
