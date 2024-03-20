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

import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

public class MessageObjectiveAdd extends NetworkMessage {

  protected final ObjectiveDataEntry objectiveDataEntry;

  public MessageObjectiveAdd(UUID uuid, ObjectiveDataEntry objectiveDataEntry) {
    super(uuid);
    this.objectiveDataEntry = objectiveDataEntry;
  }

  public static MessageObjectiveAdd decode(final FriendlyByteBuf buffer) {
    return new MessageObjectiveAdd(buffer.readUUID(), new ObjectiveDataEntry(buffer.readNbt()));
  }

  public static void encode(final MessageObjectiveAdd message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeNbt(message.objectiveDataEntry.createTag());
  }

  public static void handle(
      MessageObjectiveAdd message, Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageObjectiveAdd message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate objective data
    ObjectiveDataEntry objectiveDataEntry = message.getObjectiveDataEntry();
    if (objectiveDataEntry == null) {
      log.error("Unable to add objective data for {} because it is null!", uuid);
      return;
    }

    // Verify objective data
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    ObjectiveData<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData == null) {
      log.error("Invalid objective data for {} from {}", message, serverPlayer);
      return;
    }

    // Perform action.
    if (objectiveData.addOrUpdateCustomObjective(objectiveDataEntry)) {
      log.debug("Added objective {} for {} from {}", objectiveDataEntry, easyNPC, serverPlayer);
      log.debug(
          "Available goals for {}: {}",
          easyNPC,
          objectiveData.getEntityGoalSelector().getAvailableGoals());
      log.debug(
          "Available targets for {}: {}",
          easyNPC,
          objectiveData.getEntityTargetSelector().getAvailableGoals());
    }
  }

  public ObjectiveDataEntry getObjectiveDataEntry() {
    return this.objectiveDataEntry;
  }
}
