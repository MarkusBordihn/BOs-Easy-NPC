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
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class RemoveObjectiveMessage extends NetworkMessage<RemoveObjectiveMessage> {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "remove_objective");

  protected final ObjectiveDataEntry objectiveDataEntry;

  public RemoveObjectiveMessage(final UUID uuid, final ObjectiveDataEntry objectiveDataEntry) {
    super(uuid);
    this.objectiveDataEntry = objectiveDataEntry;
  }

  public static RemoveObjectiveMessage decode(final FriendlyByteBuf buffer) {
    return new RemoveObjectiveMessage(buffer.readUUID(), new ObjectiveDataEntry(buffer.readNbt()));
  }

  public static FriendlyByteBuf encode(
      final RemoveObjectiveMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeNbt(message.objectiveDataEntry.createTag());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(final RemoveObjectiveMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate objective data
    ObjectiveDataEntry objectiveDataEntry = message.getObjectiveData();
    if (objectiveDataEntry == null) {
      log.error("Unable to add objective data for {} because it is null!", message.getUUID());
      return;
    }

    // Verify objective data
    EasyNPC<?> easyNPC = message.getEasyNPC();
    ObjectiveData<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData == null) {
      log.error("Invalid objective data for {} from {}", message, serverPlayer);
      return;
    }

    // Perform action.
    if (objectiveData.removeCustomObjective(objectiveDataEntry)) {
      log.debug("Removed objective {} for {} from {}", objectiveDataEntry, easyNPC, serverPlayer);
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

  @Override
  public FriendlyByteBuf encodeBuffer(FriendlyByteBuf buffer) {
    return encode(this, buffer);
  }

  @Override
  public RemoveObjectiveMessage decodeBuffer(FriendlyByteBuf buffer) {
    return decode(buffer);
  }

  public ObjectiveDataEntry getObjectiveData() {
    return this.objectiveDataEntry;
  }
}
