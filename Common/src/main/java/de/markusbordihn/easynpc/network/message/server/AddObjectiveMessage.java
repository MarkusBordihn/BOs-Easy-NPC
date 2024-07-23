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
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record AddObjectiveMessage(UUID uuid, ObjectiveDataEntry objectiveDataEntry)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "add_objective");
  public static final CustomPacketPayload.Type<AddObjectiveMessage> PAYLOAD_TYPE =
      CustomPacketPayload.createType(MESSAGE_ID.toString());
  public static final StreamCodec<RegistryFriendlyByteBuf, AddObjectiveMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), AddObjectiveMessage::create);

  public static AddObjectiveMessage create(final FriendlyByteBuf buffer) {
    return new AddObjectiveMessage(buffer.readUUID(), new ObjectiveDataEntry(buffer.readNbt()));
  }

  @Override
  public void write(FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeNbt(this.objectiveDataEntry.createTag());
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
    ObjectiveData<?> objectiveData = easyNPC != null ? easyNPC.getEasyNPCObjectiveData() : null;

    if (easyNPC == null || this.objectiveDataEntry == null || objectiveData == null) {
      log.error("Failed to add/update objective for {}: Invalid data", this.uuid);
      return;
    }

    if (objectiveData.addOrUpdateCustomObjective(objectiveDataEntry)) {
      log.debug("Objective updated for {}: {}", easyNPC, objectiveDataEntry);
      log.debug("Goals: {}", objectiveData.getEntityGoalSelector().getAvailableGoals());
      log.debug("Targets: {}", objectiveData.getEntityTargetSelector().getAvailableGoals());
    }
  }
}
