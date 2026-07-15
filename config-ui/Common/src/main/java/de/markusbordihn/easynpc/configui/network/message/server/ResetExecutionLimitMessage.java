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

package de.markusbordihn.easynpc.configui.network.message.server;

import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.data.execution.ExecutionId;
import de.markusbordihn.easynpc.data.execution.ExecutionType;
import de.markusbordihn.easynpc.data.saveddata.ActionExecutionTracker;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.permissions.Permissions;

public record ResetExecutionLimitMessage(ExecutionId executionId, boolean allPlayers)
    implements NetworkMessageRecord {

  public static final Identifier MESSAGE_ID =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "reset_execution_limit");
  public static final Type<ResetExecutionLimitMessage> PAYLOAD_TYPE = new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ResetExecutionLimitMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), ResetExecutionLimitMessage::create);

  public static ResetExecutionLimitMessage create(final FriendlyByteBuf buffer) {
    return new ResetExecutionLimitMessage(
        new ExecutionId(buffer.readEnum(ExecutionType.class), buffer.readUUID()),
        buffer.readBoolean());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeEnum(this.executionId.type());
    buffer.writeUUID(this.executionId.value());
    buffer.writeBoolean(this.allPlayers);
  }

  @Override
  public Identifier id() {
    return MESSAGE_ID;
  }

  @Override
  public Type<? extends CustomPacketPayload> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public void handleServer(final ServerPlayer serverPlayer) {
    if (this.executionId == null) {
      return;
    }

    ActionExecutionTracker tracker = ActionExecutionTracker.get(serverPlayer.level());
    if (this.allPlayers) {
      if (!serverPlayer.permissions().hasPermission(Permissions.COMMANDS_GAMEMASTER)) {
        log.warn(
            "Player {} tried to reset executions for all players without permission",
            serverPlayer.getName().getString());
        return;
      }
      tracker.resetExecutionForAllPlayers(this.executionId);
      log.info(
          "Player {} reset execution limit for all players for execution {}",
          serverPlayer.getName().getString(),
          this.executionId);
    } else {
      tracker.resetExecution(serverPlayer.getUUID(), this.executionId);
      log.debug(
          "Player {} reset execution limit for execution {}",
          serverPlayer.getName().getString(),
          this.executionId);
    }
  }
}
