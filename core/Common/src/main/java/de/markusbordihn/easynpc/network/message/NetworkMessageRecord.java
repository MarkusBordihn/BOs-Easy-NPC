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
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.dialog.DialogMenu;
import io.netty.buffer.Unpooled;
import java.util.Objects;
import java.util.Optional;
import java.util.Random;
import java.util.UUID;
import java.util.function.Function;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface NetworkMessageRecord extends CustomPacketPayload {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  UUID EMPTY_UUID = new UUID(0L, 0L);

  Random RANDOM = new Random();

  int MAX_NAME_LENGTH = 256;

  static <M extends NetworkMessageRecord> Function<FriendlyByteBuf, M> guardedDecoder(
      final ResourceLocation messageID, final Function<FriendlyByteBuf, M> creator) {
    return buffer -> {
      try {
        return creator.apply(buffer);
      } catch (Exception exception) {
        // Netty thread, client-triggerable at will: a stacktrace per packet would flood the log.
        log.error("Dropped malformed network message {}: {}", messageID, exception.toString());
        return null;
      }
    };
  }

  static Runnable guardedHandler(final ResourceLocation messageID, final Runnable handler) {
    return () -> {
      try {
        handler.run();
      } catch (Exception exception) {
        log.error("Failed to handle network message {}", messageID, exception);
      }
    };
  }

  // FriendlyByteBuf.writeEnum sends Enum#ordinal as a VarInt and readEnum indexes it unchecked.
  static <E extends Enum<E>> E readEnum(final FriendlyByteBuf buffer, final Class<E> enumClass) {
    int ordinal = buffer.readVarInt();
    E[] constants = enumClass.getEnumConstants();
    if (ordinal < 0 || ordinal >= constants.length) {
      log.error("Received out of range ordinal {} for {}", ordinal, enumClass.getSimpleName());
      return null;
    }

    return constants[ordinal];
  }

  // FriendlyByteBuf.readOptional wraps the value with Optional.of, which rejects a failed read.
  static <E extends Enum<E>> Optional<E> readOptionalEnum(
      final FriendlyByteBuf buffer, final Class<E> enumClass) {
    if (!buffer.readBoolean()) {
      return Optional.empty();
    }

    return Optional.ofNullable(readEnum(buffer, enumClass));
  }

  static boolean isInRange(final double value, final double min, final double max) {
    return Double.isFinite(value) && value >= min && value <= max;
  }

  static boolean checkAccess(final UUID uuid, final ServerPlayer serverPlayer) {
    if (uuid == null || uuid.equals(EMPTY_UUID)) {
      log.error("Unable to get valid entity UUID {} for {}", uuid, serverPlayer);
      return false;
    }

    if (serverPlayer == null) {
      log.error("Unable to get valid player for entity with UUID {}", uuid);
      return false;
    }

    EasyNPC<?> easyNPC = LivingEntityManager.getServerEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      log.error("Unable to get valid entity with UUID {} for {}", uuid, serverPlayer);
      serverPlayer.sendSystemMessage(
          Component.translatable("message.easynpc.access.npc_not_found"));
      return false;
    }

    if (!LivingEntityManager.hasAccess(uuid, serverPlayer)) {
      log.error("User {} has no access to Easy NPC with uuid {}.", serverPlayer, uuid);
      serverPlayer.sendSystemMessage(
          Component.translatable("message.easynpc.access.no_permission"));
      return false;
    }

    return true;
  }

  ResourceLocation id();

  void write(FriendlyByteBuf friendlyByteBuf);

  default FriendlyByteBuf payload() {
    FriendlyByteBuf friendlyByteBuf = new FriendlyByteBuf(Unpooled.buffer());
    write(friendlyByteBuf);
    return friendlyByteBuf;
  }

  default void handleClient() {
    log.error("Network message client handler not implemented for {}", this);
  }

  default void handleServer(ServerPlayer serverPlayer) {
    log.error("Network message server handler not implemented for {}", this);
  }

  default EasyNPC<?> getEasyNPC(final UUID uuid, final ServerPlayer serverPlayer) {
    if (uuid == null || uuid.equals(EMPTY_UUID)) {
      log.error("Invalid Easy NPC UUID {} from {}", uuid, serverPlayer);
      return null;
    }

    if (serverPlayer == null) {
      log.error("Invalid server player for Easy NPC with UUID {}", uuid);
      return null;
    }
    return LivingEntityManager.getServerEasyNPCEntityByUUID(uuid, serverPlayer);
  }

  default EasyNPC<?> getEasyNPCAndCheckAccess(final UUID uuid, final ServerPlayer serverPlayer) {
    return checkAccess(uuid, serverPlayer) ? getEasyNPC(uuid, serverPlayer) : null;
  }

  default boolean checkDialogSession(final UUID uuid, final ServerPlayer serverPlayer) {
    return checkDialogSession(uuid, null, serverPlayer);
  }

  default boolean checkDialogSession(
      final UUID uuid, final UUID dialogId, final ServerPlayer serverPlayer) {
    if (uuid == null || serverPlayer == null) {
      log.warn("Blocked dialog action without valid context from {}", serverPlayer);
      return false;
    }

    if (!(serverPlayer.containerMenu instanceof DialogMenu dialogMenu)) {
      log.warn(
          "Blocked dialog action without active dialog menu for {} from {}", uuid, serverPlayer);
      return false;
    }

    ScreenData screenData = dialogMenu.getScreenData();
    if (screenData == null || !uuid.equals(screenData.uuid())) {
      log.warn(
          "Blocked dialog action with invalid menu context for {} from {}", uuid, serverPlayer);
      return false;
    }

    if (dialogId != null && !Objects.equals(dialogId, screenData.dialogId())) {
      log.warn(
          "Blocked dialog action for dialog {} with menu dialog {} from {}",
          dialogId,
          screenData.dialogId(),
          serverPlayer);
      return false;
    }

    return true;
  }
}
