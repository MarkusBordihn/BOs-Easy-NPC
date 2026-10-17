/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.api.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.access.ShoulderAccessHelper;
import de.markusbordihn.easynpc.api.event.EasyNPCEventRegistry;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.ShoulderSide;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.EasyNPCEntityTypes;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.level.storage.TagValueOutput;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCShoulderHandler {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String ENTITY_UUID_TAG = "UUID";
  private static final double RESPAWN_HEIGHT_OFFSET = 0.7D;

  // Vanilla calls ServerPlayer.removeEntitiesOnShoulder every tick while the player is in water or
  // flying.
  private static final long DISMOUNT_FAILURE_LOG_INTERVAL_TICKS = 100L;

  private static long lastDismountFailureLogGameTime = -DISMOUNT_FAILURE_LOG_INTERVAL_TICKS;

  private EasyNPCShoulderHandler() {}

  private static void logDismountFailure(
      ServerLevel serverLevel, String message, Object... parameters) {
    long ticksSinceLastLog = serverLevel.getGameTime() - lastDismountFailureLogGameTime;
    if (!log.isDebugEnabled()
        || (ticksSinceLastLog >= 0 && ticksSinceLastLog < DISMOUNT_FAILURE_LOG_INTERVAL_TICKS)) {
      return;
    }

    lastDismountFailureLogGameTime = serverLevel.getGameTime();
    log.debug(message, parameters);
  }

  public static boolean sitOnShoulder(EasyNPC<?> easyNPC, Player player) {
    if (easyNPC == null || !(player instanceof ServerPlayer serverPlayer)) {
      return false;
    }

    Optional<ShoulderSide> freeShoulderSide = getFreeShoulderSide(serverPlayer);
    if (freeShoulderSide.isEmpty()) {
      return false;
    }

    Entity entity = easyNPC.getEntity();
    TagValueOutput entityOutput =
        TagValueOutput.createWithContext(ProblemReporter.DISCARDING, entity.registryAccess());
    if (!entity.save(entityOutput)
        || !serverPlayer.setEntityOnShoulder(entityOutput.buildResult())) {
      return false;
    }

    EasyNPCEntityHandler.despawn(easyNPC, NPCRemovalReason.ON_SHOULDER);
    EasyNPCEventRegistry.fireShoulderMounted(easyNPC, player, freeShoulderSide.get());
    return true;
  }

  public static boolean dismount(Player player) {
    boolean leftDismounted = dismount(player, ShoulderSide.LEFT);
    boolean rightDismounted = dismount(player, ShoulderSide.RIGHT);
    return leftDismounted || rightDismounted;
  }

  public static boolean dismount(Player player, ShoulderSide shoulderSide) {
    return dismount(player, shoulderSide, 0);
  }

  public static boolean dismount(
      Player player, ShoulderSide shoulderSide, int remountCooldownTicks) {
    if (shoulderSide == null
        || !(player instanceof ServerPlayer serverPlayer)
        || !(serverPlayer instanceof ShoulderAccessHelper shoulderAccess)) {
      return false;
    }

    Optional<CompoundTag> entityTag = getShoulderNPCData(serverPlayer, shoulderSide);
    if (entityTag.isEmpty()) {
      return false;
    }

    ServerLevel serverLevel = serverPlayer.level();
    Optional<Entity> entity =
        EntityType.create(
            TagValueInput.create(
                ProblemReporter.DISCARDING, serverLevel.registryAccess(), entityTag.get()),
            serverLevel,
            EntitySpawnReason.LOAD);
    if (entity.isEmpty() || !moveToFreeDismountPosition(entity.get(), serverPlayer, serverLevel)) {
      logDismountFailure(
          serverLevel,
          "Unable to find a free position to put the NPC on the {} shoulder of {} down.",
          shoulderSide,
          serverPlayer.getName().getString());
      return false;
    }

    if (remountCooldownTicks > 0 && entity.get() instanceof EasyNPC<?> easyNPC) {
      blockRemountUntil(easyNPC, serverLevel.getGameTime() + remountCooldownTicks);
    }

    if (!serverLevel.addWithUUID(entity.get())) {
      logDismountFailure(
          serverLevel,
          "Unable to put the NPC {} on the {} shoulder of {} down.",
          entity.get().getUUID(),
          shoulderSide,
          serverPlayer.getName().getString());
      return false;
    }

    shoulderAccess.clearShoulderEntity(shoulderSide);
    return true;
  }

  public static Optional<CompoundTag> dismountOrTakeShoulderNPCData(
      Player player, ShoulderSide shoulderSide) {
    Optional<CompoundTag> entityTag = getShoulderNPCData(player, shoulderSide);
    if (entityTag.isEmpty()
        || !(player instanceof ShoulderAccessHelper shoulderAccess)
        || dismount(player, shoulderSide)) {
      return Optional.empty();
    }

    shoulderAccess.clearShoulderEntity(shoulderSide);
    return entityTag;
  }

  public static long getRemountAllowedGameTime(EasyNPC<?> easyNPC) {
    StatusDataCapable<?> statusData = easyNPC.getEasyNPCStatusData();
    return statusData != null
        ? statusData.getStatusDataTimestamp(StatusDataType.SHOULDER_REMOUNT_ALLOWED_GAME_TIME)
        : 0L;
  }

  private static void blockRemountUntil(EasyNPC<?> easyNPC, long remountAllowedGameTime) {
    StatusDataCapable<?> statusData = easyNPC.getEasyNPCStatusData();
    if (statusData != null) {
      statusData.setStatusDataTimestamp(
          StatusDataType.SHOULDER_REMOUNT_ALLOWED_GAME_TIME, remountAllowedGameTime);
    }
  }

  private static boolean moveToFreeDismountPosition(
      Entity entity, ServerPlayer serverPlayer, ServerLevel serverLevel) {
    entity.setPos(
        serverPlayer.getX(), serverPlayer.getY() + RESPAWN_HEIGHT_OFFSET, serverPlayer.getZ());
    if (serverLevel.noCollision(entity)) {
      return true;
    }

    entity.setPos(serverPlayer.getX(), serverPlayer.getY(), serverPlayer.getZ());
    return serverLevel.noCollision(entity);
  }

  public static Optional<CompoundTag> getShoulderNPCData(Player player, ShoulderSide shoulderSide) {
    if (!(player instanceof ServerPlayer serverPlayer) || shoulderSide == null) {
      return Optional.empty();
    }

    CompoundTag entityTag =
        shoulderSide == ShoulderSide.LEFT
            ? serverPlayer.getShoulderEntityLeft()
            : serverPlayer.getShoulderEntityRight();
    return EasyNPCEntityTypes.isEasyNPCEntityTag(entityTag, serverPlayer.level())
        ? Optional.of(entityTag.copy())
        : Optional.empty();
  }

  public static Optional<UUID> getShoulderNPCUUID(Player player, ShoulderSide shoulderSide) {
    return getShoulderNPCData(player, shoulderSide)
        .flatMap(entityTag -> entityTag.read(ENTITY_UUID_TAG, UUIDUtil.CODEC));
  }

  public static boolean isOnShoulder(UUID entityUUID) {
    return NPCEntityManager.getNPC(entityUUID)
        .map(entry -> entry.metadata().removalReason() == NPCRemovalReason.ON_SHOULDER)
        .orElse(false);
  }

  private static Optional<ShoulderSide> getFreeShoulderSide(ServerPlayer serverPlayer) {
    if (serverPlayer.getShoulderEntityLeft().isEmpty()) {
      return Optional.of(ShoulderSide.LEFT);
    }

    if (serverPlayer.getShoulderEntityRight().isEmpty()) {
      return Optional.of(ShoulderSide.RIGHT);
    }

    return Optional.empty();
  }
}
