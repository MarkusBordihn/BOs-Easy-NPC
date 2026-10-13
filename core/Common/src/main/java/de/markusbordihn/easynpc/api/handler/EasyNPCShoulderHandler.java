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

import de.markusbordihn.easynpc.access.ShoulderAccessHelper;
import de.markusbordihn.easynpc.api.event.EasyNPCEventRegistry;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.ShoulderSide;
import de.markusbordihn.easynpc.entity.EasyNPCEntityTypes;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntitySpawnRequest;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.level.storage.TagValueOutput;

public class EasyNPCShoulderHandler {

  private static final String ENTITY_UUID_TAG = "UUID";
  private static final double RESPAWN_HEIGHT_OFFSET = 0.7D;

  private EasyNPCShoulderHandler() {}

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
    boolean respawned =
        EntityType.create(
                TagValueInput.create(
                    ProblemReporter.DISCARDING, serverLevel.registryAccess(), entityTag.get()),
                serverLevel,
                new EntitySpawnRequest(EntitySpawnReason.LOAD, false))
            .map(
                entity -> {
                  entity.setPos(
                      serverPlayer.getX(),
                      serverPlayer.getY() + RESPAWN_HEIGHT_OFFSET,
                      serverPlayer.getZ());
                  return serverLevel.addWithUUID(entity);
                })
            .orElse(false);
    shoulderAccess.clearShoulderEntity(shoulderSide);
    return respawned;
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
