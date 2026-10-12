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
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.player.Player;

public class EasyNPCShoulderHandler {

  private static final String ENTITY_UUID_TAG = "UUID";
  private static final double RESPAWN_HEIGHT_OFFSET = 0.7D;

  private EasyNPCShoulderHandler() {}

  public static boolean sitOnShoulder(EasyNPC<?> easyNPC, Player player) {
    if (easyNPC == null || player == null || player.level().isClientSide()) {
      return false;
    }

    Optional<ShoulderSide> freeShoulderSide = getFreeShoulderSide(player);
    if (freeShoulderSide.isEmpty()) {
      return false;
    }

    CompoundTag entityTag = new CompoundTag();
    if (!easyNPC.getEntity().save(entityTag) || !player.setEntityOnShoulder(entityTag)) {
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
    if (player == null
        || shoulderSide == null
        || !(player.level() instanceof ServerLevel serverLevel)
        || !(player instanceof ShoulderAccessHelper shoulderAccess)) {
      return false;
    }

    Optional<CompoundTag> entityTag = getShoulderNPCData(player, shoulderSide);
    if (entityTag.isEmpty()) {
      return false;
    }

    boolean respawned =
        EntityType.create(entityTag.get(), serverLevel)
            .map(
                entity -> {
                  entity.setPos(
                      player.getX(), player.getY() + RESPAWN_HEIGHT_OFFSET, player.getZ());
                  return serverLevel.addWithUUID(entity);
                })
            .orElse(false);
    shoulderAccess.clearShoulderEntity(shoulderSide);
    return respawned;
  }

  public static Optional<CompoundTag> getShoulderNPCData(Player player, ShoulderSide shoulderSide) {
    if (player == null || shoulderSide == null) {
      return Optional.empty();
    }

    CompoundTag entityTag =
        shoulderSide == ShoulderSide.LEFT
            ? player.getShoulderEntityLeft()
            : player.getShoulderEntityRight();
    return EasyNPCEntityTypes.isEasyNPCEntityTag(entityTag, player.level())
        ? Optional.of(entityTag.copy())
        : Optional.empty();
  }

  public static Optional<UUID> getShoulderNPCUUID(Player player, ShoulderSide shoulderSide) {
    return getShoulderNPCData(player, shoulderSide)
        .filter(entityTag -> entityTag.hasUUID(ENTITY_UUID_TAG))
        .map(entityTag -> entityTag.getUUID(ENTITY_UUID_TAG));
  }

  public static boolean isOnShoulder(UUID entityUUID) {
    return NPCEntityManager.getNPC(entityUUID)
        .map(entry -> entry.metadata().removalReason() == NPCRemovalReason.ON_SHOULDER)
        .orElse(false);
  }

  private static Optional<ShoulderSide> getFreeShoulderSide(Player player) {
    if (player.getShoulderEntityLeft().isEmpty()) {
      return Optional.of(ShoulderSide.LEFT);
    }

    if (player.getShoulderEntityRight().isEmpty()) {
      return Optional.of(ShoulderSide.RIGHT);
    }

    return Optional.empty();
  }
}
