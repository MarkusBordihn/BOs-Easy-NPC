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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.api.handler.EasyNPCEntityHandler;
import de.markusbordihn.easynpc.data.npc.NPCEntityMetadata;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.EnumSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.core.BlockPos;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityDimensions;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class OwnerLoginRestoreHandler {

  public static final int RESTORE_DELAY_TICKS = 20;

  public static final int MAX_RESTORED_NPCS_PER_OWNER = 16;
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final Set<NPCRemovalReason> RESTORABLE_REMOVAL_REASONS =
      EnumSet.of(
          NPCRemovalReason.DESPAWNED,
          NPCRemovalReason.UNLOADED_WITH_PLAYER,
          NPCRemovalReason.UNLOADED_BY_PLAYER,
          NPCRemovalReason.UNLOADED_BY_SERVER,
          NPCRemovalReason.UNLOADED_BY_ACTION);

  private static final Map<UUID, Integer> pendingRestores = new ConcurrentHashMap<>();

  private OwnerLoginRestoreHandler() {}

  public static void onOwnerLogin(ServerPlayer serverPlayer) {
    if (serverPlayer == null) {
      return;
    }

    pendingRestores.put(serverPlayer.getUUID(), RESTORE_DELAY_TICKS);
  }

  public static void handleServerTick(MinecraftServer minecraftServer) {
    if (minecraftServer == null || pendingRestores.isEmpty()) {
      return;
    }

    Deque<UUID> dueOwners = new ArrayDeque<>();
    pendingRestores.replaceAll(
        (ownerUUID, remainingTicks) -> {
          if (remainingTicks <= 1) {
            dueOwners.add(ownerUUID);
            return 0;
          }
          return remainingTicks - 1;
        });

    for (UUID ownerUUID : dueOwners) {
      pendingRestores.remove(ownerUUID);
      ServerPlayer serverPlayer = minecraftServer.getPlayerList().getPlayer(ownerUUID);
      if (serverPlayer != null) {
        restoreNPCsOf(serverPlayer);
      }
    }
  }

  private static void restoreNPCsOf(ServerPlayer serverPlayer) {
    ServerLevel serverLevel = serverPlayer.serverLevel();
    int restoredCount = 0;

    for (var entry : NPCEntityManager.getNPCsByOwner(serverPlayer.getUUID())) {
      if (restoredCount >= MAX_RESTORED_NPCS_PER_OWNER) {
        log.warn(
            "Reached the restore limit of {} NPCs for {}, skipping the remaining ones.",
            MAX_RESTORED_NPCS_PER_OWNER,
            serverPlayer.getName().getString());
        break;
      }

      if (!isRestorable(entry.metadata())
          || LivingEntityManager.getServerEasyNPCEntityByUUID(entry.entityUUID(), serverLevel)
              != null) {
        continue;
      }

      if (restoreNPC(entry.entityUUID(), serverPlayer, serverLevel)) {
        restoredCount++;
      }
    }

    if (restoredCount > 0) {
      log.info("Restored {} NPC(s) for {}", restoredCount, serverPlayer.getName().getString());
    }
  }

  private static boolean isRestorable(NPCEntityMetadata metadata) {
    return metadata != null
        && metadata.restoreOnOwnerLogin()
        && RESTORABLE_REMOVAL_REASONS.contains(metadata.removalReason());
  }

  private static boolean restoreNPC(
      UUID entityUUID, ServerPlayer serverPlayer, ServerLevel serverLevel) {
    Optional<BlockPos> spawnPosition =
        PlacementHandler.findSafeSpawnNear(
            serverLevel, serverPlayer.position(), EntityDimensions.scalable(0.6F, 1.8F));
    if (spawnPosition.isEmpty()) {
      log.warn("Found no free spot to restore NPC {} near {}", entityUUID, serverPlayer.position());
      return false;
    }

    return EasyNPCEntityHandler.spawn(
        entityUUID, serverLevel, Vec3.atBottomCenterOf(spawnPosition.get()));
  }
}
