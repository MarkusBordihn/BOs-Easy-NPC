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

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.api.event.EasyNPCEventRegistry;
import de.markusbordihn.easynpc.api.handler.EasyNPCShoulderHandler;
import de.markusbordihn.easynpc.data.action.ActionManager;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.handler.OwnerLoginRestoreHandler;
import de.markusbordihn.easynpc.menu.MenuManager;
import java.util.UUID;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.TickTask;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LivingEntityEvents {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String LOG_PREFIX = "[Living Entity Events]";

  protected LivingEntityEvents() {}

  public static void handleLivingEntityJoinEvent(LivingEntity livingEntity) {
    if (livingEntity == null) {
      return;
    }

    if (livingEntity instanceof EasyNPC<?> easyNPC) {
      if (livingEntity.level() instanceof ServerLevel serverLevel
          && isUUIDTakenByOtherEntity(livingEntity, serverLevel)) {
        log.warn(
            "{} Ignoring join of {} because its UUID {} is already taken.",
            LOG_PREFIX,
            easyNPC,
            livingEntity.getUUID());
        return;
      }

      LivingEntityManager.addEasyNPC(easyNPC);
      if (livingEntity.level() instanceof ServerLevel serverLevel) {
        handleServerEasyNPCJoin(easyNPC, livingEntity, serverLevel.getServer());
      }
    } else if (livingEntity instanceof ServerPlayer serverPlayer) {
      LivingEntityManager.addServerPlayer(serverPlayer);
      OwnerLoginRestoreHandler.onOwnerLogin(serverPlayer);
    } else {
      LivingEntityManager.addLivingEntity(livingEntity);
    }
  }

  public static void handleLivingEntityLeaveEvent(LivingEntity livingEntity) {
    if (livingEntity == null) {
      return;
    }

    if (livingEntity instanceof EasyNPC<?> easyNPC) {
      if (!livingEntity.level().isClientSide()) {
        UUID entityUUID = easyNPC.getEntityUUID();
        NPCRemovalReason intentionalReason =
            NPCEntityManager.takeIntentionalRemovalReason(entityUUID);
        Entity.RemovalReason reason = livingEntity.getRemovalReason();
        NPCRemovalReason removalReason =
            intentionalReason != null ? intentionalReason : resolveRemovalReason(reason);
        if (intentionalReason == null && reason == Entity.RemovalReason.DISCARDED) {
          log.debug(
              "{} {} was discarded at {} in {} without being saved.",
              LOG_PREFIX,
              easyNPC,
              livingEntity.blockPosition(),
              livingEntity.level().dimension().location());
        } else {
          log.debug(
              "{} Removed {} ({}) at {} in {} with reason {}.",
              LOG_PREFIX,
              easyNPC,
              entityUUID,
              livingEntity.blockPosition(),
              livingEntity.level().dimension().location(),
              removalReason);
          if (intentionalReason == null && easyNPC.isPersistentNPC()) {
            NPCEntityManager.saveNPC(easyNPC, removalReason);
            if (removalReason == NPCRemovalReason.UNLOADED_TO_CHUNK) {
              NPCChurnTracker.trackChunkUnload(easyNPC);
            }
          }
        }
        EasyNPCEventRegistry.fireNPCRemoved(easyNPC, removalReason);
        NPCEntityManager.releaseNPC(entityUUID);
        ActionManager.removeMob(easyNPC.getMob());
      }
      LivingEntityManager.removeEasyNPC(easyNPC);
    } else if (livingEntity instanceof ServerPlayer serverPlayer) {
      LivingEntityManager.removeServerPlayer(serverPlayer);
      MenuManager.cleanupPlayerMenus(serverPlayer);
      ActionManager.removeServerPlayer(serverPlayer);
    } else {
      LivingEntityManager.removeLivingEntity(livingEntity);
    }
  }

  // Forge fires EntityJoinLevelEvent before addWithUUID rejects a taken UUID.
  private static boolean isUUIDTakenByOtherEntity(Entity entity, ServerLevel serverLevel) {
    Entity existingEntity = serverLevel.getEntity(entity.getUUID());
    return existingEntity != null && existingEntity != entity;
  }

  private static void handleServerEasyNPCJoin(
      EasyNPC<?> easyNPC, LivingEntity livingEntity, MinecraftServer server) {
    NPCEntityManager.ensureInitialized(server);
    UUID entityUUID = easyNPC.getEntityUUID();
    if (NPCEntityManager.isNPCDeleted(entityUUID)) {
      log.debug("{} Removing deleted {} ({}) on load.", LOG_PREFIX, easyNPC, entityUUID);
      NPCEntityManager.markIntentionalRemoval(entityUUID, NPCRemovalReason.DELETED);
      // Forge fires EntityJoinLevelEvent before the entity is tracked, so discard on the next tick.
      server.tell(new TickTask(server.getTickCount(), livingEntity::discard));
      return;
    }

    boolean isStored = NPCEntityManager.hasStoredNPC(entityUUID);
    boolean wasOnShoulder = isStored && EasyNPCShoulderHandler.isOnShoulder(entityUUID);
    boolean isPersistent = easyNPC.isPersistentNPC();
    if (!isPersistent) {
      if (isStored) {
        NPCEntityManager.removeNPC(entityUUID);
      }
    } else {
      if (isStored) {
        NPCEntityManager.updateRemovalReason(entityUUID, NPCRemovalReason.NONE);
      } else {
        NPCEntityManager.saveNPC(easyNPC);
      }
      NPCEntityManager.evictFromCache(entityUUID);
    }

    EasyNPCEventRegistry.fireNPCSpawned(easyNPC);
    if (isPersistent && !isStored) {
      fireInitialOwner(easyNPC);
    }
    if (wasOnShoulder) {
      EasyNPCEventRegistry.fireShoulderDismounted(easyNPC);
    }
  }

  private static void fireInitialOwner(EasyNPC<?> easyNPC) {
    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (ownerData == null || !ownerData.hasNPCOwner()) {
      return;
    }

    EasyNPCEventRegistry.fireOwnerChanged(easyNPC, null, ownerData.getOwnerUUID());
  }

  private static NPCRemovalReason resolveRemovalReason(Entity.RemovalReason removalReason) {
    if (removalReason == null) {
      return NPCRemovalReason.UNLOADED_TO_CHUNK;
    }

    return NPCRemovalReason.fromRemovalReason(removalReason);
  }
}
