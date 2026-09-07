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
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.OwnerLoginRestoreHandler;
import de.markusbordihn.easynpc.menu.MenuManager;
import java.util.UUID;
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
      LivingEntityManager.addEasyNPC(easyNPC);
      if (!livingEntity.level().isClientSide()) {
        UUID entityUUID = easyNPC.getEntityUUID();
        if (NPCEntityManager.hasStoredNPC(entityUUID)) {
          NPCEntityManager.updateRemovalReason(entityUUID, NPCRemovalReason.NONE);
        } else {
          NPCEntityManager.saveNPC(easyNPC);
        }
        NPCEntityManager.evictFromCache(entityUUID);
        EasyNPCEventRegistry.fireNPCSpawned(easyNPC);
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
        NPCRemovalReason intentionalReason =
            NPCEntityManager.takeIntentionalRemovalReason(easyNPC.getEntityUUID());
        Entity.RemovalReason reason = livingEntity.getRemovalReason();
        NPCRemovalReason removalReason =
            intentionalReason != null ? intentionalReason : resolveRemovalReason(reason);
        if (intentionalReason == null && reason == Entity.RemovalReason.DISCARDED) {
          log.warn(
              "{} {} was discarded at {} in {} without being saved, its latest changes are lost!",
              LOG_PREFIX,
              easyNPC,
              livingEntity.blockPosition(),
              livingEntity.level().dimension().location());
        } else {
          log.debug(
              "{} Removed {} ({}) at {} in {} with reason {}.",
              LOG_PREFIX,
              easyNPC,
              easyNPC.getEntityUUID(),
              livingEntity.blockPosition(),
              livingEntity.level().dimension().location(),
              removalReason);
          if (intentionalReason == null) {
            NPCEntityManager.saveNPC(easyNPC, removalReason);
            if (removalReason == NPCRemovalReason.UNLOADED_TO_CHUNK) {
              NPCChurnTracker.trackChunkUnload(easyNPC);
            }
          }
        }
        EasyNPCEventRegistry.fireNPCRemoved(easyNPC, removalReason);
      }
      LivingEntityManager.removeEasyNPC(easyNPC);
    } else if (livingEntity instanceof ServerPlayer serverPlayer) {
      LivingEntityManager.removeServerPlayer(serverPlayer);
      MenuManager.cleanupPlayerMenus(serverPlayer);
    } else {
      LivingEntityManager.removeLivingEntity(livingEntity);
    }
  }

  private static NPCRemovalReason resolveRemovalReason(Entity.RemovalReason removalReason) {
    if (removalReason == null) {
      return NPCRemovalReason.UNLOADED_TO_CHUNK;
    }

    return NPCRemovalReason.fromRemovalReason(removalReason);
  }
}
