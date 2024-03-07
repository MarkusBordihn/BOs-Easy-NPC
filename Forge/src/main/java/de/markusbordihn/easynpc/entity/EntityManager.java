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
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartedEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber
public class EntityManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String LOG_PREFIX = "[EntityManager]";
  private static final ConcurrentHashMap<UUID, EasyNPCEntity> npcEntityMap =
      new ConcurrentHashMap<>();

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    log.info("{} Prepare Entity Manager ...", LOG_PREFIX);
    npcEntityMap.clear();
  }

  @SubscribeEvent
  public static void handleServerStarted(ServerStartedEvent event) {
    log.info("{} Server started!", LOG_PREFIX);
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    // Ignore if event is canceled.
    if (event.isCanceled() || event.getWorld() == null || event.getEntity() == null) {
      return;
    }

    // Not all data needs to be processed on client side.
    boolean isClientSide = event.getWorld().isClientSide();

    // Only take care of Easy NPC entities.
    Entity entity = event.getEntity();
    if (entity instanceof EasyNPCEntity easyNPCEntity) {
      log.debug(
          "{} [Add] EASY NPC entity {}: {}", LOG_PREFIX, easyNPCEntity.getUUID(), easyNPCEntity);
      npcEntityMap.put(entity.getUUID(), easyNPCEntity);

      // Inform all server-side easy NPC entities about the new easyNPC.
      if (!isClientSide) {
        for (Entity entityEntry : npcEntityMap.values()) {
          if (entityEntry instanceof EasyNPCEntity easyNPCEntityChild
              && easyNPCEntityChild != easyNPCEntity) {
            easyNPCEntityChild.onEasyNPCJoin(easyNPCEntity);
          }
        }
      }
    }
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleEntityLeaveWorldEvent(EntityLeaveWorldEvent event) {
    // Ignore if event is canceled.
    if (event.isCanceled()) {
      return;
    }

    // Only take care of Easy NPC entities.
    Entity entity = event.getEntity();
    if (entity instanceof EasyNPCEntity easyNPCEntity
        && npcEntityMap.containsKey(entity.getUUID())) {
      log.debug("{} [Remove] EASY NPC entity {}: {}", LOG_PREFIX, entity.getUUID(), easyNPCEntity);
      npcEntityMap.remove(entity.getUUID());

      // Inform all server-side easy NPC entities about leaving easyNPC.
      for (Entity entityEntry : npcEntityMap.values()) {
        if (entityEntry instanceof EasyNPCEntity easyNPCEntityChild
            && easyNPCEntityChild != easyNPCEntity) {
          easyNPCEntityChild.onEasyNPCLeave(easyNPCEntity);
        }
      }
    }
  }

  public static Map<UUID, Entity> getEntityMapByOwner(UUID ownerUUID) {
    HashMap<UUID, Entity> result = new HashMap<>();
    for (var entry : npcEntityMap.entrySet()) {
      Entity entity = entry.getValue();
      if (entity instanceof EasyNPCEntity easyNPCEntity && easyNPCEntity.isOwner(ownerUUID)) {
        result.put(entry.getKey(), entity);
      }
    }
    return result;
  }

  public static EasyNPCEntity getEasyNPCEntityByUUID(UUID uuid, ServerPlayer serverPlayer) {
    if (uuid == null || serverPlayer == null || serverPlayer.getLevel() == null) {
      return null;
    }
    return getEasyNPCEntityByUUID(uuid, serverPlayer.getLevel());
  }

  public static EasyNPCEntity getEasyNPCEntityByUUID(UUID uuid, ServerLevel serverLevel) {
    if (uuid == null || serverLevel == null) {
      return null;
    }
    Entity entity = serverLevel.getEntity(uuid);
    if (entity instanceof EasyNPCEntity easyNPCEntity) {
      return easyNPCEntity;
    }
    return getEasyNPCEntityByUUID(uuid);
  }

  public static EasyNPCEntity getEasyNPCEntityByUUID(UUID uuid) {
    if (uuid == null) {
      return null;
    }
    Entity entity = npcEntityMap.getOrDefault(uuid, null);
    if (entity instanceof EasyNPCEntity easyNPCEntity) {
      return easyNPCEntity;
    }
    return null;
  }

  public static void discardEasyNPCEntityByUUID(UUID uuid, ServerPlayer serverPlayer) {
    EasyNPCEntity easyNPCEntity = getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPCEntity != null) {
      easyNPCEntity.discard();
      npcEntityMap.remove(uuid);
    }
  }
}
