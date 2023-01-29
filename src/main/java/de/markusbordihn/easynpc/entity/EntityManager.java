/**
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

import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;

import net.minecraftforge.event.entity.EntityJoinLevelEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.easynpc.Constants;

@EventBusSubscriber
public class EntityManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static ConcurrentHashMap<UUID, Entity> entityMap = new ConcurrentHashMap<>();

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleEntityJoinWorldEvent(EntityJoinLevelEvent event) {

    // Ignore if event is canceled.
    if (event.isCanceled()) {
      return;
    }

    // Only take care of Easy NPC entities.
    Entity entity = event.getEntity();
    if (entity instanceof EasyNPCEntity easyNPCEntity) {
      log.info("Found EASY NPC entity {}: {}", entity.getUUID(), easyNPCEntity);
      entityMap.put(entity.getUUID(), entity);
    }
  }

  public static Entity getEntityByUUID(UUID uuid) {
    return entityMap.getOrDefault(uuid, null);
  }

  public static EasyNPCEntity getEasyNPCEntityByUUID(UUID uuid, ServerPlayer serverPlayer) {
    ServerLevel serverLevel = serverPlayer.getLevel();
    Entity entity = serverLevel.getEntity(uuid);
    if (entity instanceof EasyNPCEntity easyNPCEntity) {
      return easyNPCEntity;
    }
    return getEasyNPCEntityByUUID(uuid);
  }

  public static EasyNPCEntity getEasyNPCEntityByUUID(UUID uuid) {
    Entity entity = entityMap.getOrDefault(uuid, null);
    if (entity instanceof EasyNPCEntity easyNPCEntity) {
      return easyNPCEntity;
    }
    return null;
  }

  public static boolean hasAccess(UUID uuid, ServerPlayer serverPlayer) {
    if (uuid == null || serverPlayer == null) {
      return false;
    }
    return hasAccess(serverPlayer.getLevel().getEntity(uuid), serverPlayer);
  }

  public static boolean hasAccess(Entity entity, ServerPlayer serverPlayer) {
    if (entity instanceof EasyNPCEntity easyNPCEntity) {
      return hasAccess(easyNPCEntity, serverPlayer);
    }
    return false;
  }

  public static boolean hasAccess(EasyNPCEntity entity, ServerPlayer serverPlayer) {
    // Allow admins and creative mode
    if (serverPlayer.isCreative()) {
      return true;
    }

    // Perform more specific checks
    if (entity.hasOwner()) {
      UUID uuid = entity.getOwnerUUID();
      return uuid != null && uuid.equals(serverPlayer.getUUID());
    }

    return false;
  }
}
