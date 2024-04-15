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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LivingEntityManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String LOG_PREFIX = "[Living Entity Manager]";

  private static final ConcurrentHashMap<String, ServerPlayer> playerNameMap =
      new ConcurrentHashMap<>();

  private static final ConcurrentHashMap<UUID, EasyNPC<?>> npcEntityMap = new ConcurrentHashMap<>();

  private static final ConcurrentHashMap<UUID, ServerPlayer> playerMap = new ConcurrentHashMap<>();

  private LivingEntityManager() {}

  public static void addEasyNPC(EasyNPC<?> easyNPC) {
    log.debug(
        "{} [Add] EASY NPC entity {}: {}", LOG_PREFIX, easyNPC, easyNPC.getEntity().getUUID());
    npcEntityMap.put(easyNPC.getEntity().getUUID(), easyNPC);

    // Inform all server-side easy NPC entities about the new easyNPC.
    if (!easyNPC.isClientSide()) {
      for (EasyNPC<?> easyNPCChild : npcEntityMap.values()) {
        if (easyNPCChild != easyNPC) {
          easyNPCChild.handleEasyNPCJoin(easyNPC);
        }
      }
    }
  }

  public static void removeEasyNPC(EasyNPC<?> easyNPC) {
    log.debug(
        "{} [Remove] EASY NPC entity {}: {}", LOG_PREFIX, easyNPC, easyNPC.getEntity().getUUID());
    npcEntityMap.remove(easyNPC.getEntity().getUUID());

    // Inform all server-side easy NPC entities about the removed easyNPC.
    if (!easyNPC.isClientSide()) {
      for (EasyNPC<?> easyNPCChild : npcEntityMap.values()) {
        if (easyNPCChild != easyNPC) {
          easyNPCChild.handleEasyNPCLeave(easyNPC);
        }
      }
    }
  }

  public static void addLivingEntity(LivingEntity livingEntity) {
    if (log.isTraceEnabled()) {
      log.trace("{} [Add] Living entity {}: {}", LOG_PREFIX, livingEntity, livingEntity.getUUID());
    }

    // Inform all server-side easy NPC entities about the new living entity.
    for (EasyNPC<?> easyNPC : npcEntityMap.values()) {
      easyNPC.handleLivingEntityJoin(livingEntity);
    }
  }

  public static void removeLivingEntity(LivingEntity livingEntity) {
    if (log.isTraceEnabled()) {
      log.trace(
          "{} [Remove] Living entity {}: {}", LOG_PREFIX, livingEntity, livingEntity.getUUID());
    }

    // Inform all server-side easy NPC entities about the leaved living entity.
    for (EasyNPC<?> easyNPC : npcEntityMap.values()) {
      easyNPC.handleLivingEntityLeave(livingEntity);
    }
  }

  public static void addServerPlayer(ServerPlayer serverPlayer) {
    log.debug("{} [Add] Server player {}: {}", LOG_PREFIX, serverPlayer, serverPlayer.getUUID());
    playerMap.put(serverPlayer.getUUID(), serverPlayer);
    playerNameMap.put(serverPlayer.getName().getString(), serverPlayer);

    // Inform all server-side easy NPC entities about the new player.
    for (EasyNPC<?> easyNPC : npcEntityMap.values()) {
      easyNPC.handlePlayerJoin(serverPlayer);
    }
  }

  public static void removeServerPlayer(ServerPlayer serverPlayer) {
    log.debug("{} [Remove] Server player {}: {}", LOG_PREFIX, serverPlayer, serverPlayer.getUUID());
    playerMap.remove(serverPlayer.getUUID());
    playerNameMap.remove(serverPlayer.getName().getString());

    // Inform all server-side easy NPC entities about the leaved player.
    for (EasyNPC<?> easyNPC : npcEntityMap.values()) {
      easyNPC.handlePlayerLeave(serverPlayer);
    }
  }

  public static LivingEntity getLivingEntityByUUID(UUID uuid, ServerLevel serverLevel) {
    if (uuid == null || serverLevel == null) {
      return null;
    }
    Entity entity = serverLevel.getEntity(uuid);
    if (entity instanceof LivingEntity livingEntity) {
      return livingEntity;
    }
    ServerPlayer serverPlayer = getPlayerByUUID(uuid, serverLevel);
    if (serverPlayer != null) {
      return serverPlayer;
    }
    EasyNPC<?> easyNPC = getEasyNPCEntityByUUID(uuid, serverLevel);
    return easyNPC != null ? easyNPC.getLivingEntity() : null;
  }

  public static EasyNPC<?> getEasyNPCEntityByUUID(UUID uuid, ServerPlayer serverPlayer) {
    return getEasyNPCEntityByUUID(uuid, serverPlayer.getLevel());
  }

  public static EasyNPC<?> getEasyNPCEntityByUUID(UUID uuid, ServerLevel serverLevel) {
    if (uuid == null || serverLevel == null) {
      return null;
    }
    Entity entity = serverLevel.getEntity(uuid);
    if (entity instanceof EasyNPC<?> easyNPC) {
      return easyNPC;
    }
    return getEasyNPCEntityByUUID(uuid);
  }

  public static EasyNPC<?> getEasyNPCEntityByUUID(UUID uuid) {
    if (uuid == null) {
      return null;
    }
    return npcEntityMap.getOrDefault(uuid, null);
  }

  public static ServerPlayer getPlayerByUUID(UUID uuid, ServerLevel serverLevel) {
    if (uuid == null || serverLevel == null) {
      return null;
    }
    Player player = serverLevel.getPlayerByUUID(uuid);
    if (player instanceof ServerPlayer serverPlayer) {
      return serverPlayer;
    }
    return playerMap.getOrDefault(uuid, null);
  }

  public static ServerPlayer getPlayerByName(String name) {
    if (name == null || name.isEmpty()) {
      return null;
    }
    return playerNameMap.getOrDefault(name, null);
  }

  public static Stream<String> getUUIDStrings() {
    return npcEntityMap.keySet().stream().map(UUID::toString);
  }

  public static Stream<String> getUUIDStringsByOwner(ServerPlayer serverPlayer) {
    Map<UUID, Entity> npcEntityMapByOwner = getEntityMapByOwner(serverPlayer);
    return npcEntityMapByOwner != null
        ? npcEntityMapByOwner.keySet().stream().map(UUID::toString)
        : Stream.empty();
  }

  public static Map<UUID, Entity> getEntityMapByOwner(ServerPlayer serverPlayer) {
    return serverPlayer != null ? getEntityMapByOwner(serverPlayer.getUUID()) : null;
  }

  public static Map<UUID, Entity> getEntityMapByOwner(UUID ownerUUID) {
    HashMap<UUID, Entity> result = new HashMap<>();
    for (var entry : npcEntityMap.entrySet()) {
      EasyNPC<?> easyNPC = entry.getValue();
      if (easyNPC instanceof OwnerData<?> ownerData && ownerData.isOwner(ownerUUID)) {
        result.put(entry.getKey(), easyNPC.getEntity());
      }
    }
    return result;
  }

  public static boolean hasAccess(UUID uuid, ServerPlayer serverPlayer) {
    if (uuid == null || serverPlayer == null) {
      return false;
    }
    return hasAccess(serverPlayer.getLevel().getEntity(uuid), serverPlayer);
  }

  public static boolean hasAccess(Entity entity, ServerPlayer serverPlayer) {
    // Allow admins and creative mode
    if (serverPlayer.isCreative()) {
      return true;
    }

    // Perform more specific checks
    if (entity instanceof EasyNPC<?> easyNPC && easyNPC instanceof OwnerData<?> ownerData) {
      UUID uuid = ownerData.getOwnerUUID();
      return uuid != null && uuid.equals(serverPlayer.getUUID());
    }

    return false;
  }

  public static void discardEasyNPCEntityByUUID(UUID uuid, ServerLevel serverLevel) {
    EasyNPC<?> easyNPC = getEasyNPCEntityByUUID(uuid, serverLevel);
    if (easyNPC != null && easyNPC.getMob() != null) {
      easyNPC.getMob().discard();
      npcEntityMap.remove(uuid);
    } else {
      log.warn("{} [Discard] Unable to discard EASY NPC entity {}: {}", LOG_PREFIX, easyNPC, uuid);
    }
  }
}
