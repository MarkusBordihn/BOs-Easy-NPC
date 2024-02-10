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
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
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

  private static final ConcurrentHashMap<UUID, LivingEntity> livingEntityMap =
      new ConcurrentHashMap<>();

  private static final ConcurrentHashMap<UUID, ServerPlayer> playerMap = new ConcurrentHashMap<>();

  private LivingEntityManager() {
  }

  public static void addEasyNPC(EasyNPC<?> easyNPC) {
    log.debug(
        "{} [Add] EASY NPC entity {}: {}",
        LOG_PREFIX,
        easyNPC,
        easyNPC.getEasyNPCEntity().getUUID());
    npcEntityMap.put(easyNPC.getEasyNPCEntity().getUUID(), easyNPC);

    // TODO: Inform NPCs about new NPCs
  }

  public static void removeEasyNPC(EasyNPC<?> easyNPC) {
    log.debug(
        "{} [Remove] EASY NPC entity {}: {}",
        LOG_PREFIX,
        easyNPC,
        easyNPC.getEasyNPCEntity().getUUID());
    npcEntityMap.remove(easyNPC.getEasyNPCEntity().getUUID());

    // TODO: Inform NPCs about removed NPCs
  }

  public static void addLivingEntity(LivingEntity livingEntity) {
    if (log.isTraceEnabled()) {
      log.trace("{} [Add] Living entity {}: {}", LOG_PREFIX, livingEntity, livingEntity.getUUID());
    }
    livingEntityMap.put(livingEntity.getUUID(), livingEntity);

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
    livingEntityMap.remove(livingEntity.getUUID());

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
    return easyNPC != null ? easyNPC.getEasyNPCEntity() : null;
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
}
