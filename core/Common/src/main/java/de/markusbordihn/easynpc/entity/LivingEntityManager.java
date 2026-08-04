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
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LivingEntityManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String LOG_PREFIX = "[Living Entity Manager]";

  private static final ConcurrentHashMap<String, ServerPlayer> playerNameMap =
      new ConcurrentHashMap<>();
  private static final ConcurrentHashMap<UUID, EasyNPC<?>> npcEntityMapServer =
      new ConcurrentHashMap<>();
  private static final ConcurrentHashMap<UUID, EasyNPC<?>> npcEntityMapClient =
      new ConcurrentHashMap<>();
  private static final ConcurrentHashMap<UUID, Set<EasyNPC<?>>> presetMap =
      new ConcurrentHashMap<>();

  private static final ConcurrentHashMap<ResourceKey<Level>, Set<EasyNPC<?>>> entityEventListeners =
      new ConcurrentHashMap<>();
  private static final ConcurrentHashMap<ResourceKey<Level>, Set<EasyNPC<?>>> playerEventListeners =
      new ConcurrentHashMap<>();

  private static final ConcurrentHashMap<UUID, ServerPlayer> playerMap = new ConcurrentHashMap<>();
  private static final ConcurrentHashMap<String, PresetCountCache> presetCountCache =
      new ConcurrentHashMap<>();
  private static final int PRESET_COUNT_CACHE_TTL = 10;

  private LivingEntityManager() {}

  public static void addEasyNPC(EasyNPC<?> easyNPC) {
    UUID uuid = easyNPC.getEntityUUID();
    log.debug("{} [Add] EASY NPC entity {}: {}", LOG_PREFIX, easyNPC, uuid);

    // Client-side instances stay in a separate registry, out of the preset map and broadcasts.
    if (easyNPC.isClientSideInstance()) {
      npcEntityMapClient.put(uuid, easyNPC);
      return;
    }

    npcEntityMapServer.put(uuid, easyNPC);

    PresetDataCapable<?> presetData = easyNPC.getEasyNPCPresetData();
    if (presetData != null && presetData.hasPresetUUID()) {
      presetMap
          .computeIfAbsent(presetData.getPresetUUID(), k -> ConcurrentHashMap.newKeySet())
          .add(easyNPC);
    }

    updateObjectiveEventInterest(easyNPC);

    for (Set<EasyNPC<?>> listeners : entityEventListeners.values()) {
      for (EasyNPC<?> easyNPCChild : listeners) {
        if (easyNPCChild != easyNPC) {
          easyNPCChild.handleEasyNPCJoinEvent(easyNPC);
        }
      }
    }
  }

  public static void removeEasyNPC(EasyNPC<?> easyNPC) {
    UUID uuid = easyNPC.getEntityUUID();
    log.debug("{} [Remove] EASY NPC entity {}: {}", LOG_PREFIX, easyNPC, uuid);

    // Instance-bound removal so a stale instance never evicts a live one of the other side.
    if (easyNPC.isClientSideInstance()) {
      npcEntityMapClient.remove(uuid, easyNPC);
      return;
    }

    npcEntityMapServer.remove(uuid, easyNPC);
    clearObjectiveEventInterest(easyNPC);

    PresetDataCapable<?> presetData = easyNPC.getEasyNPCPresetData();
    if (presetData != null && presetData.hasPresetUUID()) {
      UUID presetUUID = presetData.getPresetUUID();
      presetMap.computeIfPresent(
          presetUUID,
          (k, set) -> {
            set.remove(easyNPC);
            return set.isEmpty() ? null : set;
          });
    }

    for (Set<EasyNPC<?>> listeners : entityEventListeners.values()) {
      for (EasyNPC<?> easyNPCChild : listeners) {
        if (easyNPCChild != easyNPC) {
          easyNPCChild.handleEasyNPCLeaveEvent(easyNPC);
        }
      }
    }
  }

  public static void addLivingEntity(LivingEntity livingEntity) {
    if (log.isTraceEnabled()) {
      log.trace("{} [Add] Living entity {}: {}", LOG_PREFIX, livingEntity, livingEntity.getUUID());
    }

    Set<EasyNPC<?>> listeners = entityEventListeners.get(livingEntity.level().dimension());
    if (listeners != null) {
      for (EasyNPC<?> easyNPC : listeners) {
        easyNPC.handleLivingEntityJoinEvent(livingEntity);
      }
    }
  }

  public static void removeLivingEntity(LivingEntity livingEntity) {
    if (log.isTraceEnabled()) {
      log.trace(
          "{} [Remove] Living entity {}: {}", LOG_PREFIX, livingEntity, livingEntity.getUUID());
    }

    Set<EasyNPC<?>> listeners = entityEventListeners.get(livingEntity.level().dimension());
    if (listeners != null) {
      for (EasyNPC<?> easyNPC : listeners) {
        easyNPC.handleLivingEntityLeaveEvent(livingEntity);
      }
    }
  }

  public static void addServerPlayer(ServerPlayer serverPlayer) {
    log.debug("{} [Add] Server player {}: {}", LOG_PREFIX, serverPlayer, serverPlayer.getUUID());
    playerMap.put(serverPlayer.getUUID(), serverPlayer);
    playerNameMap.put(serverPlayer.getName().getString(), serverPlayer);

    for (Set<EasyNPC<?>> listeners : playerEventListeners.values()) {
      for (EasyNPC<?> easyNPC : listeners) {
        easyNPC.handlePlayerJoinEvent(serverPlayer);
      }
    }
  }

  public static void removeServerPlayer(ServerPlayer serverPlayer) {
    log.debug("{} [Remove] Server player {}: {}", LOG_PREFIX, serverPlayer, serverPlayer.getUUID());
    playerMap.remove(serverPlayer.getUUID());
    playerNameMap.remove(serverPlayer.getName().getString());

    for (Set<EasyNPC<?>> listeners : playerEventListeners.values()) {
      for (EasyNPC<?> easyNPC : listeners) {
        easyNPC.handlePlayerLeaveEvent(serverPlayer);
      }
    }
  }

  public static void updateObjectiveEventInterest(EasyNPC<?> easyNPC) {
    if (easyNPC == null || easyNPC.isClientSideInstance()) {
      return;
    }
    ResourceKey<Level> dimension = getDimension(easyNPC);
    updateInterest(entityEventListeners, dimension, easyNPC, needsEntityEvents(easyNPC));
    updateInterest(playerEventListeners, dimension, easyNPC, needsPlayerEvents(easyNPC));
  }

  private static void clearObjectiveEventInterest(EasyNPC<?> easyNPC) {
    removeFromListeners(entityEventListeners, easyNPC);
    removeFromListeners(playerEventListeners, easyNPC);
  }

  private static void updateInterest(
      ConcurrentHashMap<ResourceKey<Level>, Set<EasyNPC<?>>> listeners,
      ResourceKey<Level> dimension,
      EasyNPC<?> easyNPC,
      boolean interested) {
    // Drop any stale membership (e.g. a previous dimension) before re-adding.
    removeFromListeners(listeners, easyNPC);
    if (interested && dimension != null) {
      listeners.computeIfAbsent(dimension, key -> ConcurrentHashMap.newKeySet()).add(easyNPC);
    }
  }

  private static void removeFromListeners(
      ConcurrentHashMap<ResourceKey<Level>, Set<EasyNPC<?>>> listeners, EasyNPC<?> easyNPC) {
    for (Set<EasyNPC<?>> set : listeners.values()) {
      set.remove(easyNPC);
    }
  }

  private static ResourceKey<Level> getDimension(EasyNPC<?> easyNPC) {
    Level level = easyNPC.getEntityLevel();
    return level != null ? level.dimension() : null;
  }

  private static boolean needsEntityEvents(EasyNPC<?> easyNPC) {
    return easyNPC instanceof ObjectiveDataCapable<?> objectiveData
        && objectiveData.hasEntityTargetObjectives();
  }

  private static boolean needsPlayerEvents(EasyNPC<?> easyNPC) {
    return easyNPC instanceof ObjectiveDataCapable<?> objectiveData
        && (objectiveData.hasOwnerTargetObjectives() || objectiveData.hasPlayerTargetObjectives());
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
    EasyNPC<?> easyNPC = getServerEasyNPCEntityByUUID(uuid, serverLevel);
    return easyNPC != null ? easyNPC.getLivingEntity() : null;
  }

  public static EasyNPC<?> getServerEasyNPCEntityByUUID(UUID uuid, ServerPlayer serverPlayer) {
    return serverPlayer != null
        ? getServerEasyNPCEntityByUUID(uuid, serverPlayer.serverLevel())
        : null;
  }

  public static EasyNPC<?> getServerEasyNPCEntityByUUID(UUID uuid, ServerLevel serverLevel) {
    if (uuid == null || serverLevel == null) {
      return null;
    }
    Entity entity = serverLevel.getEntity(uuid);
    if (entity instanceof EasyNPC<?> easyNPC) {
      return easyNPC;
    }
    return getServerEasyNPCEntityByUUID(uuid);
  }

  public static EasyNPC<?> getServerEasyNPCEntityByUUID(UUID uuid) {
    if (uuid == null) {
      return null;
    }
    return npcEntityMapServer.getOrDefault(uuid, null);
  }

  public static EasyNPC<?> getClientEasyNPCEntityByUUID(UUID uuid) {
    if (uuid == null) {
      return null;
    }
    return npcEntityMapClient.getOrDefault(uuid, null);
  }

  public static Stream<EasyNPC<?>> getServerEasyNPCEntities() {
    return npcEntityMapServer.values().stream();
  }

  public static Stream<EasyNPC<?>> getClientEasyNPCEntities() {
    return npcEntityMapClient.values().stream();
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
    return npcEntityMapServer.keySet().stream().map(UUID::toString);
  }

  public static Map<UUID, Entity> getEntityMapByOwner(ServerPlayer serverPlayer) {
    return serverPlayer != null ? getEntityMapByOwner(serverPlayer.getUUID()) : null;
  }

  public static Map<UUID, Entity> getEntityMapByOwner(UUID ownerUUID) {
    HashMap<UUID, Entity> result = new HashMap<>();
    for (var entry : npcEntityMapServer.entrySet()) {
      EasyNPC<?> easyNPC = entry.getValue();
      if (easyNPC instanceof OwnerDataCapable<?> ownerData && ownerData.isNPCOwner(ownerUUID)) {
        result.put(entry.getKey(), easyNPC.getEntity());
      }
    }
    return result;
  }

  public static int getEntityCountByPresetUUID(UUID presetUUID) {
    return presetMap.getOrDefault(presetUUID, ConcurrentHashMap.newKeySet()).size();
  }

  public static int getEntityCountByPresetUUID(UUID presetUUID, ServerLevel serverLevel) {
    if (presetUUID == null || serverLevel == null) {
      return 0;
    }

    ResourceKey<Level> dimension = serverLevel.dimension();
    String cacheKey = presetUUID + "|" + dimension.location();
    long currentTick = serverLevel.getGameTime();
    PresetCountCache cache = presetCountCache.get(cacheKey);
    if (cache != null && (currentTick - cache.tickTime) < PRESET_COUNT_CACHE_TTL) {
      return cache.count;
    }

    int count = getEntityCountByPresetUUIDInDimension(presetUUID, dimension);
    presetCountCache.put(cacheKey, new PresetCountCache(count, currentTick));
    return count;
  }

  private static int getEntityCountByPresetUUIDInDimension(
      UUID presetUUID, ResourceKey<Level> dimension) {
    Set<EasyNPC<?>> presetEntities = presetMap.get(presetUUID);
    if (presetEntities == null) {
      return 0;
    }
    int count = 0;
    for (EasyNPC<?> easyNPC : presetEntities) {
      Level level = easyNPC.getEntityLevel();
      if (level != null && level.dimension().equals(dimension)) {
        count++;
      }
    }
    return count;
  }

  public static boolean hasAccess(UUID uuid, ServerPlayer serverPlayer) {
    if (uuid == null || serverPlayer == null) {
      return false;
    }
    return hasAccess(serverPlayer.serverLevel().getEntity(uuid), serverPlayer);
  }

  public static boolean hasAccess(Entity entity, ServerPlayer serverPlayer) {
    if (serverPlayer.isCreative()) {
      return true;
    }

    if (entity instanceof EasyNPC<?> easyNPC && easyNPC instanceof OwnerDataCapable<?> ownerData) {
      UUID uuid = ownerData.getOwnerUUID();
      return uuid != null && uuid.equals(serverPlayer.getUUID());
    }

    return false;
  }

  public static void discardEasyNPCEntityByUUID(UUID uuid, ServerLevel serverLevel) {
    EasyNPC<?> easyNPC = getServerEasyNPCEntityByUUID(uuid, serverLevel);
    if (easyNPC != null && easyNPC.getMob() != null) {
      easyNPC.getMob().discard();
      npcEntityMapServer.remove(uuid);
    } else {
      log.warn("{} [Discard] Unable to discard EASY NPC entity {}: {}", LOG_PREFIX, easyNPC, uuid);
    }
  }

  private record PresetCountCache(int count, long tickTime) {}
}
