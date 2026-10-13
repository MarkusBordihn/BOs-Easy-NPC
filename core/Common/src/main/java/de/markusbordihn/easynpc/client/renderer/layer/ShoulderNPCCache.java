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

package de.markusbordihn.easynpc.client.renderer.layer;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.screen.EntityScreenRenderer;
import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.data.npc.ShoulderSide;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.entity.EasyNPCEntityTypes;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntitySpawnRequest;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.level.storage.TagValueInput;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ShoulderNPCCache {

  static final float FORWARD_FACING_YAW = 180.0F;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String ENTITY_ID_TAG = "id";
  private static final String ENTITY_UUID_TAG = "UUID";
  private static final long UNUSED_ENTRY_TICKS = 200L;
  private static final Map<UUID, CachedShoulderNPC> cachedShoulderNPCs = new HashMap<>();
  private static final Map<Integer, PlayerShoulderEntities> playerShoulderEntities =
      new ConcurrentHashMap<>();
  private static ClientLevel cachedLevel;

  private ShoulderNPCCache() {}

  public static void setShoulderEntities(
      int playerId, CompoundTag leftShoulderEntity, CompoundTag rightShoulderEntity) {
    if (leftShoulderEntity.isEmpty() && rightShoulderEntity.isEmpty()) {
      playerShoulderEntities.remove(playerId);
    } else {
      playerShoulderEntities.put(
          playerId, new PlayerShoulderEntities(leftShoulderEntity, rightShoulderEntity));
    }
  }

  public static CompoundTag getShoulderEntity(int playerId, ShoulderSide shoulderSide) {
    PlayerShoulderEntities shoulderEntities = playerShoulderEntities.get(playerId);
    if (shoulderEntities == null) {
      return new CompoundTag();
    }

    return shoulderSide == ShoulderSide.LEFT
        ? shoulderEntities.leftShoulderEntity()
        : shoulderEntities.rightShoulderEntity();
  }

  public static void clear() {
    playerShoulderEntities.clear();
    cachedShoulderNPCs.clear();
    cachedLevel = null;
  }

  public static Optional<EasyNPC<?>> get(CompoundTag entityTag) {
    ClientLevel level = Minecraft.getInstance().level;
    if (level == null || !EasyNPCEntityTypes.isEasyNPCEntityTag(entityTag, level)) {
      return Optional.empty();
    }

    Optional<UUID> entityUUID = entityTag.read(ENTITY_UUID_TAG, UUIDUtil.CODEC);
    if (entityUUID.isEmpty()) {
      return Optional.empty();
    }

    if (level != cachedLevel) {
      cachedShoulderNPCs.clear();
      cachedLevel = level;
    }

    long gameTime = level.getGameTime();
    CachedShoulderNPC cachedShoulderNPC = cachedShoulderNPCs.get(entityUUID.get());
    if (cachedShoulderNPC == null || !cachedShoulderNPC.entityTag().equals(entityTag)) {
      removeUnusedEntries(gameTime);
      cachedShoulderNPC =
          new CachedShoulderNPC(entityTag.copy(), createShoulderNPC(entityTag, level), gameTime);
    } else {
      cachedShoulderNPC = cachedShoulderNPC.withLastUsedGameTime(gameTime);
    }
    cachedShoulderNPCs.put(entityUUID.get(), cachedShoulderNPC);
    return cachedShoulderNPC.easyNPC();
  }

  public static boolean isRenderedByEasyModelEntities(EasyNPC<?> easyNPC) {
    if (!CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED
        || !(easyNPC instanceof EasyModelNPC easyModelNPC)) {
      return false;
    }

    RenderDataEntry renderDataEntry = easyModelNPC.getEasyNPCRenderData().getRenderDataEntry();
    return renderDataEntry != null
        && renderDataEntry.getRenderType() == RenderType.EASY_MODEL_ENTITY
        && easyModelNPC.getEasyModelProfileId() != null;
  }

  private static Optional<EasyNPC<?>> createShoulderNPC(CompoundTag entityTag, ClientLevel level) {
    Optional<Entity> entity;
    try {
      entity =
          EntityType.create(
              TagValueInput.create(ProblemReporter.DISCARDING, level.registryAccess(), entityTag),
              level,
              new EntitySpawnRequest(EntitySpawnReason.LOAD, false));
    } catch (RuntimeException e) {
      log.error(
          "Unable to create shoulder NPC from {}", entityTag.getStringOr(ENTITY_ID_TAG, ""), e);
      return Optional.empty();
    }
    if (entity.isEmpty()
        || !(entity.get() instanceof LivingEntity shoulderEntity)
        || !(shoulderEntity instanceof EasyNPC<?> easyNPC)) {
      return Optional.empty();
    }

    EntityScreenRenderer.assignRenderEntityId(shoulderEntity);
    shoulderEntity.setCustomNameVisible(false);
    shoulderEntity.setYBodyRot(FORWARD_FACING_YAW);
    shoulderEntity.yBodyRotO = FORWARD_FACING_YAW;
    shoulderEntity.setYHeadRot(FORWARD_FACING_YAW);
    shoulderEntity.yHeadRotO = FORWARD_FACING_YAW;
    shoulderEntity.setXRot(0.0F);
    shoulderEntity.xRotO = 0.0F;
    return Optional.of(easyNPC);
  }

  private static void removeUnusedEntries(long gameTime) {
    cachedShoulderNPCs
        .values()
        .removeIf(
            cachedShoulderNPC ->
                gameTime - cachedShoulderNPC.lastUsedGameTime() > UNUSED_ENTRY_TICKS);
  }

  private record PlayerShoulderEntities(
      CompoundTag leftShoulderEntity, CompoundTag rightShoulderEntity) {}

  private record CachedShoulderNPC(
      CompoundTag entityTag, Optional<EasyNPC<?>> easyNPC, long lastUsedGameTime) {

    CachedShoulderNPC withLastUsedGameTime(long lastUsedGameTime) {
      return new CachedShoulderNPC(this.entityTag, this.easyNPC, lastUsedGameTime);
    }
  }
}
