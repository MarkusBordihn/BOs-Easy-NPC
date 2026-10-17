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
import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesLoader;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.entity.EasyNPCEntityTypes;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ShoulderNPCCache {

  static final float FORWARD_FACING_YAW = 180.0F;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String ENTITY_ID_TAG = "id";
  private static final String ENTITY_UUID_TAG = "UUID";
  private static final long UNUSED_ENTRY_TICKS = 200L;
  private static final Map<UUID, CachedShoulderNPC> cachedShoulderNPCs = new HashMap<>();
  private static ClientLevel cachedLevel;

  private ShoulderNPCCache() {}

  public static Optional<EasyNPC<?>> get(CompoundTag entityTag) {
    ClientLevel level = Minecraft.getInstance().level;
    if (level == null
        || !EasyNPCEntityTypes.isEasyNPCEntityTag(entityTag, level)
        || !entityTag.hasUUID(ENTITY_UUID_TAG)) {
      return Optional.empty();
    }

    if (level != cachedLevel) {
      cachedShoulderNPCs.clear();
      cachedLevel = level;
    }

    UUID entityUUID = entityTag.getUUID(ENTITY_UUID_TAG);
    long gameTime = level.getGameTime();
    CachedShoulderNPC cachedShoulderNPC = cachedShoulderNPCs.get(entityUUID);
    if (cachedShoulderNPC == null || !cachedShoulderNPC.entityTag().equals(entityTag)) {
      removeUnusedEntries(gameTime);
      cachedShoulderNPC =
          new CachedShoulderNPC(entityTag.copy(), createShoulderNPC(entityTag, level), gameTime);
      if (CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED) {
        registerEasyModelShoulderResolver(entityTag);
      }
    } else {
      cachedShoulderNPC = cachedShoulderNPC.withLastUsedGameTime(gameTime);
    }
    cachedShoulderNPCs.put(entityUUID, cachedShoulderNPC);
    return cachedShoulderNPC.easyNPC();
  }

  // EasyModelShoulderModels.resolve matches the exact entity type, addon types are unknown before.
  private static void registerEasyModelShoulderResolver(CompoundTag entityTag) {
    ResourceLocation entityTypeId = ResourceLocation.tryParse(entityTag.getString(ENTITY_ID_TAG));
    if (entityTypeId != null) {
      EasyModelEntitiesLoader.registerShoulderModelResolver(entityTypeId);
    }
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
      entity = EntityType.create(entityTag, level);
    } catch (RuntimeException e) {
      log.error("Unable to create shoulder NPC from {}", entityTag.getString(ENTITY_ID_TAG), e);
      return Optional.empty();
    }
    if (entity.isEmpty()
        || !(entity.get() instanceof LivingEntity shoulderEntity)
        || !(shoulderEntity instanceof EasyNPC<?> easyNPC)) {
      return Optional.empty();
    }

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

  private record CachedShoulderNPC(
      CompoundTag entityTag, Optional<EasyNPC<?>> easyNPC, long lastUsedGameTime) {

    CachedShoulderNPC withLastUsedGameTime(long lastUsedGameTime) {
      return new CachedShoulderNPC(this.entityTag, this.easyNPC, lastUsedGameTime);
    }
  }
}
