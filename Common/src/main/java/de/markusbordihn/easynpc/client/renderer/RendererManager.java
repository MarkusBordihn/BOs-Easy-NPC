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

package de.markusbordihn.easynpc.client.renderer;

import de.markusbordihn.easynpc.Constants;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RendererManager {

  private static final String LOG_PREFIX = "[RendererManager]";

  private static final Set<EntityType<?>> unsupportedEntityTypes = new HashSet<>();
  private static final Map<EntityType<?>, PathfinderMob> pathfinderMobMap = new HashMap<>();
  private static final Map<EntityType<?>, EntityRenderer<?>> entityRendererMap = new HashMap<>();
  private static final Map<EntityType<?>, LivingEntityRenderer<?, ?>> livingEntityRendererMap =
      new HashMap<>();
  private static Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private RendererManager() {}

  public static void setPathfinderMob(EntityType<?> entityType, PathfinderMob pathfinderMob) {
    pathfinderMobMap.put(entityType, pathfinderMob);
  }

  public static void setEntityRenderer(EntityType<?> entityType, EntityRenderer<?> entityRenderer) {
    entityRendererMap.put(entityType, entityRenderer);
  }

  public static void setLivingEntityRenderer(
      EntityType<?> entityType, LivingEntityRenderer<?, ?> livingEntityRenderer) {
    livingEntityRendererMap.put(entityType, livingEntityRenderer);
  }

  public static PathfinderMob getPathfinderMob(EntityType<?> entityType) {
    return pathfinderMobMap.get(entityType);
  }

  public static EntityRenderer<?> getEntityRenderer(EntityType<?> entityType) {
    return entityRendererMap.get(entityType);
  }

  public static LivingEntityRenderer<?, ?> getLivingEntityRenderer(EntityType<?> entityType) {
    return livingEntityRendererMap.get(entityType);
  }

  public static boolean isSupportedEntityType(EntityType<?> entityType) {
    return !unsupportedEntityTypes.contains(entityType);
  }

  public static boolean isUnsupportedEntityType(EntityType<?> entityType) {
    return unsupportedEntityTypes.contains(entityType);
  }

  public static boolean hasPathfinderMob(EntityType<?> entityType) {
    return pathfinderMobMap.containsKey(entityType);
  }

  public static boolean hasEntityRenderer(EntityType<?> entityType) {
    return entityRendererMap.containsKey(entityType);
  }

  public static boolean hasLivingEntityRenderer(EntityType<?> entityType) {
    return livingEntityRendererMap.containsKey(entityType);
  }

  public static PathfinderMob registerPathfinderMob(EntityType<?> entityType, Level level) {
    if (entityType == null) {
      return null;
    }
    if (pathfinderMobMap.containsKey(entityType)) {
      log.warn("{} PathfinderMob for {} already registered", LOG_PREFIX, entityType);
      return pathfinderMobMap.get(entityType);
    }
    if (unsupportedEntityTypes.contains(entityType)) {
      log.error("{} Entity type {} is unsupported!", LOG_PREFIX, entityType);
      return null;
    }

    // Verify if entity type is supported and created entity is a PathfinderMob.
    Entity entity = entityType.create(level);
    if (entity instanceof PathfinderMob pathfinderMob) {
      log.debug("{} Registering PathfinderMob {} for {}", LOG_PREFIX, pathfinderMob, entityType);

      // For better performance we disable AI, sound and physics for the entity.
      pathfinderMob.setNoAi(true);
      pathfinderMob.setSilent(true);
      pathfinderMob.noPhysics = true;
      setPathfinderMob(entityType, pathfinderMob);
      return pathfinderMob;
    } else {
      log.error(
          "{} Invalid Entity type {} is not extending PathfinderMob!", LOG_PREFIX, entityType);
      unsupportedEntityTypes.add(entityType);
    }
    return null;
  }

  public static void registerRenderer(EntityType<?> entityType, Level level) {
    if (entityType == null) {
      return;
    }
    if (pathfinderMobMap.containsKey(entityType)) {
      log.warn("{} Renderer for {} already registered", LOG_PREFIX, entityType);
      return;
    }
    if (unsupportedEntityTypes.contains(entityType)) {
      log.error("{} Entity type {} is unsupported!", LOG_PREFIX, entityType);
      return;
    }

    // Verify if entity type is supported.
    PathfinderMob pathfinderMob = registerPathfinderMob(entityType, level);
    if (pathfinderMob == null) {
      log.error("{} PathfinderMob for {} is not available!", LOG_PREFIX, entityType);
      return;
    }

    // Verify if entity renderer is available.
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    EntityRenderer<?> entityRenderer = entityRenderDispatcher.getRenderer(pathfinderMob);
    if (entityRenderer instanceof LivingEntityRenderer<?, ?> livingEntityRenderer) {
      log.debug(
          "{} Registering living entity renderer {} for {}",
          LOG_PREFIX,
          livingEntityRenderer,
          entityType);
      pathfinderMobMap.put(entityType, pathfinderMob);
      entityRendererMap.put(entityType, entityRenderer);
      livingEntityRendererMap.put(entityType, livingEntityRenderer);
    } else {
      log.debug("{} Registering entity renderer {} for {}", LOG_PREFIX, entityRenderer, entityType);
      pathfinderMobMap.put(entityType, pathfinderMob);
      entityRendererMap.put(entityType, entityRenderer);
    }
  }

  public static void copyCustomEntityData(PathfinderMob sourceEntity, Entity targetEntity) {
    if (sourceEntity == null || targetEntity == null || sourceEntity == targetEntity) {
      return;
    }
    // Synchronize entity tick count.
    targetEntity.tickCount = sourceEntity.tickCount;

    // Adjust entity position and speed.
    targetEntity.setYRot(sourceEntity.getYRot());
    targetEntity.yRotO = sourceEntity.yRotO;

    targetEntity.setXRot(sourceEntity.getXRot());
    targetEntity.xRotO = sourceEntity.xRotO;

    targetEntity.setYHeadRot(sourceEntity.getYHeadRot());

    targetEntity.setYBodyRot(sourceEntity.yBodyRot);

    // Additional entity data.
    targetEntity.setOnGround(sourceEntity.onGround());
    targetEntity.setDeltaMovement(sourceEntity.getDeltaMovement());
  }

  public static void copyCustomLivingEntityData(
      PathfinderMob sourceEntity, LivingEntity targetEntity) {
    if (sourceEntity == null || targetEntity == null || sourceEntity == targetEntity) {
      return;
    }
    // Adjust basic entity data.
    copyCustomEntityData(sourceEntity, targetEntity);

    // Adjust entity position and speed.
    targetEntity.yHeadRotO = sourceEntity.yHeadRotO;
    targetEntity.yBodyRotO = sourceEntity.yBodyRotO;

    // Adjust animation position and speed.
    targetEntity.attackAnim = sourceEntity.attackAnim;
    targetEntity.oAttackAnim = sourceEntity.oAttackAnim;
  }
}
