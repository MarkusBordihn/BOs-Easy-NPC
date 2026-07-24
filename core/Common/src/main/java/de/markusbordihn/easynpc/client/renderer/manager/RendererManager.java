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

package de.markusbordihn.easynpc.client.renderer.manager;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import java.util.HashMap;
import java.util.Map;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RendererManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Renderer Manager]";

  private static final Map<EntityType<? extends Entity>, EntityRenderer<? extends Entity>>
      entityRendererMap = new HashMap<>();
  private static final Map<
          EntityType<? extends Entity>,
          LivingEntityRenderer<
              ? extends LivingEntity, ? extends EntityModel<? extends LivingEntity>>>
      livingEntityRendererMap = new HashMap<>();

  private RendererManager() {}

  public static LivingEntityRenderer<
          ? extends LivingEntity, ? extends EntityModel<? extends LivingEntity>>
      getLivingEntityRenderer(
          EntityType<? extends Entity> entityType, PathfinderMob pathfinderMob) {
    if (entityType == null || pathfinderMob == null) {
      return null;
    }

    // Check if entity renderer is already available.
    LivingEntityRenderer<? extends LivingEntity, ? extends EntityModel<? extends LivingEntity>>
        livingEntityRenderer = livingEntityRendererMap.get(entityType);
    if (livingEntityRenderer != null) {
      return livingEntityRenderer;
    }

    // Try to register entity renderer, if not available.
    registerLivingEntityRenderer(entityType, pathfinderMob);

    return livingEntityRendererMap.get(entityType);
  }

  public static EntityRenderer<? extends Entity> getEntityRenderer(
      EntityType<?> entityType, PathfinderMob pathfinderMob) {
    if (entityType == null || pathfinderMob == null) {
      return null;
    }

    // Check if entity renderer is already available.
    EntityRenderer<? extends Entity> entityRenderer = entityRendererMap.get(entityType);
    if (entityRenderer != null) {
      return entityRenderer;
    }

    // Try to register entity renderer, if not available.
    registerEntityRenderer(entityType, pathfinderMob);

    return entityRendererMap.get(entityType);
  }

  public static LivingEntityRenderer<
          ? extends LivingEntity, ? extends EntityModel<? extends LivingEntity>>
      registerLivingEntityRenderer(
          EntityType<? extends Entity> entityType, PathfinderMob pathfinderMob) {
    if (entityType == null || pathfinderMob == null) {
      return null;
    }

    // Check if entity renderer is already available.
    if (livingEntityRendererMap.containsKey(entityType)) {
      return livingEntityRendererMap.get(entityType);
    }

    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    EntityRenderer<? extends Entity> entityRenderer =
        entityRenderDispatcher.getRenderer(pathfinderMob);
    if (entityRenderer
        instanceof
        LivingEntityRenderer<? extends LivingEntity, ? extends EntityModel<? extends Entity>>
            livingEntityRenderer) {
      log.debug(
          "{} Registering living entity renderer {} for {}",
          LOG_PREFIX,
          livingEntityRenderer,
          entityType);
      livingEntityRendererMap.put(entityType, livingEntityRenderer);
      return livingEntityRenderer;
    } else if (!entityRendererMap.containsKey(entityType)) {
      log.debug("{} Registering entity renderer {} for {}", LOG_PREFIX, entityRenderer, entityType);
      entityRendererMap.put(entityType, entityRenderer);
    }

    return null;
  }

  public static EntityRenderer<? extends Entity> registerEntityRenderer(
      EntityType<? extends Entity> entityType, PathfinderMob pathfinderMob) {
    if (entityType == null || pathfinderMob == null) {
      return null;
    }

    // Check if entity renderer is already available.
    if (entityRendererMap.containsKey(entityType)) {
      return entityRendererMap.get(entityType);
    }

    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    EntityRenderer<? extends Entity> entityRenderer =
        entityRenderDispatcher.getRenderer(pathfinderMob);

    log.debug("{} Registering entity renderer {} for {}", LOG_PREFIX, entityRenderer, entityType);
    entityRendererMap.put(entityType, entityRenderer);
    return entityRenderer;
  }

  public static void copyCustomEntityData(
      PathfinderMob sourceEntity, Entity targetEntity, String entityTypeName) {
    if (sourceEntity == null
        || targetEntity == null
        || entityTypeName == null
        || sourceEntity == targetEntity) {
      return;
    }

    // Synchronize entity tick count.
    targetEntity.tickCount = sourceEntity.tickCount;

    // Adjust entity rotation.
    targetEntity.setYRot(sourceEntity.getYRot());
    targetEntity.yRotO = sourceEntity.yRotO;

    targetEntity.setXRot(sourceEntity.getXRot());
    targetEntity.xRotO = sourceEntity.xRotO;

    targetEntity.setYHeadRot(sourceEntity.getYHeadRot());

    targetEntity.setYBodyRot(sourceEntity.yBodyRot);

    // Sync former position to allow proper interpolation and animation.
    targetEntity.xo = sourceEntity.xo;
    targetEntity.yo = sourceEntity.yo;
    targetEntity.zo = sourceEntity.zo;

    // Sync entity position (to allow proper spawning and de-spawning).
    targetEntity.setPos(sourceEntity.getX(), sourceEntity.getY(), sourceEntity.getZ());

    // Additional entity data.
    targetEntity.setOnGround(sourceEntity.onGround());
    targetEntity.setDeltaMovement(sourceEntity.getDeltaMovement());

    // Custom name support, needed for name based textures like "jeb_" or "Toast".
    if (sourceEntity.hasCustomName()) {
      targetEntity.setCustomName(sourceEntity.getCustomName());
    } else if (targetEntity.hasCustomName()) {
      targetEntity.setCustomName(null);
    }
    targetEntity.setCustomNameVisible(false);

    // Sync entity pose, if available.
    if (sourceEntity.getPose() != targetEntity.getPose()) {
      targetEntity.setPose(sourceEntity.getPose());
    }
  }

  public static void copyCustomLivingEntityData(
      PathfinderMob sourceEntity, LivingEntity targetEntity, String entityTypeName) {
    if (sourceEntity == null
        || targetEntity == null
        || entityTypeName == null
        || sourceEntity == targetEntity) {
      return;
    }

    // Adjust basic entity data.
    copyCustomEntityData(sourceEntity, targetEntity, entityTypeName);

    // Adjust entity position and speed.
    targetEntity.yHeadRotO = sourceEntity.yHeadRotO;
    targetEntity.yBodyRotO = sourceEntity.yBodyRotO;

    // Adjust animation position and speed.
    targetEntity.attackAnim = sourceEntity.attackAnim;
    targetEntity.oAttackAnim = sourceEntity.oAttackAnim;
    targetEntity.walkDist = sourceEntity.walkDist;
    targetEntity.walkDistO = sourceEntity.walkDistO;

    // Hand item support.
    targetEntity.setItemInHand(InteractionHand.MAIN_HAND, sourceEntity.getMainHandItem());
    targetEntity.setItemInHand(InteractionHand.OFF_HAND, sourceEntity.getOffhandItem());
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  public static void renderLivingEntity(
      PathfinderMob sourceEntity,
      LivingEntity targetEntity,
      String entityTypeName,
      LivingEntityRenderer<?, ?> livingEntityRenderer,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    copyCustomLivingEntityData(sourceEntity, targetEntity, entityTypeName);
    ((LivingEntityRenderer) livingEntityRenderer)
        .render(targetEntity, entityYaw, partialTicks, poseStack, buffer, packedLight);
  }
}
