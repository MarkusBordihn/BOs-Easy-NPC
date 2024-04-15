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

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.data.render.RenderDataSet;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface EasyNPCRenderer<E extends PathfinderMob, M extends EntityModel<E>> {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static boolean renderEntity(
      PathfinderMob entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return false;
    }

    RenderData<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData == null
        || renderData.getRenderData() == null
        || renderData.getRenderData().getRenderType() == RenderType.DEFAULT) {
      return false;
    }

    RenderDataSet renderDataSet = renderData.getRenderData();
    RenderType renderType = renderDataSet.getRenderType();
    EntityType<?> renderEntityType = renderDataSet.getRenderEntityType();

    if (renderType == RenderType.CUSTOM_ENTITY
        && renderEntityType != null
        && !RendererManager.isUnsupportedEntityType(renderEntityType)) {

      // Try to render custom entity or living entity over existing renderer.
      PathfinderMob customEntity = RendererManager.getPathfinderMob(renderEntityType);
      if (customEntity != null) {
        LivingEntityRenderer<?, ?> livingEntityRenderer =
            RendererManager.getLivingEntityRenderer(renderEntityType);
        if (livingEntityRenderer != null) {
          renderCustomLivingEntity(
              entity,
              customEntity,
              (LivingEntityRenderer<LivingEntity, EntityModel<LivingEntity>>) livingEntityRenderer,
              entityYaw,
              partialTicks,
              poseStack,
              buffer,
              packedLight);
          return true;
        }

        EntityRenderer<?> entityRenderer = RendererManager.getEntityRenderer(renderEntityType);
        if (entityRenderer != null) {
          renderCustomEntity(
              entity,
              customEntity,
              (EntityRenderer<Entity>) entityRenderer,
              entityYaw,
              partialTicks,
              poseStack,
              buffer,
              packedLight);
          return true;
        }
      }

      // Register custom entity renderer, if not already registered and supported.
      RendererManager.registerRenderer(renderEntityType, entity.level);
    }

    return false;
  }

  static void renderCustomEntity(
      PathfinderMob entity,
      Entity customEntity,
      EntityRenderer<Entity> entityRenderer,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    RendererManager.copyCustomEntityData(entity, customEntity);
    entityRenderer.render(customEntity, entityYaw, partialTicks, poseStack, buffer, packedLight);
  }

  static void renderCustomLivingEntity(
      PathfinderMob entity,
      LivingEntity customEntity,
      LivingEntityRenderer<LivingEntity, EntityModel<LivingEntity>> livingEntityRenderer,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    RendererManager.copyCustomLivingEntityData(entity, customEntity);
    livingEntityRenderer.render(
        customEntity, entityYaw, partialTicks, poseStack, buffer, packedLight);
  }

  ResourceLocation getTextureByVariant(Enum<?> variant);

  ResourceLocation getDefaultTexture();

  default ResourceLocation getTextureOverlayByVariant(Enum<?> variant) {
    return Constants.BLANK_ENTITY_TEXTURE;
  }

  default ResourceLocation getCustomTexture(SkinData<?> entity) {
    return CustomTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  default ResourceLocation getPlayerTexture(SkinData<?> entity) {
    return PlayerTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  default ResourceLocation getRemoteTexture(SkinData<?> entity) {
    return RemoteTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  default <N extends EasyNPC<E>> ResourceLocation getEntityTexture(N easyNPC) {
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    return switch (skinData.getSkinType()) {
      case NONE -> Constants.BLANK_ENTITY_TEXTURE;
      case CUSTOM -> getCustomTexture(skinData);
      case SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> getRemoteTexture(skinData);
      default -> getTextureByVariant(easyNPC.getEasyNPCVariantData().getVariant());
    };
  }

  default <N extends EasyNPC<E>> ResourceLocation getEntityOverlayTexture(N easyNPC) {
    if (easyNPC.getEasyNPCSkinData().getSkinType() == SkinType.DEFAULT) {
      return getTextureOverlayByVariant(easyNPC.getEasyNPCVariantData().getVariant());
    } else {
      return Constants.BLANK_ENTITY_TEXTURE;
    }
  }

  default <N extends EasyNPC<E>> ResourceLocation getEntityPlayerTexture(N easyNPC) {
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    return switch (easyNPC.getEasyNPCSkinData().getSkinType()) {
      case NONE -> Constants.BLANK_ENTITY_TEXTURE;
      case CUSTOM -> getCustomTexture(skinData);
      case PLAYER_SKIN -> getPlayerTexture(skinData);
      case SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> getRemoteTexture(skinData);
      default -> getTextureByVariant(easyNPC.getEasyNPCVariantData().getVariant());
    };
  }
}
