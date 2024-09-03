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
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.client.renderer.manager.RendererManager;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.data.render.RenderType;
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
import net.minecraft.world.entity.PathfinderMob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface EasyNPCRenderer<E extends PathfinderMob, M extends EntityModel<E>> {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  default boolean renderEntity(
      PathfinderMob entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {

    // We only take care of EasyNPC entities.
    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return false;
    }

    // Get render data.
    RenderData<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData == null
        || renderData.getRenderDataSet() == null
        || renderData.getRenderDataSet().getRenderType() != RenderType.CUSTOM_ENTITY) {
      return false;
    }

    // Get custom render data.
    EntityType<? extends Entity> renderEntityType =
        renderData.getRenderDataSet().getRenderEntityType();

    // Get custom entity for render custom .
    PathfinderMob customEntity = EntityTypeManager.getPathfinderMob(renderEntityType, entity.level);
    if (customEntity == null) {
      return false;
    }

    // Render custom entity over living render, if supported.
    LivingEntityRenderer<E, M> livingEntityRenderer =
        (LivingEntityRenderer<E, M>)
            RendererManager.getLivingEntityRenderer(renderEntityType, customEntity);
    if (livingEntityRenderer != null) {
      try {
        RendererManager.copyCustomLivingEntityData(entity, customEntity);
        livingEntityRenderer.render(
            (E) customEntity, entityYaw, partialTicks, poseStack, buffer, packedLight);
        return true;
      } catch (Exception exception) {
        log.error(
            "Failed to render custom living entity {} ({}):",
            customEntity,
            renderEntityType,
            exception);
        EntityTypeManager.addUnsupportedEntityType(renderEntityType);
        return false;
      }
    }

    // Alternative render custom entity over entity render, if supported.
    EntityRenderer<E> entityRenderer =
        (EntityRenderer<E>) RendererManager.getEntityRenderer(renderEntityType, customEntity);
    if (entityRenderer != null) {
      try {
        RendererManager.copyCustomLivingEntityData(entity, customEntity);
        entityRenderer.render(
            (E) customEntity, entityYaw, partialTicks, poseStack, buffer, packedLight);
        return true;
      } catch (Exception exception) {
        log.error(
            "Failed to render custom entity {} ({}):", customEntity, renderEntityType, exception);
        EntityTypeManager.addUnsupportedEntityType(renderEntityType);
        return false;
      }
    }

    // Give up rendering, if no custom renderer is available.
    return false;
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
