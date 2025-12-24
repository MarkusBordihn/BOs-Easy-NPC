package de.markusbordihn.easynpc.client.renderer.entity.custom;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.custom.DopplerModel;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.client.renderer.manager.RendererManager;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.skin.variant.DopplerSkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.PathfinderMob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DopplerRenderer<E extends PathfinderMob>
    extends HumanoidMobRenderer<E, DopplerModel<E>> implements EasyNPCEntityRenderer {

  protected static final ResourceLocation DEFAULT_TEXTURE =
      DopplerSkinVariant.DOPPLER.getTextureLocation();
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public DopplerRenderer(
      EntityRendererProvider.Context context, ModelLayerLocation modelLayerLocation) {
    super(context, new DopplerModel<>(context.bakeLayer(modelLayerLocation)), 0.5F);
  }

  private boolean renderEntity(
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
    RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData == null
        || renderData.getRenderDataEntry() == null
        || renderData.getRenderDataEntry().getRenderType() != RenderType.CUSTOM_ENTITY) {
      return false;
    }

    // Get custom render data.
    EntityType<? extends Entity> renderEntityType =
        renderData.getRenderDataEntry().getRenderEntityType();

    // Get custom entity for render custom .
    PathfinderMob customEntity =
        EntityTypeManager.getPathfinderMob(renderEntityType, entity.level());
    if (customEntity == null) {
      return false;
    }

    // Get entity type name.
    String entityTypeName = EntityTypeManager.getEntityTypeName(renderEntityType);

    // Render custom entity over living render, if supported.
    LivingEntityRenderer<E, DopplerModel<E>> livingEntityRenderer =
        (LivingEntityRenderer<E, DopplerModel<E>>)
            RendererManager.getLivingEntityRenderer(renderEntityType, customEntity);
    if (livingEntityRenderer != null) {
      try {
        RendererManager.copyCustomLivingEntityData(entity, customEntity, entityTypeName);
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
        RendererManager.copyCustomLivingEntityData(entity, customEntity, entityTypeName);
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

  @Override
  public ResourceLocation getTextureLocation(E entity) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      return getEntityTexture(easyNPC);
    }
    return DEFAULT_TEXTURE;
  }

  @Override
  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }

  @Override
  public void render(
      E entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight) {
    if (renderEntity(entity, entityYaw, partialTicks, poseStack, bufferSource, packedLight)) {
      return;
    }

    super.render(entity, entityYaw, partialTicks, poseStack, bufferSource, packedLight);
  }
}
