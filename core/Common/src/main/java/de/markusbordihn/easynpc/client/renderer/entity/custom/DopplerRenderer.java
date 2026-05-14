package de.markusbordihn.easynpc.client.renderer.entity.custom;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.custom.DopplerModel;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.SkullHeadRenderLayer;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.client.renderer.manager.RendererManager;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.skin.variant.DopplerSkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.entity.state.HumanoidRenderState;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.client.renderer.state.CameraRenderState;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DopplerRenderer
    extends HumanoidMobRenderer<
        PathfinderMob, HumanoidRenderState, DopplerModel<HumanoidRenderState>>
    implements EasyNPCEntityRenderer {

  protected static final Identifier DEFAULT_TEXTURE =
      DopplerSkinVariant.DOPPLER.getTextureLocation();
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public DopplerRenderer(
      EntityRendererProvider.Context context, ModelLayerLocation modelLayerLocation) {
    super(context, new DopplerModel<>(context.bakeLayer(modelLayerLocation)), 0.5F);
    this.addLayer(new SkullHeadRenderLayer<>(this));
  }

  private static boolean renderEntity(
      EasyNPC<?> entity,
      EntityModel<?> entityModel,
      HumanoidRenderState renderState,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState) {

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
        EntityTypeManager.getPathfinderMob(renderEntityType, entity.getEntityLevel());
    if (customEntity == null) {
      return false;
    }

    // Get entity type name.
    String entityTypeName = EntityTypeManager.getEntityTypeName(renderEntityType);

    // Render custom entity over living render, if supported.
    LivingEntityRenderer<
            LivingEntity, LivingEntityRenderState, EntityModel<? super LivingEntityRenderState>>
        livingEntityRenderer =
            (LivingEntityRenderer<
                    LivingEntity,
                    LivingEntityRenderState,
                    EntityModel<? super LivingEntityRenderState>>)
                RendererManager.getLivingEntityRenderer(renderEntityType, customEntity);
    if (livingEntityRenderer != null) {
      try {
        // Copy entity data FIRST, so the render state gets the correct rotation
        RendererManager.copyCustomLivingEntityData(
            entity.getPathfinderMob(), customEntity, entityTypeName);

        // Sync body rotation with head rotation for proper mouse following in screens
        if (RendererManager.isScreenRendering()) {
          customEntity.yBodyRot = customEntity.getYHeadRot();
          customEntity.yBodyRotO = customEntity.yHeadRotO;
        }

        // Create render state AFTER copying data
        LivingEntityRenderState livingEntityRenderState =
            livingEntityRenderer.createRenderState(customEntity, 1.0F);
        if (livingEntityRenderState instanceof EasyNPCRenderStateExtension extension) {
          extension.setEasyNpcUUID(easyNPC.getEntityUUID());
        }

        livingEntityRenderer.submit(
            livingEntityRenderState, poseStack, submitNodeCollector, cameraRenderState);
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
    EntityRenderer<Entity, EntityRenderState> entityRenderer =
        (EntityRenderer<Entity, EntityRenderState>)
            RendererManager.getEntityRenderer(renderEntityType, customEntity);
    EntityRenderState entityRenderState = entityRenderer.createRenderState(customEntity, 1.0F);
    if (entityRenderState instanceof EasyNPCRenderStateExtension extension) {
      extension.setEasyNpcUUID(easyNPC.getEntityUUID());
    }
    if (entityRenderer != null) {
      try {
        RendererManager.copyCustomLivingEntityData(
            entity.getPathfinderMob(), customEntity, entityTypeName);
        entityRenderer.submit(entityRenderState, poseStack, submitNodeCollector, cameraRenderState);
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
  public HumanoidRenderState createRenderState() {
    return new HumanoidRenderState();
  }

  @Override
  public Identifier getTextureLocation(HumanoidRenderState renderState) {
    return getTextureFromRenderState(renderState);
  }

  @Override
  public Identifier getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }

  @Override
  public void submit(
      HumanoidRenderState renderState,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState) {
    EasyNPC<?> easyNPC = getEasyNPC(renderState);
    if (renderEntity(
        easyNPC, this.getModel(), renderState, poseStack, submitNodeCollector, cameraRenderState)) {
      return;
    }

    super.submit(renderState, poseStack, submitNodeCollector, cameraRenderState);
  }
}
