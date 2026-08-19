package de.markusbordihn.easynpc.client.renderer.entity.custom;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.custom.DopplerModel;
import de.markusbordihn.easynpc.client.renderer.OpacitySubmitNodeCollector;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCLivingEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.SpeechBubbleRenderer;
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
import net.minecraft.client.renderer.state.level.CameraRenderState;
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

  private static void applyNPCRotationToImitatedModel(
      HumanoidRenderState npcRenderState, EntityRenderState imitatedModelRenderState) {
    if (imitatedModelRenderState instanceof LivingEntityRenderState livingEntityRenderState) {
      livingEntityRenderState.bodyRot = npcRenderState.bodyRot;
      livingEntityRenderState.yRot = npcRenderState.yRot;
      livingEntityRenderState.xRot = npcRenderState.xRot;
    }
  }

  private static boolean renderEntity(
      EasyNPC<?> entity,
      EntityModel<?> entityModel,
      HumanoidRenderState renderState,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState) {

    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return false;
    }

    RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData == null
        || renderData.getRenderDataEntry() == null
        || renderData.getRenderDataEntry().getRenderType() != RenderType.CUSTOM_ENTITY) {
      return false;
    }

    EntityType<? extends Entity> renderEntityType =
        renderData.getRenderDataEntry().getRenderEntityType();

    PathfinderMob customEntity =
        EntityTypeManager.getPathfinderMob(renderEntityType, entity.getEntityLevel());
    if (customEntity == null) {
      return false;
    }

    String entityTypeName = EntityTypeManager.getEntityTypeName(renderEntityType);
    SubmitNodeCollector entitySubmitNodeCollector =
        OpacitySubmitNodeCollector.wrapIfNeeded(easyNPC, submitNodeCollector);

    LivingEntityRenderer<
            LivingEntity, LivingEntityRenderState, EntityModel<? super LivingEntityRenderState>>
        livingEntityRenderer =
            (LivingEntityRenderer<
                    LivingEntity,
                    LivingEntityRenderState,
                    EntityModel<? super LivingEntityRenderState>>)
                RendererManager.getLivingEntityRenderer(renderEntityType, customEntity);
    if (livingEntityRenderer != null) {
      poseStack.pushPose();
      try {
        // Copy entity data FIRST, so the render state gets the correct rotation
        RendererManager.copyCustomLivingEntityData(
            entity.getPathfinderMob(), customEntity, entityTypeName);

        LivingEntityRenderState livingEntityRenderState =
            livingEntityRenderer.createRenderState(customEntity, 1.0F);
        if (livingEntityRenderState instanceof EasyNPCRenderStateExtension extension) {
          extension.setEasyNpcUUID(easyNPC.getEntityUUID());
        }
        applyNPCRotationToImitatedModel(renderState, livingEntityRenderState);

        EasyNPCLivingEntityRenderer.handleRotation(easyNPC, poseStack);
        EasyNPCLivingEntityRenderer.handleScale(easyNPC, poseStack);

        livingEntityRenderer.submit(
            livingEntityRenderState, poseStack, entitySubmitNodeCollector, cameraRenderState);
        return true;
      } catch (Exception exception) {
        log.error(
            "Failed to render custom living entity {} ({}):",
            customEntity,
            renderEntityType,
            exception);
        EntityTypeManager.addUnsupportedEntityType(renderEntityType);
        return false;
      } finally {
        poseStack.popPose();
      }
    }

    EntityRenderer<Entity, EntityRenderState> entityRenderer =
        (EntityRenderer<Entity, EntityRenderState>)
            RendererManager.getEntityRenderer(renderEntityType, customEntity);
    EntityRenderState entityRenderState = entityRenderer.createRenderState(customEntity, 1.0F);
    if (entityRenderState instanceof EasyNPCRenderStateExtension extension) {
      extension.setEasyNpcUUID(easyNPC.getEntityUUID());
    }
    if (entityRenderer != null) {
      poseStack.pushPose();
      try {
        RendererManager.copyCustomLivingEntityData(
            entity.getPathfinderMob(), customEntity, entityTypeName);
        applyNPCRotationToImitatedModel(renderState, entityRenderState);
        EasyNPCLivingEntityRenderer.handleRotation(easyNPC, poseStack);
        EasyNPCLivingEntityRenderer.handleScale(easyNPC, poseStack);
        entityRenderer.submit(
            entityRenderState, poseStack, entitySubmitNodeCollector, cameraRenderState);
        return true;
      } catch (Exception exception) {
        log.error(
            "Failed to render custom entity {} ({}):", customEntity, renderEntityType, exception);
        EntityTypeManager.addUnsupportedEntityType(renderEntityType);
        return false;
      } finally {
        poseStack.popPose();
      }
    }

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
      this.submitNameDisplay(renderState, poseStack, submitNodeCollector, cameraRenderState);

      // This branch never reaches EntityRenderer#submit, where the speech bubble mixin is attached.
      SpeechBubbleRenderer.submit(renderState, poseStack, submitNodeCollector, cameraRenderState);

      return;
    }

    super.submit(renderState, poseStack, submitNodeCollector, cameraRenderState);
  }
}
