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

package de.markusbordihn.easynpc.client.renderer.entity.easymodelentities;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easymodelentities.api.EasyModelReloadEvents;
import de.markusbordihn.easymodelentities.api.client.EasyModelEntitiesClientApi;
import de.markusbordihn.easymodelentities.api.client.EasyModelPartAnimator;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelEntityRenderOptions;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelPartTransform;
import de.markusbordihn.easymodelentities.data.model.bake.ModelBounds;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.custom.DopplerModel;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.variant.DopplerSkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import de.markusbordihn.easynpc.mixin.renderer.MobRendererInvoker;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyModelNPCRenderer<E extends PathfinderMob>
    extends HumanoidMobRenderer<E, DopplerModel<E>> implements EasyNPCEntityRenderer {

  protected static final ResourceLocation DEFAULT_TEXTURE =
      DopplerSkinVariant.DOPPLER.getTextureLocation();
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<ResourceLocation, Boolean> invalidProfileCache =
      new ConcurrentHashMap<>();
  private static final Map<ResourceLocation, Float> guiPreviewScaleCache =
      new ConcurrentHashMap<>();
  private static boolean reloadListenerRegistered = false;

  public EasyModelNPCRenderer(
      EntityRendererProvider.Context context, ModelLayerLocation modelLayerLocation) {
    super(context, new DopplerModel<>(context.bakeLayer(modelLayerLocation)), 0.5F);
    registerReloadListener();
  }

  private static synchronized void registerReloadListener() {
    if (reloadListenerRegistered) {
      return;
    }
    reloadListenerRegistered = true;
    EasyModelReloadEvents.onProfileReload(EasyModelNPCRenderer::clearCaches);
    EasyModelReloadEvents.onRenderProfileReload(EasyModelNPCRenderer::clearCaches);
  }

  private static void clearCaches() {
    invalidProfileCache.clear();
    guiPreviewScaleCache.clear();
  }

  private static EasyModelPartAnimator createPartAnimator(EasyModelNPC easyModelNPC) {
    return context -> {
      ModelPartType modelPartType = EasyModelEntitiesManager.getModelPartType(context.partName());
      if (modelPartType == ModelPartType.UNKNOWN) {
        return EasyModelPartTransform.NONE;
      }
      CustomRotation rotation = easyModelNPC.getModelPartRotation(modelPartType);
      CustomPosition position = easyModelNPC.getModelPartPosition(modelPartType);
      CustomScale scale = easyModelNPC.getModelPartScale(modelPartType);
      boolean visible = easyModelNPC.getModelPartVisibility(modelPartType);
      return new EasyModelPartTransform(rotation.x(), rotation.y(), rotation.z())
          .withOffset(position.x(), position.y(), position.z())
          .withScale(scale.x(), scale.y(), scale.z())
          .withVisible(visible);
    };
  }

  private static float getGuiPreviewScale(
      ResourceLocation profileId, float displayedSubject, float fallbackHeight) {
    return guiPreviewScaleCache.computeIfAbsent(
        profileId,
        id -> {
          float subject = displayedSubject > 0f ? displayedSubject : fallbackHeight;
          return EntityTypeManager.calculateGuiPreviewScaleFactor(subject);
        });
  }

  private boolean renderEasyModel(
      E entity,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {

    if (!(entity instanceof EasyModelNPC easyModelNPC)) {
      return false;
    }

    RenderDataCapable<?> renderData = easyModelNPC.getEasyNPCRenderData();
    if (renderData == null
        || renderData.getRenderDataEntry() == null
        || renderData.getRenderDataEntry().getRenderType() != RenderType.EASY_MODEL_ENTITY) {
      return false;
    }

    ResourceLocation profileId = easyModelNPC.getEasyModelProfileId();
    if (invalidProfileCache.containsKey(profileId)) {
      return false;
    }

    float bodyYaw = Mth.rotLerp(partialTicks, entity.yBodyRotO, entity.yBodyRot);

    try {
      boolean rendered;
      if (IntegrationRegistry.isGuiPreviewMode()) {
        rendered = renderPreview(entity, profileId, bodyYaw, poseStack, buffer, packedLight);
      } else {
        rendered =
            renderInWorld(
                easyModelNPC,
                entity,
                profileId,
                bodyYaw,
                partialTicks,
                poseStack,
                buffer,
                packedLight);
      }

      if (!rendered) {
        invalidProfileCache.put(profileId, Boolean.TRUE);
      }
      return rendered;
    } catch (Throwable throwable) {
      log.error("Failed to render Easy Model Entities profile {}:", profileId, throwable);
      invalidProfileCache.put(profileId, Boolean.TRUE);
      return false;
    }
  }

  private boolean renderPreview(
      E entity,
      ResourceLocation profileId,
      float bodyYaw,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    ModelBounds bounds = EasyModelEntitiesClientApi.getDisplayedBounds(profileId).orElse(null);
    if (bounds == null) {
      return false;
    }

    float displayedSubject =
        Math.max((float) Math.hypot(bounds.sizeX(), bounds.sizeZ()), bounds.sizeY());
    float previewScale = getGuiPreviewScale(profileId, displayedSubject, entity.getBbHeight());
    float yLift =
        Math.max(
            0f, (EntityTypeManager.GUI_PREVIEW_TARGET_HEIGHT - previewScale * bounds.sizeY()) / 2f);
    poseStack.pushPose();
    poseStack.translate(0.0, yLift, 0.0);
    poseStack.scale(previewScale, previewScale, previewScale);
    boolean rendered =
        EasyModelEntitiesClientApi.render(
            profileId,
            poseStack,
            buffer,
            packedLight,
            bodyYaw,
            EasyModelEntityRenderOptions.DEFAULT);
    poseStack.popPose();
    return rendered;
  }

  private boolean renderInWorld(
      EasyModelNPC easyModelNPC,
      E entity,
      ResourceLocation profileId,
      float bodyYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    EasyModelEntityRenderOptions renderOptions = EasyModelEntityRenderOptions.DEFAULT;
    if (easyModelNPC.hasChangedModel()) {
      renderOptions = renderOptions.withPartAnimator(createPartAnimator(easyModelNPC));
    }

    CustomScale rootScale = easyModelNPC.getModelRootData().scale();
    boolean scaled = rootScale.x() != 1.0f || rootScale.y() != 1.0f || rootScale.z() != 1.0f;
    if (scaled) {
      poseStack.pushPose();
      poseStack.scale(rootScale.x(), rootScale.y(), rootScale.z());
    }
    boolean rendered =
        EasyModelEntitiesClientApi.render(
            entity,
            profileId,
            poseStack,
            buffer,
            packedLight,
            bodyYaw,
            partialTicks,
            renderOptions);
    if (scaled) {
      poseStack.popPose();
    }
    return rendered;
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
    if (renderEasyModel(entity, partialTicks, poseStack, bufferSource, packedLight)) {
      if (this.shouldShowName(entity)) {
        this.renderNameTag(
            entity, entity.getDisplayName(), poseStack, bufferSource, packedLight, partialTicks);
      }
      Entity leashHolder = entity.getLeashHolder();
      if (leashHolder != null) {
        ((MobRendererInvoker) this)
            .invokeRenderLeash(entity, partialTicks, poseStack, bufferSource, leashHolder);
      }
      return;
    }
    super.render(entity, entityYaw, partialTicks, poseStack, bufferSource, packedLight);
  }
}
