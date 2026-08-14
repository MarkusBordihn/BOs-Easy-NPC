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
import com.mojang.math.Axis;
import de.markusbordihn.easymodelentities.api.EasyModelReloadEvents;
import de.markusbordihn.easymodelentities.api.client.EasyModelEntitiesClientApi;
import de.markusbordihn.easymodelentities.api.client.EasyModelPartAnimator;
import de.markusbordihn.easymodelentities.api.client.EasyModelPartPoseListener;
import de.markusbordihn.easymodelentities.api.data.EasyModelAnimation;
import de.markusbordihn.easymodelentities.api.data.EasyModelBodyType;
import de.markusbordihn.easymodelentities.api.data.EasyModelVec3f;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelAnimationPlayback;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelAnimationPlaybackMode;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelAnimationSwitchTiming;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelAnimationTransition;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelBounds;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelEntityRenderOptions;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelHeadLook;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelItemAnchor;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelPartAnimationMode;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelPartPose;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelPartTransform;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.api.texture.ModelTextureAPI;
import de.markusbordihn.easynpc.client.model.custom.DopplerModel;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.SpeechBubbleRenderer;
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesLoader;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.data.model.ModelAnimationBehavior;
import de.markusbordihn.easynpc.data.model.ModelAnimationOperation;
import de.markusbordihn.easynpc.data.model.ModelAnimationPlayback;
import de.markusbordihn.easynpc.data.model.ModelAnimationPlaybackMode;
import de.markusbordihn.easynpc.data.model.ModelAnimationRequest;
import de.markusbordihn.easynpc.data.model.ModelAnimationSwitchTiming;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.render.ModelTextureSetting;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.variant.DopplerSkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import de.markusbordihn.easynpc.mixin.renderer.MobRendererInvoker;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.ItemInHandRenderer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyModelNPCRenderer<E extends PathfinderMob>
    extends HumanoidMobRenderer<E, DopplerModel<E>> implements EasyNPCEntityRenderer {

  protected static final ResourceLocation DEFAULT_TEXTURE =
      DopplerSkinVariant.DOPPLER.getTextureLocation();
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final float HOVER_BOB_SPEED = 0.08F;
  private static final float HOVER_BOB_PIXELS = 0.6F;
  private static final Map<ResourceLocation, Boolean> invalidProfileCache =
      new ConcurrentHashMap<>();
  private static final Map<ResourceLocation, Float> guiPreviewScaleCache =
      new ConcurrentHashMap<>();
  private static final Map<Entity, Integer> handledAnimationRequests =
      Collections.synchronizedMap(new WeakHashMap<>());
  private static boolean reloadListenerRegistered = false;

  private final ItemInHandRenderer itemInHandRenderer;

  public EasyModelNPCRenderer(
      EntityRendererProvider.Context context, ModelLayerLocation modelLayerLocation) {
    super(context, new DopplerModel<>(context.bakeLayer(modelLayerLocation)), 0.5F);
    this.itemInHandRenderer = context.getItemInHandRenderer();
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

  private static boolean isFloatingModel(EasyModelNPC easyModelNPC) {
    ResourceLocation profileId = easyModelNPC.getEasyModelProfileId();
    return profileId != null
        && EasyModelEntitiesClientApi.getBodyType(profileId)
            .filter(bodyType -> bodyType == EasyModelBodyType.FLOATING)
            .isPresent();
  }

  private static EasyModelPartTransform createModelPartTransform(
      EasyModelNPC easyModelNPC, ModelPartType modelPartType) {
    CustomRotation rotation = easyModelNPC.getModelPartRotation(modelPartType);
    CustomPosition position = easyModelNPC.getModelPartPosition(modelPartType);
    CustomScale scale = easyModelNPC.getModelPartScale(modelPartType);
    boolean visible = easyModelNPC.getModelPartVisibility(modelPartType);
    return new EasyModelPartTransform(rotation.x(), rotation.y(), rotation.z())
        .withOffset(position.x(), position.y(), position.z())
        .withScale(scale.x(), scale.y(), scale.z())
        .withVisible(visible);
  }

  private static EasyModelPartTransform createHoverTransform(float ageInTicks) {
    return EasyModelPartTransform.NONE.withOffset(
        0.0F, Mth.sin(ageInTicks * HOVER_BOB_SPEED) * HOVER_BOB_PIXELS, 0.0F);
  }

  private static EasyModelPartAnimator createPartAnimator(
      EasyModelNPC easyModelNPC, boolean hasModelChanges, boolean isFloating) {
    return context -> {
      ModelPartType modelPartType = EasyModelEntitiesManager.getModelPartType(context.partName());
      EasyModelPartTransform partTransform =
          hasModelChanges && modelPartType != ModelPartType.UNKNOWN
              ? createModelPartTransform(easyModelNPC, modelPartType)
              : EasyModelPartTransform.NONE;
      if (isFloating && modelPartType == ModelPartType.ROOT) {
        return partTransform.add(createHoverTransform(context.ageInTicks()));
      }
      return partTransform;
    };
  }

  private static EasyModelEntityRenderOptions createRenderOptions(EasyModelNPC easyModelNPC) {
    EasyModelEntityRenderOptions renderOptions = EasyModelEntityRenderOptions.DEFAULT;
    boolean hasModelChanges = easyModelNPC.hasChangedModel();
    boolean isFloating = isFloatingModel(easyModelNPC);
    if (hasModelChanges || isFloating) {
      renderOptions =
          renderOptions.withPartAnimator(
              createPartAnimator(easyModelNPC, hasModelChanges, isFloating));
    }
    if (easyModelNPC.getModelPartRotation(ModelPartType.HEAD).hasChangedRotation()) {
      renderOptions = renderOptions.withHeadLook(EasyModelHeadLook.NONE);
    }
    ModelAnimationBehavior behavior = easyModelNPC.getModelAnimationBehavior();
    boolean manualAnimation =
        easyModelNPC.getModelAnimationRequest().operation() == ModelAnimationOperation.PLAY;
    if (!manualAnimation
        && (behavior == ModelAnimationBehavior.NONE
            || (behavior == ModelAnimationBehavior.DEFAULT && hasModelChanges))) {
      renderOptions = renderOptions.withPartAnimationMode(EasyModelPartAnimationMode.REPLACE);
    }
    ModelTextureSetting textureSetting = ModelTextureAPI.getTextureSetting(easyModelNPC);
    if (!textureSetting.isEmpty()) {
      renderOptions =
          renderOptions.withTextureSetting(
              EasyModelEntitiesLoader.toEasyModelTextureSetting(textureSetting));
    }
    return renderOptions;
  }

  private static ModelAnimationRequest prepareAnimationPlayback(
      Entity entity, EasyModelNPC easyModelNPC) {
    ModelAnimationRequest request = easyModelNPC.getModelAnimationRequest();
    if (!request.isPresent()
        || handledAnimationRequests.getOrDefault(entity, 0) == request.sequence()) {
      return null;
    }

    boolean staleOneShot =
        (request.operation() == ModelAnimationOperation.RESTART
                || (request.operation() == ModelAnimationOperation.PLAY
                    && request.playback().isSingleRun()))
            && entity.level().getGameTime() - request.issuedGameTime() > 20L;
    if (staleOneShot) {
      handledAnimationRequests.put(entity, request.sequence());
      return null;
    }

    EasyModelAnimationTransition transition = toEasyModelTransition(request);
    switch (request.operation()) {
      case PLAY:
        EasyModelAnimation animation =
            EasyModelAnimation.parse(request.animationName()).orElse(null);
        if (animation == null) {
          handledAnimationRequests.put(entity, request.sequence());
          return null;
        }
        EasyModelEntitiesClientApi.playAnimation(
            entity, animation, toEasyModelPlayback(request), transition);
        break;
      case STOP:
        EasyModelEntitiesClientApi.stopAnimation(entity, transition);
        break;
      case RESTART:
        EasyModelEntitiesClientApi.restartAnimation(entity);
        break;
      default:
        handledAnimationRequests.put(entity, request.sequence());
        return null;
    }
    return request;
  }

  private static EasyModelAnimationTransition toEasyModelTransition(ModelAnimationRequest request) {
    EasyModelAnimationSwitchTiming timing =
        request.transition().timing() == ModelAnimationSwitchTiming.AFTER_CURRENT
            ? EasyModelAnimationSwitchTiming.AFTER_CURRENT
            : EasyModelAnimationSwitchTiming.IMMEDIATE;
    return new EasyModelAnimationTransition(timing, request.transition().blendDurationTicks());
  }

  private static EasyModelAnimationPlayback toEasyModelPlayback(ModelAnimationRequest request) {
    ModelAnimationPlayback playback = request.playback();
    return new EasyModelAnimationPlayback(
        toEasyModelPlaybackMode(playback.mode()), playback.repeatCount(), playback.durationTicks());
  }

  private static EasyModelAnimationPlaybackMode toEasyModelPlaybackMode(
      ModelAnimationPlaybackMode playbackMode) {
    return switch (playbackMode) {
      case LOOP -> EasyModelAnimationPlaybackMode.LOOP;
      case REPEAT -> EasyModelAnimationPlaybackMode.REPEAT;
      case ONCE -> EasyModelAnimationPlaybackMode.ONCE;
    };
  }

  private static void markAnimationRequestHandled(
      Entity entity, ModelAnimationRequest request, boolean rendered) {
    if (request == null || !rendered) {
      return;
    }

    handledAnimationRequests.put(entity, request.sequence());
  }

  private static void applyRootRotation(
      EasyModelNPC easyModelNPC, PoseStack poseStack, float pivotY) {
    CustomRotation rootRotation = easyModelNPC.getModelRootData().rotation();
    if (!rootRotation.hasChangedRotation()) {
      return;
    }

    float xDeg = (float) Math.toDegrees(rootRotation.x());
    float zDeg = (float) Math.toDegrees(rootRotation.z());
    if (xDeg == 0.0f && zDeg == 0.0f) {
      return;
    }

    poseStack.translate(0.0f, pivotY, 0.0f);
    if (xDeg != 0.0f) {
      poseStack.mulPose(Axis.XP.rotationDegrees(xDeg));
    }
    if (zDeg != 0.0f) {
      poseStack.mulPose(Axis.ZP.rotationDegrees(zDeg));
    }
    poseStack.translate(0.0f, -pivotY, 0.0f);
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

  private HandPoseCapture createHandPoseCapture(E entity, ResourceLocation profileId) {
    ItemStack mainHandItem = entity.getMainHandItem();
    ItemStack offHandItem = entity.getOffhandItem();
    if (mainHandItem.isEmpty() && offHandItem.isEmpty()) {
      return null;
    }

    HumanoidArm mainArm = entity.getMainArm();
    EasyModelItemAnchor mainHandAnchor =
        mainHandItem.isEmpty()
            ? null
            : EasyModelEntitiesClientApi.getItemAnchor(profileId, mainArm).orElse(null);
    EasyModelItemAnchor offHandAnchor =
        offHandItem.isEmpty()
            ? null
            : EasyModelEntitiesClientApi.getItemAnchor(profileId, mainArm.getOpposite())
                .orElse(null);
    if (mainHandAnchor == null && offHandAnchor == null) {
      return null;
    }
    return new HandPoseCapture(mainHandAnchor, offHandAnchor);
  }

  private void renderHandItems(
      E entity,
      HandPoseCapture handPoseCapture,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    if (handPoseCapture == null) {
      return;
    }

    HumanoidArm mainArm = entity.getMainArm();
    renderHandItem(
        entity,
        entity.getMainHandItem(),
        handPoseCapture.mainHandAnchor,
        handPoseCapture.mainHandPose,
        mainArm,
        poseStack,
        buffer,
        packedLight);
    renderHandItem(
        entity,
        entity.getOffhandItem(),
        handPoseCapture.offHandAnchor,
        handPoseCapture.offHandPose,
        mainArm.getOpposite(),
        poseStack,
        buffer,
        packedLight);
  }

  private void renderHandItem(
      E entity,
      ItemStack itemStack,
      EasyModelItemAnchor itemAnchor,
      EasyModelPartPose partPose,
      HumanoidArm arm,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    if (itemStack.isEmpty() || itemAnchor == null || partPose == null) {
      return;
    }

    poseStack.pushPose();
    partPose.applyTo(poseStack);
    EasyModelVec3f localOffset = itemAnchor.localOffset();
    poseStack.translate(localOffset.x() / 16.0f, localOffset.y() / 16.0f, localOffset.z() / 16.0f);
    poseStack.mulPose(Axis.XP.rotationDegrees(-90.0f));
    poseStack.mulPose(Axis.YP.rotationDegrees(180.0f));
    boolean isLeftHand = arm == HumanoidArm.LEFT;
    this.itemInHandRenderer.renderItem(
        entity,
        itemStack,
        isLeftHand
            ? ItemDisplayContext.THIRD_PERSON_LEFT_HAND
            : ItemDisplayContext.THIRD_PERSON_RIGHT_HAND,
        isLeftHand,
        poseStack,
        buffer,
        packedLight);
    poseStack.popPose();
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
        rendered =
            renderPreview(easyModelNPC, entity, profileId, bodyYaw, poseStack, buffer, packedLight);
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
      EasyModelNPC easyModelNPC,
      E entity,
      ResourceLocation profileId,
      float bodyYaw,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    EasyModelBounds bounds = EasyModelEntitiesClientApi.getDisplayedBounds(profileId).orElse(null);
    if (bounds == null) {
      return false;
    }

    float displayedSubject =
        Math.max((float) Math.hypot(bounds.sizeX(), bounds.sizeZ()), bounds.sizeY());
    float previewScale = getGuiPreviewScale(profileId, displayedSubject, entity.getBbHeight());
    CustomScale rootScale = easyModelNPC.getModelRootData().scale();
    float displayedHeight = previewScale * rootScale.y() * bounds.sizeY();
    float yLift =
        Math.max(0f, (EntityTypeManager.GUI_PREVIEW_TARGET_HEIGHT - displayedHeight) / 2f);
    poseStack.pushPose();
    poseStack.translate(0.0, yLift, 0.0);
    applyRootRotation(easyModelNPC, poseStack, displayedHeight * 0.5f);
    poseStack.scale(
        previewScale * rootScale.x(), previewScale * rootScale.y(), previewScale * rootScale.z());
    HandPoseCapture handPoseCapture = createHandPoseCapture(entity, profileId);
    EasyModelEntityRenderOptions renderOptions = createRenderOptions(easyModelNPC);
    if (handPoseCapture != null) {
      renderOptions = renderOptions.withPartPoseListener(handPoseCapture);
    }
    boolean rendered =
        EasyModelEntitiesClientApi.render(
            profileId, poseStack, buffer, packedLight, bodyYaw, renderOptions);
    poseStack.popPose();
    if (rendered) {
      renderHandItems(entity, handPoseCapture, poseStack, buffer, packedLight);
    }
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
    CustomScale rootScale = easyModelNPC.getModelRootData().scale();
    boolean scaled = rootScale.x() != 1.0f || rootScale.y() != 1.0f || rootScale.z() != 1.0f;
    boolean rotated = easyModelNPC.getModelRootData().rotation().hasChangedRotation();
    if (scaled || rotated) {
      poseStack.pushPose();
      applyRootRotation(easyModelNPC, poseStack, entity.getBbHeight() * 0.5f);
      if (scaled) {
        poseStack.scale(rootScale.x(), rootScale.y(), rootScale.z());
      }
    }
    HandPoseCapture handPoseCapture = createHandPoseCapture(entity, profileId);
    ModelAnimationRequest animationRequest = prepareAnimationPlayback(entity, easyModelNPC);
    EasyModelEntityRenderOptions renderOptions = createRenderOptions(easyModelNPC);
    if (handPoseCapture != null) {
      renderOptions = renderOptions.withPartPoseListener(handPoseCapture);
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
    markAnimationRequestHandled(entity, animationRequest, rendered);
    if (scaled || rotated) {
      poseStack.popPose();
    }
    if (rendered) {
      renderHandItems(entity, handPoseCapture, poseStack, buffer, packedLight);
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
        this.renderNameTag(entity, entity.getDisplayName(), poseStack, bufferSource, packedLight);
      }

      // This branch never reaches EntityRenderer#render, where the speech bubble mixin is attached.
      SpeechBubbleRenderer.render(entity, poseStack, bufferSource, packedLight);

      Entity leashHolder = entity.getLeashHolder();
      if (leashHolder != null) {
        ((MobRendererInvoker) this)
            .invokeRenderLeash(entity, partialTicks, poseStack, bufferSource, leashHolder);
      }
      return;
    }
    super.render(entity, entityYaw, partialTicks, poseStack, bufferSource, packedLight);
  }

  private static final class HandPoseCapture implements EasyModelPartPoseListener {

    private final EasyModelItemAnchor mainHandAnchor;
    private final EasyModelItemAnchor offHandAnchor;
    private EasyModelPartPose mainHandPose;
    private EasyModelPartPose offHandPose;

    private HandPoseCapture(EasyModelItemAnchor mainHandAnchor, EasyModelItemAnchor offHandAnchor) {
      this.mainHandAnchor = mainHandAnchor;
      this.offHandAnchor = offHandAnchor;
    }

    @Override
    public boolean wantsPart(String partName) {
      return (this.mainHandAnchor != null && this.mainHandAnchor.partName().equals(partName))
          || (this.offHandAnchor != null && this.offHandAnchor.partName().equals(partName));
    }

    @Override
    public void onPartPose(EasyModelPartPose partPose) {
      if (this.mainHandAnchor != null
          && this.mainHandAnchor.partName().equals(partPose.partName())) {
        this.mainHandPose = partPose;
      }
      if (this.offHandAnchor != null && this.offHandAnchor.partName().equals(partPose.partName())) {
        this.offHandPose = partPose;
      }
    }
  }
}
