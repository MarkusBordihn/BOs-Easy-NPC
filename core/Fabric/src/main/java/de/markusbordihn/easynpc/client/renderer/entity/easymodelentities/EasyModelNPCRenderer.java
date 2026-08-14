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
import de.markusbordihn.easymodelentities.api.data.EasyModelAnimation;
import de.markusbordihn.easymodelentities.api.data.EasyModelAnimationSetting;
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
import de.markusbordihn.easymodelentities.client.render.EasyModelEntityRenderBackend;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.api.texture.ModelTextureAPI;
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
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.item.ItemModelResolver;
import net.minecraft.client.renderer.item.ItemStackRenderState;
import net.minecraft.client.renderer.state.CameraRenderState;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.resources.Identifier;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyModelNPCRenderer<E extends PathfinderMob>
    extends EntityRenderer<E, EasyModelNPCRenderState> {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Identifier FALLBACK_PROFILE_ID =
      Identifier.parse(EasyModelEntitiesManager.DEFAULT_PROFILE);
  private static final float HOVER_BOB_SPEED = 0.08F;
  private static final float HOVER_BOB_PIXELS = 0.6F;
  private static final Map<Identifier, Boolean> invalidProfileCache = new ConcurrentHashMap<>();
  private static final Map<Identifier, Float> guiPreviewScaleCache = new ConcurrentHashMap<>();
  private static final Map<Entity, Integer> handledAnimationRequests =
      Collections.synchronizedMap(new WeakHashMap<>());
  private static boolean reloadListenerRegistered = false;

  private final ItemModelResolver itemModelResolver;

  public EasyModelNPCRenderer(EntityRendererProvider.Context context) {
    super(context);
    this.shadowRadius = 0.3f;
    this.itemModelResolver = context.getItemModelResolver();
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
    Identifier profileId = easyModelNPC.getEasyModelProfileId();
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

  private static float getGuiPreviewScale(
      Identifier profileId, float displayedSubject, float fallbackHeight) {
    return guiPreviewScaleCache.computeIfAbsent(
        profileId,
        id -> {
          float subject = displayedSubject > 0f ? displayedSubject : fallbackHeight;
          return EntityTypeManager.calculateGuiPreviewScaleFactor(subject);
        });
  }

  private static Identifier resolveRenderProfile(
      Entity entity,
      EasyModelNPCRenderState renderState,
      Identifier profileId,
      float partialTick,
      EasyModelEntityRenderOptions renderOptions) {
    if (!invalidProfileCache.containsKey(profileId)) {
      if (extractEasyModelRenderState(entity, renderState, profileId, partialTick, renderOptions)) {
        return profileId;
      }
      invalidProfileCache.put(profileId, Boolean.TRUE);
      log.warn(
          "No Easy Model Entities contract for profile {}, using fallback profile {} instead.",
          profileId,
          FALLBACK_PROFILE_ID);
    }
    if (!profileId.equals(FALLBACK_PROFILE_ID)
        && !invalidProfileCache.containsKey(FALLBACK_PROFILE_ID)
        && extractEasyModelRenderState(
            entity, renderState, FALLBACK_PROFILE_ID, partialTick, renderOptions)) {
      return FALLBACK_PROFILE_ID;
    }
    return null;
  }

  private static boolean extractEasyModelRenderState(
      Entity entity,
      EasyModelNPCRenderState renderState,
      Identifier profileId,
      float partialTick,
      EasyModelEntityRenderOptions renderOptions) {
    renderState.easyModelRenderState = null;
    EasyModelEntityRenderBackend.resolveContract(profileId, EasyModelAnimationSetting.AUTO)
        .ifPresent(
            contract ->
                EasyModelEntityRenderBackend.extractRenderState(
                    entity, contract, renderState, partialTick, renderOptions));
    return renderState.easyModelRenderState != null;
  }

  private static EasyModelEntityRenderOptions createRenderOptions(EasyModelNPC easyModelNPC) {
    boolean hasModelChanges = easyModelNPC.hasChangedModel();
    boolean isFloating = isFloatingModel(easyModelNPC);
    EasyModelEntityRenderOptions renderOptions =
        EasyModelEntityRenderOptions.DEFAULT.withPartAnimationMode(
            resolvePartAnimationMode(easyModelNPC, hasModelChanges));
    if (hasModelChanges || isFloating) {
      renderOptions =
          renderOptions.withPartAnimator(
              createPartAnimator(easyModelNPC, hasModelChanges, isFloating));
    }
    if (easyModelNPC.getModelPartRotation(ModelPartType.HEAD).hasChangedRotation()) {
      renderOptions = renderOptions.withHeadLook(EasyModelHeadLook.NONE);
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

  private static void markAnimationRequestHandled(Entity entity, ModelAnimationRequest request) {
    if (request != null) {
      handledAnimationRequests.put(entity, request.sequence());
    }
  }

  private static EasyModelPartAnimationMode resolvePartAnimationMode(
      EasyModelNPC easyModelNPC, boolean hasModelChanges) {
    ModelAnimationBehavior behavior = easyModelNPC.getModelAnimationBehavior();
    boolean manualAnimation =
        easyModelNPC.getModelAnimationRequest().operation() == ModelAnimationOperation.PLAY;
    return !manualAnimation
            && (behavior == ModelAnimationBehavior.NONE
                || (behavior == ModelAnimationBehavior.DEFAULT && hasModelChanges))
        ? EasyModelPartAnimationMode.REPLACE
        : EasyModelPartAnimationMode.ADD;
  }

  private static void applyRootRotation(EasyModelNPCRenderState renderState, PoseStack poseStack) {
    float xDegrees = (float) Math.toDegrees(renderState.rootRotationX);
    float zDegrees = (float) Math.toDegrees(renderState.rootRotationZ);
    if (xDegrees == 0.0f && zDegrees == 0.0f) {
      return;
    }
    poseStack.translate(0.0f, renderState.rootPivotY, 0.0f);
    if (xDegrees != 0.0f) {
      poseStack.mulPose(Axis.XP.rotationDegrees(xDegrees));
    }
    if (zDegrees != 0.0f) {
      poseStack.mulPose(Axis.ZP.rotationDegrees(zDegrees));
    }
    poseStack.translate(0.0f, -renderState.rootPivotY, 0.0f);
  }

  @Override
  protected boolean shouldShowName(E entity, double distanceToCameraSq) {
    return Minecraft.renderNames() && super.shouldShowName(entity, distanceToCameraSq);
  }

  @Override
  public EasyModelNPCRenderState createRenderState() {
    return new EasyModelNPCRenderState();
  }

  @Override
  public void extractRenderState(E entity, EasyModelNPCRenderState renderState, float partialTick) {
    super.extractRenderState(entity, renderState, partialTick);
    renderState.setEasyNpcUUID(entity.getUUID());
    renderState.easyModelRenderState = null;
    renderState.profileId = null;
    renderState.rootScaleX = 1.0f;
    renderState.rootScaleY = 1.0f;
    renderState.rootScaleZ = 1.0f;
    renderState.previewScale = 0.0f;
    renderState.previewYLift = 0.0f;
    renderState.rootRotationX = 0.0f;
    renderState.rootRotationZ = 0.0f;
    renderState.rootPivotY = 0.0f;
    renderState.mainHandItem.clear();
    renderState.offHandItem.clear();
    renderState.mainHandAnchor = null;
    renderState.offHandAnchor = null;

    if (!(entity instanceof EasyModelNPC easyModelNPC)) {
      return;
    }
    Identifier profileId = easyModelNPC.getEasyModelProfileId();
    if (profileId == null) {
      return;
    }
    // The playback has to reach Easy Model Entities before it resolves the animation frame.
    ModelAnimationRequest animationRequest = prepareAnimationPlayback(entity, easyModelNPC);

    Identifier renderProfileId =
        resolveRenderProfile(
            entity, renderState, profileId, partialTick, createRenderOptions(easyModelNPC));
    if (renderProfileId == null || renderState.easyModelRenderState == null) {
      return;
    }
    renderState.profileId = renderProfileId;
    markAnimationRequestHandled(entity, animationRequest);

    CustomScale rootScale = easyModelNPC.getModelRootData().scale();
    renderState.rootScaleX = rootScale.x();
    renderState.rootScaleY = rootScale.y();
    renderState.rootScaleZ = rootScale.z();
    CustomRotation rootRotation = easyModelNPC.getModelRootData().rotation();
    renderState.rootRotationX = rootRotation.x();
    renderState.rootRotationZ = rootRotation.z();

    extractHandItems(entity, renderState, renderProfileId);

    if (IntegrationRegistry.isGuiPreviewMode()) {
      extractGuiPreviewState(entity, renderState, renderProfileId);
      return;
    }

    renderState.rootPivotY = entity.getBbHeight() * 0.5f;
  }

  private void extractGuiPreviewState(
      E entity, EasyModelNPCRenderState renderState, Identifier profileId) {
    renderState.limbSwing = 0.0f;
    renderState.limbSwingAmount = 0.0f;
    renderState.airborneAmount = 0.0f;
    renderState.attackAmount = 0.0f;

    EasyModelBounds bounds = EasyModelEntitiesClientApi.getDisplayedBounds(profileId).orElse(null);
    if (bounds == null) {
      return;
    }
    float displayedSubject =
        Math.max((float) Math.hypot(bounds.sizeX(), bounds.sizeZ()), bounds.sizeY());
    float previewScale = getGuiPreviewScale(profileId, displayedSubject, entity.getBbHeight());
    renderState.previewScale = previewScale;
    float displayedHeight = previewScale * renderState.rootScaleY * bounds.sizeY();
    renderState.previewYLift =
        Math.max(0f, (EntityTypeManager.GUI_PREVIEW_TARGET_HEIGHT - displayedHeight) / 2f);
    renderState.rootPivotY = displayedHeight * 0.5f;
  }

  @Override
  public void submit(
      EasyModelNPCRenderState renderState,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState) {
    if (renderState.easyModelRenderState != null) {
      poseStack.pushPose();
      if (renderState.previewScale > 0.0f) {
        poseStack.translate(0.0f, renderState.previewYLift, 0.0f);
        applyRootRotation(renderState, poseStack);
        poseStack.scale(
            renderState.previewScale * renderState.rootScaleX,
            renderState.previewScale * renderState.rootScaleY,
            renderState.previewScale * renderState.rootScaleZ);
      } else {
        applyRootRotation(renderState, poseStack);
        if (renderState.rootScaleX != 1.0f
            || renderState.rootScaleY != 1.0f
            || renderState.rootScaleZ != 1.0f) {
          poseStack.scale(renderState.rootScaleX, renderState.rootScaleY, renderState.rootScaleZ);
        }
      }
      try {
        EasyModelEntityRenderBackend.render(
            renderState, poseStack, submitNodeCollector, renderState.lightCoords);
      } catch (Exception exception) {
        if (renderState.profileId != null) {
          invalidProfileCache.put(renderState.profileId, Boolean.TRUE);
        }
        log.error(
            "Failed to render Easy Model Entities profile {}:", renderState.profileId, exception);
      }
      renderHandItems(renderState, poseStack, submitNodeCollector);
      poseStack.popPose();
    }
    super.submit(renderState, poseStack, submitNodeCollector, cameraRenderState);
  }

  private void extractHandItems(
      E entity, EasyModelNPCRenderState renderState, Identifier profileId) {
    HumanoidArm mainArm = entity.getMainArm();
    renderState.mainArmLeft = mainArm == HumanoidArm.LEFT;
    renderState.mainHandAnchor =
        extractHandItem(
            entity, entity.getMainHandItem(), mainArm, profileId, renderState.mainHandItem);
    renderState.offHandAnchor =
        extractHandItem(
            entity,
            entity.getOffhandItem(),
            mainArm.getOpposite(),
            profileId,
            renderState.offHandItem);
  }

  private EasyModelItemAnchor extractHandItem(
      E entity,
      ItemStack itemStack,
      HumanoidArm arm,
      Identifier profileId,
      ItemStackRenderState itemRenderState) {
    if (itemStack.isEmpty()) {
      return null;
    }
    EasyModelItemAnchor anchor =
        EasyModelEntitiesClientApi.getItemAnchor(profileId, arm).orElse(null);
    if (anchor == null) {
      return null;
    }
    this.itemModelResolver.updateForLiving(
        itemRenderState,
        itemStack,
        arm == HumanoidArm.LEFT
            ? ItemDisplayContext.THIRD_PERSON_LEFT_HAND
            : ItemDisplayContext.THIRD_PERSON_RIGHT_HAND,
        entity);
    return anchor;
  }

  private void renderHandItems(
      EasyModelNPCRenderState renderState,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector) {
    renderHandItem(
        renderState,
        renderState.mainHandItem,
        renderState.mainHandAnchor,
        poseStack,
        submitNodeCollector);
    renderHandItem(
        renderState,
        renderState.offHandItem,
        renderState.offHandAnchor,
        poseStack,
        submitNodeCollector);
  }

  private void renderHandItem(
      EasyModelNPCRenderState renderState,
      ItemStackRenderState itemRenderState,
      EasyModelItemAnchor anchor,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector) {
    if (anchor == null || itemRenderState.isEmpty()) {
      return;
    }
    EasyModelPartPose partPose =
        EasyModelEntityRenderBackend.resolvePartPose(renderState, anchor.partName(), poseStack)
            .orElse(null);
    if (partPose == null) {
      return;
    }
    poseStack.pushPose();
    partPose.applyTo(poseStack);
    EasyModelVec3f localOffset = anchor.localOffset();
    poseStack.translate(localOffset.x() / 16.0f, localOffset.y() / 16.0f, localOffset.z() / 16.0f);
    poseStack.mulPose(Axis.XP.rotationDegrees(-90.0f));
    poseStack.mulPose(Axis.YP.rotationDegrees(180.0f));
    itemRenderState.submit(
        poseStack, submitNodeCollector, renderState.lightCoords, OverlayTexture.NO_OVERLAY, 0);
    poseStack.popPose();
  }
}
