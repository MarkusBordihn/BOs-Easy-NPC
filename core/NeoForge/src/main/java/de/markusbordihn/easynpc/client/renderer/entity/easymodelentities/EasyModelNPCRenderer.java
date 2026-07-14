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
import de.markusbordihn.easymodelentities.api.data.client.EasyModelItemAnchor;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelPartPose;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelPartTransform;
import de.markusbordihn.easymodelentities.client.render.EasyModelEntityRenderBackend;
import de.markusbordihn.easymodelentities.data.model.Vec3f;
import de.markusbordihn.easymodelentities.data.model.bake.ModelBounds;
import de.markusbordihn.easymodelentities.runtime.EasyModelAnimationState;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.item.ItemModelResolver;
import net.minecraft.client.renderer.item.ItemStackRenderState;
import net.minecraft.client.renderer.state.level.CameraRenderState;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.resources.Identifier;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.LivingEntity;
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
  private static final Map<Identifier, Boolean> invalidProfileCache = new ConcurrentHashMap<>();
  private static final Map<Identifier, Float> guiPreviewScaleCache = new ConcurrentHashMap<>();
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
      Identifier profileId, float displayedSubject, float fallbackHeight) {
    return guiPreviewScaleCache.computeIfAbsent(
        profileId,
        id -> {
          float subject = displayedSubject > 0f ? displayedSubject : fallbackHeight;
          return EntityTypeManager.calculateGuiPreviewScaleFactor(subject);
        });
  }

  private static Identifier resolveRenderProfile(
      EasyModelNPCRenderState renderState, Identifier profileId) {
    if (!invalidProfileCache.containsKey(profileId)) {
      if (resolveEasyModelRenderState(renderState, profileId)) {
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
        && resolveEasyModelRenderState(renderState, FALLBACK_PROFILE_ID)) {
      return FALLBACK_PROFILE_ID;
    }
    return null;
  }

  private static boolean resolveEasyModelRenderState(
      EasyModelNPCRenderState renderState, Identifier profileId) {
    renderState.easyModelRenderState =
        EasyModelEntityRenderBackend.resolveContract(profileId, EasyModelAnimationState.AUTO)
            .map(EasyModelEntityRenderBackend::resolveRenderState)
            .orElse(null);
    return renderState.easyModelRenderState != null;
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
    renderState.easyModelRenderState = null;
    renderState.partAnimator = EasyModelPartAnimator.NONE;
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
    Identifier renderProfileId = resolveRenderProfile(renderState, profileId);
    if (renderProfileId == null || renderState.easyModelRenderState == null) {
      return;
    }
    renderState.profileId = renderProfileId;

    renderState.entityYaw =
        entity instanceof LivingEntity le
            ? Mth.rotLerp(partialTick, le.yBodyRotO, le.yBodyRot)
            : Mth.rotLerp(partialTick, entity.yRotO, entity.getYRot());

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
    renderState.airborneAmount = EasyModelEntityRenderBackend.airborneAmount(entity);
    renderState.attackAmount = EasyModelEntityRenderBackend.attackAmount(entity, partialTick);
    if (entity instanceof LivingEntity le) {
      renderState.limbSwing = le.walkAnimation.position(partialTick);
      renderState.limbSwingAmount = Math.min(le.walkAnimation.speed(partialTick), 1.0f);
    }

    if (easyModelNPC.hasChangedModel()) {
      renderState.partAnimator = createPartAnimator(easyModelNPC);
    }
  }

  private void extractGuiPreviewState(
      E entity, EasyModelNPCRenderState renderState, Identifier profileId) {
    renderState.limbSwing = 0.0f;
    renderState.limbSwingAmount = 0.0f;
    renderState.airborneAmount = 0.0f;
    renderState.attackAmount = 0.0f;

    ModelBounds bounds = EasyModelEntitiesClientApi.getDisplayedBounds(profileId).orElse(null);
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
    Vec3f localOffset = anchor.localOffset();
    poseStack.translate(localOffset.x() / 16.0f, localOffset.y() / 16.0f, localOffset.z() / 16.0f);
    poseStack.mulPose(Axis.XP.rotationDegrees(-90.0f));
    poseStack.mulPose(Axis.YP.rotationDegrees(180.0f));
    itemRenderState.submit(
        poseStack, submitNodeCollector, renderState.lightCoords, OverlayTexture.NO_OVERLAY, 0);
    poseStack.popPose();
  }
}
