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

import de.markusbordihn.easymodelentities.api.client.EasyModelEntitiesClientApi;
import de.markusbordihn.easymodelentities.api.client.EasyModelPartAnimator;
import de.markusbordihn.easymodelentities.api.data.EasyModelBodyType;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelEntityRenderOptions;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelHeadLook;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelPartAnimationMode;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelPartTransform;
import de.markusbordihn.easynpc.api.texture.ModelTextureAPI;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesLoader;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.model.ModelAnimationBehavior;
import de.markusbordihn.easynpc.data.model.ModelAnimationOperation;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.render.ModelTextureSetting;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import net.minecraft.resources.Identifier;
import net.minecraft.util.Mth;

public final class EasyModelNPCRenderOptions {

  private static final float HOVER_BOB_SPEED = 0.08F;
  private static final float HOVER_BOB_PIXELS = 0.6F;

  private EasyModelNPCRenderOptions() {}

  public static EasyModelEntityRenderOptions create(EasyModelNPC easyModelNPC) {
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
    int opacity = AttributeHandler.getOpacity(easyModelNPC);
    if (opacity < DisplayAttributeType.MAX_OPACITY) {
      renderOptions = renderOptions.withOpacity(opacity / (float) DisplayAttributeType.MAX_OPACITY);
    }
    return renderOptions;
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
}
