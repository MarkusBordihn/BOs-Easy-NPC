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
import de.markusbordihn.easymodelentities.api.data.client.EasyModelEntityRenderOptions;
import de.markusbordihn.easymodelentities.client.render.EasyModelEntityRenderBackend;
import de.markusbordihn.easymodelentities.client.render.EasyModelEntityRenderState;
import de.markusbordihn.easymodelentities.runtime.EasyModelAnimationState;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.state.level.CameraRenderState;
import net.minecraft.resources.Identifier;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyModelNPCRenderer<E extends PathfinderMob>
    extends EntityRenderer<E, EasyModelEntityRenderState> {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public EasyModelNPCRenderer(EntityRendererProvider.Context context) {
    super(context);
    this.shadowRadius = 0.3f;
  }

  @Override
  public EasyModelEntityRenderState createRenderState() {
    return new EasyModelEntityRenderState();
  }

  @Override
  public void extractRenderState(
      E entity, EasyModelEntityRenderState renderState, float partialTick) {
    super.extractRenderState(entity, renderState, partialTick);
    renderState.easyModelRenderState = null;
    renderState.renderOptions = EasyModelEntityRenderOptions.DEFAULT;
    renderState.animationState = EasyModelAnimationState.AUTO;
    renderState.limbSwing = 0.0f;
    renderState.limbSwingAmount = 0.0f;
    renderState.airborneAmount = 0.0f;
    if (!(entity instanceof EasyModelNPC easyModelNPC)) {
      return;
    }
    Identifier profileId = easyModelNPC.getEasyModelProfileId();
    if (profileId == null) {
      return;
    }
    EasyModelEntityRenderBackend.resolveContract(profileId, EasyModelAnimationState.AUTO)
        .ifPresentOrElse(
            contract ->
                renderState.easyModelRenderState =
                    EasyModelEntityRenderBackend.resolveRenderState(contract),
            () -> log.debug("No EME contract for profile: {}", profileId));

    renderState.renderOptions =
        EasyModelEntityRenderOptions.DEFAULT.withPartAnimator(
            EasyModelNPCPartAnimator.snapshot(easyModelNPC));

    renderState.entityYaw =
        entity instanceof LivingEntity le
            ? Mth.rotLerp(partialTick, le.yBodyRotO, le.yBodyRot)
            : Mth.rotLerp(partialTick, entity.yRotO, entity.getYRot());
    renderState.airborneAmount = EasyModelEntityRenderBackend.airborneAmount(entity);
    if (entity instanceof LivingEntity le) {
      renderState.limbSwing = le.walkAnimation.position(partialTick);
      renderState.limbSwingAmount = Math.min(le.walkAnimation.speed(partialTick), 1.0f);
    }
  }

  @Override
  public void submit(
      EasyModelEntityRenderState renderState,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState) {
    if (renderState.easyModelRenderState == null) {
      return;
    }
    EasyModelEntityRenderBackend.render(
        renderState, poseStack, submitNodeCollector, renderState.lightCoords);
  }
}
