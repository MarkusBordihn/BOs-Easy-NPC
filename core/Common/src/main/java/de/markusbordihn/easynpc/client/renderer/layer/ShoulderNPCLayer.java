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

package de.markusbordihn.easynpc.client.renderer.layer;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.data.npc.ShoulderSide;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Optional;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.player.PlayerModel;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.client.renderer.entity.state.AvatarRenderState;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.state.CameraRenderState;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;

public class ShoulderNPCLayer extends RenderLayer<AvatarRenderState, PlayerModel> {

  private static final float SHOULDER_SIDE_OFFSET = 0.4F;
  private static final float CROUCHING_SHOULDER_DROP = 0.2F;

  public ShoulderNPCLayer(RenderLayerParent<AvatarRenderState, PlayerModel> renderer) {
    super(renderer);
  }

  @Override
  public void submit(
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      int packedLight,
      AvatarRenderState renderState,
      float yRot,
      float xRot) {
    this.submitShoulder(
        poseStack, submitNodeCollector, packedLight, renderState, ShoulderSide.LEFT);
    this.submitShoulder(
        poseStack, submitNodeCollector, packedLight, renderState, ShoulderSide.RIGHT);
  }

  private void submitShoulder(
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      int packedLight,
      AvatarRenderState renderState,
      ShoulderSide shoulderSide) {
    Optional<EasyNPC<?>> shoulderNPC =
        ShoulderNPCCache.get(ShoulderNPCCache.getShoulderEntity(renderState.id, shoulderSide));
    if (shoulderNPC.isEmpty()
        || ShoulderNPCCache.isRenderedByEasyModelEntities(shoulderNPC.get())) {
      return;
    }

    Entity shoulderEntity = shoulderNPC.get().getEntity();
    int playerTickCount = Mth.floor(renderState.ageInTicks);
    shoulderEntity.tickCount = playerTickCount;
    @SuppressWarnings("unchecked")
    EntityRenderer<Entity, EntityRenderState> shoulderRenderer =
        (EntityRenderer<Entity, EntityRenderState>)
            Minecraft.getInstance().getEntityRenderDispatcher().getRenderer(shoulderEntity);
    EntityRenderState shoulderRenderState =
        shoulderRenderer.createRenderState(shoulderEntity, renderState.ageInTicks - playerTickCount);
    shoulderRenderState.lightCoords = packedLight;
    shoulderRenderState.nameTag = null;

    poseStack.pushPose();
    this.getParentModel().body.translateAndRotate(poseStack);
    poseStack.translate(
        shoulderSide == ShoulderSide.LEFT ? SHOULDER_SIDE_OFFSET : -SHOULDER_SIDE_OFFSET,
        renderState.isCrouching ? CROUCHING_SHOULDER_DROP : 0.0F,
        0.0F);
    poseStack.mulPose(Axis.ZP.rotationDegrees(180.0F));
    Vec3 shoulderOffset =
        shoulderNPC
            .get()
            .getEasyNPCAttributeData()
            .getEntityAttributes()
            .getInteractionAttributes()
            .shoulderOffset();
    poseStack.translate(
        shoulderSide == ShoulderSide.LEFT ? -shoulderOffset.x : shoulderOffset.x,
        shoulderOffset.y,
        -shoulderOffset.z);
    shoulderRenderer.submit(
        shoulderRenderState, poseStack, submitNodeCollector, new CameraRenderState());
    poseStack.popPose();
  }
}
