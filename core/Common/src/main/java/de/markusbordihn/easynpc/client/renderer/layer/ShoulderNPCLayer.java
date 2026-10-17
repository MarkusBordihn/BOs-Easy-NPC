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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Optional;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.player.AbstractClientPlayer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;

public class ShoulderNPCLayer
    extends RenderLayer<AbstractClientPlayer, PlayerModel<AbstractClientPlayer>> {

  private static final float SHOULDER_SIDE_OFFSET = 0.4F;
  private static final float CROUCHING_SHOULDER_DROP = 0.2F;

  public ShoulderNPCLayer(
      RenderLayerParent<AbstractClientPlayer, PlayerModel<AbstractClientPlayer>> renderer) {
    super(renderer);
  }

  @Override
  public void render(
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight,
      AbstractClientPlayer player,
      float limbSwing,
      float limbSwingAmount,
      float partialTick,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    this.renderShoulder(
        poseStack,
        bufferSource,
        packedLight,
        player,
        partialTick,
        player.getShoulderEntityLeft(),
        true);
    this.renderShoulder(
        poseStack,
        bufferSource,
        packedLight,
        player,
        partialTick,
        player.getShoulderEntityRight(),
        false);
  }

  private void renderShoulder(
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight,
      AbstractClientPlayer player,
      float partialTick,
      CompoundTag shoulderTag,
      boolean leftShoulder) {
    Optional<EasyNPC<?>> shoulderNPC = ShoulderNPCCache.get(shoulderTag);
    if (shoulderNPC.isEmpty()
        || ShoulderNPCCache.isRenderedByEasyModelEntities(shoulderNPC.get())) {
      return;
    }

    Entity shoulderEntity = shoulderNPC.get().getEntity();
    shoulderEntity.tickCount = player.tickCount;
    poseStack.pushPose();
    this.getParentModel().body.translateAndRotate(poseStack);
    poseStack.translate(
        leftShoulder ? SHOULDER_SIDE_OFFSET : -SHOULDER_SIDE_OFFSET,
        player.isCrouching() ? CROUCHING_SHOULDER_DROP : 0.0F,
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
        leftShoulder ? -shoulderOffset.x : shoulderOffset.x, shoulderOffset.y, -shoulderOffset.z);
    Minecraft.getInstance()
        .getEntityRenderDispatcher()
        .getRenderer(shoulderEntity)
        .render(
            shoulderEntity,
            ShoulderNPCCache.FORWARD_FACING_YAW,
            partialTick,
            poseStack,
            bufferSource,
            packedLight);
    poseStack.popPose();
  }
}
