/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.easynpc.client.renderer.blockentity;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.block.entity.EasyNPCSpawnerBlockEntity;
import de.markusbordihn.easynpc.level.BaseEasyNPCSpawner;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderer;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.BaseSpawner;

public class BaseEasyNPCSpawnerBlockEntityRenderer<T extends EasyNPCSpawnerBlockEntity>
    implements BlockEntityRenderer<T> {

  public BaseEasyNPCSpawnerBlockEntityRenderer(BlockEntityRendererProvider.Context context) {}

  @Override
  public void render(
      T baseEasyNPCSpawnerBlockEntity,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight,
      int packedOverlay) {
    // Check if the spawner has a valid entity and ignore non-valid entities.
    BaseSpawner baseSpawner = baseEasyNPCSpawnerBlockEntity.getSpawner();
    if (!(baseSpawner instanceof BaseEasyNPCSpawner baseEasyNPCSpawner)
        || !baseEasyNPCSpawner.hasEasyNPC()) {
      return;
    }

    poseStack.pushPose();
    poseStack.translate(0.5F, 0.0F, 0.5F);
    Entity entity =
        baseSpawner.getOrCreateDisplayEntity(
            baseEasyNPCSpawnerBlockEntity.getLevel(), baseEasyNPCSpawnerBlockEntity.getBlockPos());
    if (entity == null) {
      poseStack.popPose();
      return;
    }

    float scale = 0.53125F;
    float maxDimension = Math.max(entity.getBbWidth(), entity.getBbHeight());
    if (maxDimension > 1.0F) {
      scale /= maxDimension;
    }
    poseStack.translate(0.0F, 0.4F, 0.0F);
    poseStack.mulPose(
        Axis.YP.rotationDegrees(
            (float)
                (Mth.lerp(partialTicks, baseEasyNPCSpawner.getoSpin(), baseEasyNPCSpawner.getSpin())
                    * 10.0F)));
    poseStack.translate(0.0F, -0.2F, 0.0F);
    poseStack.mulPose(Axis.XP.rotationDegrees(-30.0F));
    poseStack.scale(scale, scale, scale);
    Minecraft.getInstance()
        .getEntityRenderDispatcher()
        .render(entity, 0.0F, 0.0F, 0.0F, 0.0F, partialTicks, poseStack, bufferSource, packedLight);
    poseStack.popPose();
  }
}
