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
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderer;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.feature.ModelFeatureRenderer;
import net.minecraft.client.renderer.state.level.CameraRenderState;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.Nullable;

public class BaseEasyNPCSpawnerBlockEntityRenderer<T extends EasyNPCSpawnerBlockEntity>
    implements BlockEntityRenderer<T, EasyNPCSpawnerRenderState> {

  private final EntityRenderDispatcher entityRenderer;

  public BaseEasyNPCSpawnerBlockEntityRenderer(BlockEntityRendererProvider.Context context) {
    this.entityRenderer = context.entityRenderer();
  }

  @Override
  public EasyNPCSpawnerRenderState createRenderState() {
    return new EasyNPCSpawnerRenderState();
  }

  @Override
  public void extractRenderState(
      T blockEntity,
      EasyNPCSpawnerRenderState renderState,
      float partialTicks,
      Vec3 cameraPos,
      @Nullable ModelFeatureRenderer.CrumblingOverlay crumblingOverlay) {
    BlockEntityRenderer.super.extractRenderState(
        blockEntity, renderState, partialTicks, cameraPos, crumblingOverlay);

    Level level = blockEntity.getLevel();
    if (level == null) {
      renderState.entityRenderState = null;
      return;
    }

    BaseSpawner baseSpawner = blockEntity.getSpawner();
    Entity entity = baseSpawner.getOrCreateDisplayEntity(level, blockEntity.getBlockPos());
    if (entity == null) {
      renderState.entityRenderState = null;
      return;
    }

    // Calculate scale based on entity dimensions
    float scale = 0.53125F;
    float maxDimension = Math.max(entity.getBbWidth(), entity.getBbHeight());
    if (maxDimension > 1.0F) {
      scale /= maxDimension;
    }
    renderState.scale = scale;

    // Interpolate spin rotation
    renderState.spinInterpolated =
        (float) Mth.lerp(partialTicks, baseSpawner.getOSpin(), baseSpawner.getSpin()) * 10.0F;

    // Extract entity render state
    try {
      @SuppressWarnings("unchecked")
      EntityRenderer<Entity, EntityRenderState> renderer =
          (EntityRenderer<Entity, EntityRenderState>) this.entityRenderer.getRenderer(entity);
      EntityRenderState entityRenderState = renderer.createRenderState(entity, partialTicks);
      renderer.extractRenderState(entity, entityRenderState, partialTicks);

      // Transfer light coords from block entity render state to entity render state
      entityRenderState.lightCoords = renderState.lightCoords;
      renderState.entityRenderState = entityRenderState;
    } catch (Exception exception) {
      renderState.entityRenderState = null;
    }
  }

  @Override
  public void submit(
      EasyNPCSpawnerRenderState renderState,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState) {
    if (renderState.entityRenderState == null) {
      return;
    }

    poseStack.pushPose();
    poseStack.translate(0.5F, 0.0F, 0.5F);
    poseStack.translate(0.0F, 0.4F, 0.0F);
    poseStack.mulPose(Axis.YP.rotationDegrees(renderState.spinInterpolated));
    poseStack.translate(0.0F, -0.2F, 0.0F);
    poseStack.mulPose(Axis.XP.rotationDegrees(-30.0F));
    poseStack.scale(renderState.scale, renderState.scale, renderState.scale);
    this.entityRenderer.submit(
        renderState.entityRenderState,
        cameraRenderState,
        0.0,
        0.0,
        0.0,
        poseStack,
        submitNodeCollector);
    poseStack.popPose();
  }
}
