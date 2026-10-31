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

package de.markusbordihn.easynpc.client.renderer.entity.projectile;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.data.attack.ProjectileType;
import de.markusbordihn.easynpc.entity.projectile.EasyNPCItemProjectile;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.ThrownItemRenderer;
import net.minecraft.client.renderer.entity.state.ThrownItemRenderState;
import net.minecraft.client.renderer.state.CameraRenderState;
import net.minecraft.core.BlockPos;

public class EasyNPCItemProjectileRenderer extends ThrownItemRenderer<EasyNPCItemProjectile> {

  private static final float SMALL_FIREBALL_SCALE = 0.75F;
  private static final float LARGE_FIREBALL_SCALE = 3.0F;
  private static final int FULL_BRIGHT_LIGHT_LEVEL = 15;

  public EasyNPCItemProjectileRenderer(EntityRendererProvider.Context context) {
    super(context);
  }

  private static boolean isFireball(ProjectileType projectileType) {
    return projectileType == ProjectileType.SMALL_FIREBALL
        || projectileType == ProjectileType.LARGE_FIREBALL;
  }

  @Override
  public ThrownItemRenderState createRenderState() {
    return new EasyNPCItemProjectileRenderState();
  }

  @Override
  public void extractRenderState(
      EasyNPCItemProjectile itemProjectile, ThrownItemRenderState renderState, float partialTick) {
    super.extractRenderState(itemProjectile, renderState, partialTick);
    if (renderState instanceof EasyNPCItemProjectileRenderState itemProjectileRenderState) {
      itemProjectileRenderState.scale =
          switch (itemProjectile.getProjectileType()) {
            case SMALL_FIREBALL -> SMALL_FIREBALL_SCALE;
            case LARGE_FIREBALL -> LARGE_FIREBALL_SCALE;
            default -> 1.0F;
          };
    }
  }

  @Override
  public void submit(
      ThrownItemRenderState renderState,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState) {
    float scale =
        renderState instanceof EasyNPCItemProjectileRenderState itemProjectileRenderState
            ? itemProjectileRenderState.scale
            : 1.0F;
    poseStack.pushPose();
    poseStack.scale(scale, scale, scale);
    super.submit(renderState, poseStack, submitNodeCollector, cameraRenderState);
    poseStack.popPose();
  }

  @Override
  protected int getBlockLightLevel(EasyNPCItemProjectile itemProjectile, BlockPos blockPos) {
    return isFireball(itemProjectile.getProjectileType())
        ? FULL_BRIGHT_LIGHT_LEVEL
        : super.getBlockLightLevel(itemProjectile, blockPos);
  }
}
