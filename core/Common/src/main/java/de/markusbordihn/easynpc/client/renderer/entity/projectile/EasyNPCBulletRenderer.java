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
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.attack.ProjectileType;
import de.markusbordihn.easynpc.entity.projectile.EasyNPCBullet;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.model.object.projectile.TridentModel;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.ArrowRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.SpectralArrowRenderer;
import net.minecraft.client.renderer.entity.ThrownTridentRenderer;
import net.minecraft.client.renderer.entity.TippableArrowRenderer;
import net.minecraft.client.renderer.state.level.CameraRenderState;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.resources.Identifier;
import net.minecraft.util.Unit;

public class EasyNPCBulletRenderer extends ArrowRenderer<EasyNPCBullet, EasyNPCBulletRenderState> {

  public static final Identifier TEXTURE_LOCATION =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "textures/entity/projectile/bullet.png");

  private final TridentModel tridentModel;

  public EasyNPCBulletRenderer(EntityRendererProvider.Context context) {
    super(context);
    this.tridentModel = new TridentModel(context.bakeLayer(ModelLayers.TRIDENT));
  }

  @Override
  public EasyNPCBulletRenderState createRenderState() {
    return new EasyNPCBulletRenderState();
  }

  @Override
  public void extractRenderState(
      EasyNPCBullet bullet, EasyNPCBulletRenderState renderState, float partialTick) {
    super.extractRenderState(bullet, renderState, partialTick);
    renderState.projectileType = bullet.getProjectileType();
  }

  @Override
  public void submit(
      EasyNPCBulletRenderState renderState,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState) {
    if (renderState.projectileType != ProjectileType.TRIDENT) {
      super.submit(renderState, poseStack, submitNodeCollector, cameraRenderState);
      return;
    }

    poseStack.pushPose();
    poseStack.rotateDegrees(Axis.YP, renderState.yRot - 90.0F);
    poseStack.rotateDegrees(Axis.ZP, renderState.xRot + 90.0F);
    submitNodeCollector.submitModel(
        this.tridentModel,
        Unit.INSTANCE,
        poseStack,
        this.tridentModel.renderType(ThrownTridentRenderer.TRIDENT_LOCATION),
        renderState.lightCoords,
        OverlayTexture.NO_OVERLAY,
        renderState.outlineColor);
    poseStack.popPose();
  }

  @Override
  protected Identifier getTextureLocation(EasyNPCBulletRenderState renderState) {
    return switch (renderState.projectileType) {
      case ARROW, FIRE_ARROW -> TippableArrowRenderer.NORMAL_ARROW_LOCATION;
      case SPECTRAL_ARROW -> SpectralArrowRenderer.SPECTRAL_ARROW_LOCATION;
      case TRIDENT -> ThrownTridentRenderer.TRIDENT_LOCATION;
      default -> TEXTURE_LOCATION;
    };
  }
}
