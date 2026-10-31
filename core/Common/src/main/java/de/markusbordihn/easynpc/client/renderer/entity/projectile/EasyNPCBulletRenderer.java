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
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.attack.ProjectileType;
import de.markusbordihn.easynpc.entity.projectile.EasyNPCBullet;
import net.minecraft.client.model.TridentModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.ArrowRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.client.renderer.entity.SpectralArrowRenderer;
import net.minecraft.client.renderer.entity.ThrownTridentRenderer;
import net.minecraft.client.renderer.entity.TippableArrowRenderer;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;

public class EasyNPCBulletRenderer extends ArrowRenderer<EasyNPCBullet> {

  public static final ResourceLocation TEXTURE_LOCATION =
      ResourceLocation.fromNamespaceAndPath(
          Constants.MOD_ID, "textures/entity/projectile/bullet.png");

  private final TridentModel tridentModel;

  public EasyNPCBulletRenderer(EntityRendererProvider.Context context) {
    super(context);
    this.tridentModel = new TridentModel(context.bakeLayer(ModelLayers.TRIDENT));
  }

  @Override
  public void render(
      EasyNPCBullet bullet,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight) {
    if (bullet.getProjectileType() != ProjectileType.TRIDENT) {
      super.render(bullet, entityYaw, partialTicks, poseStack, bufferSource, packedLight);
      return;
    }

    poseStack.pushPose();
    poseStack.mulPose(
        Axis.YP.rotationDegrees(Mth.lerp(partialTicks, bullet.yRotO, bullet.getYRot()) - 90.0F));
    poseStack.mulPose(
        Axis.ZP.rotationDegrees(Mth.lerp(partialTicks, bullet.xRotO, bullet.getXRot()) + 90.0F));
    VertexConsumer vertexConsumer =
        ItemRenderer.getFoilBufferDirect(
            bufferSource,
            this.tridentModel.renderType(ThrownTridentRenderer.TRIDENT_LOCATION),
            false,
            false);
    this.tridentModel.renderToBuffer(
        poseStack, vertexConsumer, packedLight, OverlayTexture.NO_OVERLAY);
    poseStack.popPose();
  }

  @Override
  public ResourceLocation getTextureLocation(EasyNPCBullet bullet) {
    return switch (bullet.getProjectileType()) {
      case ARROW, FIRE_ARROW -> TippableArrowRenderer.NORMAL_ARROW_LOCATION;
      case SPECTRAL_ARROW -> SpectralArrowRenderer.SPECTRAL_ARROW_LOCATION;
      case TRIDENT -> ThrownTridentRenderer.TRIDENT_LOCATION;
      default -> TEXTURE_LOCATION;
    };
  }
}
