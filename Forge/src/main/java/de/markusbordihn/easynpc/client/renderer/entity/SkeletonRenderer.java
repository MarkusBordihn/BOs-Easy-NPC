/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.client.renderer.entity;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.client.model.custom.CustomSkeletonModel;
import de.markusbordihn.easynpc.client.renderer.entity.layers.CustomHumanoidArmorLayer;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.npc.Skeleton;
import de.markusbordihn.easynpc.entity.npc.Skeleton.Variant;
import java.util.EnumMap;
import java.util.Map;
import javax.annotation.Nonnull;
import net.minecraft.Util;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.layers.ElytraLayer;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class SkeletonRenderer extends HumanoidMobRenderer<Skeleton, CustomSkeletonModel<Skeleton>>
    implements EasyNPCRenderer {

  // Variant Textures
  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(
                Variant.SKELETON, new ResourceLocation("textures/entity/skeleton/skeleton.png"));
            map.put(Variant.STRAY, new ResourceLocation("textures/entity/skeleton/stray.png"));
            map.put(
                Variant.WITHER_SKELETON,
                new ResourceLocation("textures/entity/skeleton/wither_skeleton.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT.get(Variant.SKELETON);

  public SkeletonRenderer(EntityRendererProvider.Context context) {
    super(context, new CustomSkeletonModel<>(context.bakeLayer(ModelLayers.SKELETON)), 0.5F);
    this.addLayer(
        new CustomHumanoidArmorLayer<>(
            this,
            new CustomSkeletonModel<>(context.bakeLayer(ModelLayers.SKELETON_INNER_ARMOR)),
            new CustomSkeletonModel<>(context.bakeLayer(ModelLayers.SKELETON_OUTER_ARMOR)),
            context.getModelManager()));
    this.addLayer(new ItemInHandLayer<>(this, context.getItemInHandRenderer()));
    this.addLayer(new ElytraLayer<>(this, context.getModelSet()));
  }

  @Override
  public ResourceLocation getTextureByVariant(Enum<?> variant) {
    return TEXTURE_BY_VARIANT.getOrDefault(variant, DEFAULT_TEXTURE);
  }

  @Override
  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }

  @Override
  public ResourceLocation getTextureLocation(Skeleton entity) {
    return this.getEntityTexture(entity);
  }

  @Override
  protected void scale(Skeleton entity, PoseStack poseStack, float unused) {
    this.scaleEntity(entity, poseStack);
  }

  @Override
  public void render(
      Skeleton entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      net.minecraft.client.renderer.MultiBufferSource buffer,
      int light) {
    CustomSkeletonModel<Skeleton> playerModel = this.getModel();

    // Model Rotation
    this.rotateEntity(entity, poseStack);

    // Render additional poses
    if (entity.getModelPose() == ModelPose.DEFAULT) {
      switch (entity.getPose()) {
        case DYING:
          poseStack.translate(-1.0D, 0.0D, 0.0D);
          poseStack.mulPose(Axis.YP.rotationDegrees(180f));
          poseStack.mulPose(Axis.ZP.rotationDegrees(this.getFlipDegrees(entity)));
          poseStack.mulPose(Axis.YP.rotationDegrees(270.0F));
          playerModel.getHead().xRot = -0.7853982F;
          playerModel.getHead().yRot = -0.7853982F;
          playerModel.getHead().zRot = -0.7853982F;
          break;
        case LONG_JUMPING:
          playerModel.leftArmPose = HumanoidModel.ArmPose.CROSSBOW_HOLD;
          playerModel.rightArmPose = HumanoidModel.ArmPose.SPYGLASS;
          break;
        case SLEEPING:
          poseStack.translate(1.0D, 0.0D, 0.0D);
          break;
        case SPIN_ATTACK:
          playerModel.leftArmPose = HumanoidModel.ArmPose.BLOCK;
          playerModel.rightArmPose = HumanoidModel.ArmPose.THROW_SPEAR;
          poseStack.mulPose(Axis.YP.rotationDegrees(-35f));
          break;
        default:
          playerModel.leftArmPose = HumanoidModel.ArmPose.EMPTY;
          playerModel.rightArmPose = HumanoidModel.ArmPose.EMPTY;
          playerModel.getHead().xRot = 0F;
          playerModel.getHead().yRot = 0F;
          playerModel.getHead().zRot = 0F;
          break;
      }
    }

    super.render(entity, entityYaw, partialTicks, poseStack, buffer, light);
  }

  @Override
  protected void renderNameTag(
      Skeleton entity,
      Component component,
      PoseStack poseStack,
      MultiBufferSource multiBufferSource,
      int color) {
    this.renderEntityNameTag(entity, poseStack);
    super.renderNameTag(entity, component, poseStack, multiBufferSource, color);
  }

  @Override
  protected int getBlockLightLevel(@Nonnull Skeleton entity, @Nonnull BlockPos blockPos) {
    return getEntityLightLevel(entity, blockPos);
  }
}
