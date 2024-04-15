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

package de.markusbordihn.easynpc.client.renderer.entity.standard;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Vector3f;
import de.markusbordihn.easynpc.client.model.standard.StandardZombieModel;
import de.markusbordihn.easynpc.client.renderer.EasyNPCModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.base.BaseHumanoidMobModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.OuterLayer;
import de.markusbordihn.easynpc.entity.easynpc.npc.Zombie;
import de.markusbordihn.easynpc.entity.easynpc.npc.Zombie.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.layers.CustomHeadLayer;
import net.minecraft.client.renderer.entity.layers.ElytraLayer;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;

public class ZombieModelRenderer
    extends BaseHumanoidMobModelRenderer<Zombie, Variant, StandardZombieModel<Zombie>> {

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(Variant.DROWNED, new ResourceLocation("textures/entity/zombie/drowned.png"));
            map.put(Variant.HUSK, new ResourceLocation("textures/entity/zombie/husk.png"));
            map.put(Variant.ZOMBIE, new ResourceLocation("textures/entity/zombie/zombie.png"));
          });
  protected static final Map<Variant, ResourceLocation> OUTER_TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(
                Variant.DROWNED,
                new ResourceLocation("textures/entity/zombie/drowned_outer_layer.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE = TEXTURE_BY_VARIANT.get(Variant.ZOMBIE);

  public <T extends RenderLayer<Zombie, StandardZombieModel<Zombie>>> ZombieModelRenderer(
      EntityRendererProvider.Context context, Class<T> humanoidArmorLayerClass) {
    super(
        context,
        new StandardZombieModel<>(context.bakeLayer(ModelLayers.ZOMBIE)),
        0.5F,
        DEFAULT_TEXTURE,
        TEXTURE_BY_VARIANT);
    this.addLayer(
        EasyNPCModelRenderer.getHumanoidArmorLayer(
            this,
            context,
            ModelLayers.ZOMBIE_INNER_ARMOR,
            ModelLayers.ZOMBIE_OUTER_ARMOR,
            humanoidArmorLayerClass));
    this.addLayer(
        new CustomHeadLayer<>(this, context.getModelSet(), context.getItemInHandRenderer()));
    this.addLayer(new OuterLayer<>(this, context.getModelSet(), OUTER_TEXTURE_BY_VARIANT));
    this.addLayer(new ElytraLayer<>(this, context.getModelSet()));
  }

  @Override
  public void renderDefaultPose(
      Zombie entity,
      StandardZombieModel<Zombie> playerModel,
      Pose pose,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      net.minecraft.client.renderer.MultiBufferSource buffer,
      int light) {
    switch (pose) {
      case DYING:
        poseStack.translate(-1.0D, 0.0D, 0.0D);
        poseStack.mulPose(Vector3f.YP.rotationDegrees(180f));
        poseStack.mulPose(Vector3f.ZP.rotationDegrees(this.getFlipDegrees(entity)));
        poseStack.mulPose(Vector3f.YP.rotationDegrees(270.0f));
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
        poseStack.mulPose(Vector3f.YP.rotationDegrees(-35f));
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
}
