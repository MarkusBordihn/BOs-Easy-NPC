/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.easynpc.client.model.standard;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.ModelHelper;
import de.markusbordihn.easynpc.client.model.base.BaseHierarchicalArmModel;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.model.geom.PartPose;
import net.minecraft.client.model.geom.builders.CubeDeformation;
import net.minecraft.client.model.geom.builders.CubeListBuilder;
import net.minecraft.client.model.geom.builders.LayerDefinition;
import net.minecraft.client.model.geom.builders.MeshDefinition;
import net.minecraft.client.model.geom.builders.PartDefinition;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.Mob;

public class StandardAllayModel<T extends Mob> extends BaseHierarchicalArmModel<T> {

  private final ModelPart rightWing;
  private final ModelPart leftWing;

  public StandardAllayModel(ModelPart modelPart) {
    super(modelPart);
    this.rightWing = this.body.getChild("right_wing");
    this.leftWing = this.body.getChild("left_wing");
  }

  public static LayerDefinition createBodyLayer() {
    MeshDefinition meshDefinition = new MeshDefinition();
    PartDefinition partDefinition = meshDefinition.getRoot();
    PartDefinition rootDefinition =
        partDefinition.addOrReplaceChild(
            "root", CubeListBuilder.create(), PartPose.offset(0.0F, 23.5F, 0.0F));
    rootDefinition.addOrReplaceChild(
        "head",
        CubeListBuilder.create()
            .texOffs(0, 0)
            .addBox(-2.5F, -5.0F, -2.5F, 5.0F, 5.0F, 5.0F, new CubeDeformation(0.0F)),
        PartPose.offset(0.0F, -3.99F, 0.0F));
    PartDefinition bodyDefinition =
        rootDefinition.addOrReplaceChild(
            "body",
            CubeListBuilder.create()
                .texOffs(0, 10)
                .addBox(-1.5F, 0.0F, -1.0F, 3.0F, 4.0F, 2.0F, new CubeDeformation(0.0F))
                .texOffs(0, 16)
                .addBox(-1.5F, 0.0F, -1.0F, 3.0F, 5.0F, 2.0F, new CubeDeformation(-0.2F)),
            PartPose.offset(0.0F, -4.0F, 0.0F));
    bodyDefinition.addOrReplaceChild(
        "right_arm",
        CubeListBuilder.create()
            .texOffs(23, 0)
            .addBox(-0.75F, -0.5F, -1.0F, 1.0F, 4.0F, 2.0F, new CubeDeformation(-0.01F)),
        PartPose.offset(-1.75F, 0.5F, 0.0F));
    bodyDefinition.addOrReplaceChild(
        "left_arm",
        CubeListBuilder.create()
            .texOffs(23, 6)
            .addBox(-0.25F, -0.5F, -1.0F, 1.0F, 4.0F, 2.0F, new CubeDeformation(-0.01F)),
        PartPose.offset(1.75F, 0.5F, 0.0F));
    bodyDefinition.addOrReplaceChild(
        "right_wing",
        CubeListBuilder.create()
            .texOffs(16, 14)
            .addBox(0.0F, 1.0F, 0.0F, 0.0F, 5.0F, 8.0F, new CubeDeformation(0.0F)),
        PartPose.offset(-0.5F, 0.0F, 0.6F));
    bodyDefinition.addOrReplaceChild(
        "left_wing",
        CubeListBuilder.create()
            .texOffs(16, 14)
            .addBox(0.0F, 1.0F, 0.0F, 0.0F, 5.0F, 8.0F, new CubeDeformation(0.0F)),
        PartPose.offset(0.5F, 0.0F, 0.6F));
    return LayerDefinition.create(meshDefinition, 32, 32);
  }

  @Override
  public boolean setupCrouchingModelPose(
      T entity,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    this.body.xRot = 0.5F;
    this.body.y += 1.2F;
    this.head.y += 2.2F;
    this.leftArm.xRot += 0.4F;
    this.leftArm.y = 2.2F;
    this.rightArm.xRot += 0.4F;
    this.rightArm.y = 2.2F;
    return true;
  }

  @Override
  public void setupAnim(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    // Change root position so that the model is floating in the air.
    ModelHelper.setPosition(this.root, 0, 18, 0);

    super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
  }

  @Override
  public boolean animateModelArms(
      T entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    if (rightArmPart == null || leftArmPart == null) {
      return false;
    }
    float ageAmount = ageInTicks * 9.0F * Constants.PI_180DEG;
    float armRotationAmount =
        0.43633232F
            - Mth.cos(ageAmount + ((float) Math.PI * 1.5F)) * (float) Math.PI * 0.075F * 1.0F
            - limbSwingAmount;
    this.rightArm.xRot =
        Mth.lerp(
            1f,
            limbSwingAmount * 0.6981317F,
            Mth.lerp(limbSwingAmount, (-(float) Math.PI / 3F), (-(float) Math.PI / 4F)));
    this.leftArm.xRot = this.rightArm.xRot;
    this.leftArm.zRot = -armRotationAmount;
    this.rightArm.zRot = armRotationAmount;
    this.rightArm.yRot = 0.27925268F;
    this.leftArm.yRot = -0.27925268F;
    return true;
  }

  @Override
  public boolean animateModelBody(
      T entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart bodyPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    float ageAmount = ageInTicks * 9.0F * Constants.PI_180DEG;
    this.body.xRot = limbSwingAmount * 0.6981317F;
    this.root.y += (float) Math.cos(ageAmount) * 0.25F * 1.0F - limbSwingAmount;
    return true;
  }

  @Override
  public boolean additionalModelAnimation(
      T entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    float wingRotationAmount =
        Mth.cos(ageInTicks * 20.0F * Constants.PI_180DEG + limbSwingAmount)
            * (float) Math.PI
            * 0.15F;
    this.rightWing.xRot = 0.43633232F;
    this.rightWing.yRot = -0.61086524F + wingRotationAmount;
    this.leftWing.xRot = 0.43633232F;
    this.leftWing.yRot = 0.61086524F - wingRotationAmount;
    return true;
  }

  public ModelPart getHead() {
    return this.head;
  }

  @Override
  public void translateToHand(HumanoidArm humanoidArm, PoseStack poseStack) {
    this.root.translateAndRotate(poseStack);
    this.body.translateAndRotate(poseStack);
    poseStack.translate(0.0F, 0.0625F, 0.1875F);
    poseStack.mulPose(Axis.XP.rotation(this.rightArm.xRot));
    poseStack.scale(0.7F, 0.7F, 0.7F);
    poseStack.translate(0.0625D, 0.0D, 0.0D);
  }

  @Override
  public void renderToBuffer(
      PoseStack poseStack, VertexConsumer vertexConsumer, int lightLevel, int overlay, int color) {
    this.root.render(poseStack, vertexConsumer, lightLevel, overlay, color);
  }
}
