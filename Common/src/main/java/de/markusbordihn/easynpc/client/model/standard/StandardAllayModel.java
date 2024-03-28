/*
 * Copyright 2023 Markus Bordihn
 *
 * <p>Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * <p>The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * <p>THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.easynpc.client.model.standard;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.mojang.math.Vector3f;
import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.ModelHelper;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.client.model.ArmedModel;
import net.minecraft.client.model.HierarchicalModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.model.geom.PartPose;
import net.minecraft.client.model.geom.builders.CubeDeformation;
import net.minecraft.client.model.geom.builders.CubeListBuilder;
import net.minecraft.client.model.geom.builders.LayerDefinition;
import net.minecraft.client.model.geom.builders.MeshDefinition;
import net.minecraft.client.model.geom.builders.PartDefinition;
import net.minecraft.core.Rotations;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Pose;

public class StandardAllayModel<T extends LivingEntity> extends HierarchicalModel<T>
    implements ArmedModel, EasyNPCModel {

  protected final ModelPart head;
  private final ModelPart root;
  private final ModelPart body;
  private final ModelPart rightArm;
  private final ModelPart leftArm;
  private final ModelPart rightWing;
  private final ModelPart leftWing;

  public StandardAllayModel(ModelPart modelPart) {
    this.root = modelPart.getChild("root");
    this.head = this.root.getChild("head");
    this.body = this.root.getChild("body");
    this.rightArm = this.body.getChild("right_arm");
    this.leftArm = this.body.getChild("left_arm");
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

  public ModelPart root() {
    return this.root;
  }

  public ModelPart getHead() {
    return this.head;
  }

  @Override
  public Rotations getDefaultModelLeftArmRotation() {
    return ALLAY_MODEL_LEFT_ARM_ROTATION;
  }

  @Override
  public Rotations getDefaultModelRightArmRotation() {
    return ALLAY_MODEL_RIGHT_ARM_ROTATION;
  }

  @Override
  public CustomPosition getDefaultModelRootPosition() {
    return ALLAY_MODEL_ROOT_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelLeftArmPosition() {
    return ALLAY_MODEL_LEFT_ARM_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelRightArmPosition() {
    return ALLAY_MODEL_RIGHT_ARM_POSITION;
  }

  @Override
  public void setupAnim(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return;
    }
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();

    // Reset player model to avoid any issues with other mods.
    EasyNPCModel.resetArmModel(this, head, body, leftArm, rightArm);

    // Change root position so that the model is floating in the air.
    ModelHelper.setPosition(this.root, 0, 15, 0);

    // Individual Part Modifications
    if (modelData.getModelPose() == ModelPose.CUSTOM) {
      EasyNPCModel.setupArmModel(easyNPC, head, body, leftArm, rightArm, netHeadYaw, headPitch);
    } else if (modelData.getDefaultPose() == Pose.CROUCHING) {
      this.body.xRot = 0.5F;
      this.body.y = 3.2F;
      this.head.y = 4.2F;
      this.leftArm.xRot += 0.4F;
      this.leftArm.y = 2.2F;
      this.rightArm.xRot += 0.4F;
      this.rightArm.y = 2.2F;
    } else {
      // Body animations
      float ageAmount = ageInTicks * 9.0F * ((float) Math.PI / 180F);
      float limbSwingRotation = Math.min(limbSwingAmount / 0.3F, 1.0F);
      float bodyRotationAmount = limbSwingRotation * 0.6981317F;
      this.body.xRot = bodyRotationAmount;
      this.root.y += (float) Math.cos(ageAmount) * 0.25F * 1.0F - limbSwingRotation;

      // Arm animations
      float armRotationAmount =
          0.43633232F
              - Mth.cos(ageAmount + ((float) Math.PI * 1.5F)) * (float) Math.PI * 0.075F * 1.0F
              - limbSwingRotation;
      this.rightArm.xRot =
          Mth.lerp(
              1f,
              bodyRotationAmount,
              Mth.lerp(limbSwingRotation, (-(float) Math.PI / 3F), (-(float) Math.PI / 4F)));
      this.leftArm.xRot = this.rightArm.xRot;
      this.leftArm.zRot = -armRotationAmount;
      this.rightArm.zRot = armRotationAmount;
      this.rightArm.yRot = 0.27925268F;
      this.leftArm.yRot = -0.27925268F;
    }

    // Wing animations
    float wingRotationAmount =
        Mth.cos(ageInTicks * 20.0F * ((float) Math.PI / 180F) + limbSwingAmount)
            * (float) Math.PI
            * 0.15F;
    this.rightWing.xRot = 0.43633232F;
    this.rightWing.yRot = -0.61086524F + wingRotationAmount;
    this.leftWing.xRot = 0.43633232F;
    this.leftWing.yRot = 0.61086524F - wingRotationAmount;
  }

  protected ModelPart getArm(HumanoidArm humanoidArm) {
    return humanoidArm == HumanoidArm.LEFT ? this.leftArm : this.rightArm;
  }

  public void translateToHand(HumanoidArm humanoidArm, PoseStack poseStack) {
    this.root.translateAndRotate(poseStack);
    this.body.translateAndRotate(poseStack);
    poseStack.translate(0.0F, 0.0625F, 0.1875F);
    poseStack.mulPose(Vector3f.XP.rotation(this.rightArm.xRot));
    poseStack.scale(0.7F, 0.7F, 0.7F);
    poseStack.translate(0.0625D, 0.0D, 0.0D);
  }

  @Override
  public void renderToBuffer(
      PoseStack poseStack,
      VertexConsumer vertexConsumer,
      int lightLevel,
      int overlay,
      float unused1,
      float unused2,
      float unused3,
      float unused4) {
    this.root.render(poseStack, vertexConsumer, lightLevel, overlay);
  }
}
