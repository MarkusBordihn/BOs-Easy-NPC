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

package de.markusbordihn.easynpc.client.model.standard;

import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.client.model.AnimationUtils;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.VillagerHeadModel;
import net.minecraft.client.model.ZombieVillagerModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.model.geom.builders.CubeDeformation;
import net.minecraft.client.model.geom.builders.LayerDefinition;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Pose;

public class StandardZombieVillagerModel<T extends LivingEntity> extends HumanoidModel<T>
    implements EasyNPCModel, VillagerHeadModel {

  private final ModelPart hatRim;

  public StandardZombieVillagerModel(ModelPart modelPart) {
    super(modelPart);
    this.hatRim = this.hat.getChild("hat_rim");
  }

  public static LayerDefinition createBodyLayer() {
    return ZombieVillagerModel.createBodyLayer();
  }

  public static LayerDefinition createArmorLayer(CubeDeformation cubeDeformation) {
    return ZombieVillagerModel.createArmorLayer(cubeDeformation);
  }

  @Override
  public void setupAnim(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    boolean isAggressive;
    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return;
    }
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();

    // Reset player model to avoid any issues with other mods.
    EasyNPCModel.resetHumanoidModel(
        this, this.head, this.body, this.rightArm, this.leftArm, this.rightLeg, this.leftLeg);

    // Individual Part Modifications
    if (modelData.getModelPose() == ModelPose.CUSTOM) {
      EasyNPCModel.setupHumanoidModel(
          easyNPC,
          this.head,
          this.body,
          this.rightArm,
          this.leftArm,
          this.rightLeg,
          this.leftLeg,
          netHeadYaw,
          headPitch);
    } else if (modelData.getDefaultPose() == Pose.CROUCHING) {
      // Crouching Pose
      this.body.xRot = 0.5F;
      this.body.y = 3.2F;
      this.head.y = 4.2F;
      this.leftArm.xRot += 0.4F;
      this.leftArm.y = 5.2F;
      this.leftLeg.y = 12.2F;
      this.leftLeg.z = 4.0F;
      this.rightArm.xRot += 0.4F;
      this.rightArm.y = 5.2F;
      this.rightLeg.y = 12.2F;
      this.rightLeg.z = 4.0F;
    }

    if (modelData.getModelPose() == ModelPose.CUSTOM
        || modelData.getDefaultPose() == Pose.CROUCHING) {
      // Handle animations, if model specific part was not adjusted.
      if (modelData.getModelPose() == ModelPose.CUSTOM) {
        EasyNPCModel.animateHumanoidModel(
            this,
            easyNPC,
            this.head,
            this.body,
            this.rightArm,
            this.leftArm,
            this.rightLeg,
            this.leftLeg,
            ageInTicks,
            limbSwing,
            limbSwingAmount);
      }

      this.hat.copyFrom(this.head);
    } else {
      super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
      AttackData<?> attackData = easyNPC.getEasyNPCAttackData();
      AnimationUtils.animateZombieArms(
          this.leftArm, this.rightArm, attackData.isAggressive(), this.attackTime, ageInTicks);
    }
  }

  public void hatVisible(boolean visible) {
    this.head.visible = visible;
    this.hat.visible = visible;
    this.hatRim.visible = visible;
  }
}
