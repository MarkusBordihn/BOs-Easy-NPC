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
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.client.model.VillagerModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Pose;

public class StandardVillagerModel<T extends Entity> extends VillagerModel<T>
    implements EasyNPCModel {

  private final ModelPart arms;
  private final ModelPart body;
  private final ModelPart head;
  private final ModelPart leftLeg;
  private final ModelPart rightLeg;

  public StandardVillagerModel(ModelPart modelPart) {
    super(modelPart);
    this.arms = modelPart.getChild("arms");
    this.body = modelPart.getChild("body");
    this.head = modelPart.getChild("head");
    this.leftLeg = modelPart.getChild("left_leg");
    this.rightLeg = modelPart.getChild("right_leg");
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
    EasyNPCModel.resetVillagerModel(
        this, this.head, this.body, this.arms, this.leftLeg, this.rightLeg);

    // Individual Part Modifications
    if (modelData.getModelPose() == ModelPose.CUSTOM) {
      EasyNPCModel.setupVillagerModel(
          easyNPC,
          this.head,
          this.body,
          this.arms,
          this.leftLeg,
          this.rightLeg,
          netHeadYaw,
          headPitch);
    } else if (modelData.getDefaultPose() == Pose.CROUCHING) {
      // Crouching Pose
      this.arms.xRot += 0.4F;
      this.arms.y = 5.2F;
      this.body.xRot = 0.5F;
      this.body.y = 3.2F;
      this.head.y = 4.2F;
      this.leftLeg.y = 12.2F;
      this.leftLeg.z = 4.0F;
      this.rightLeg.y = 12.2F;
      this.rightLeg.z = 4.0F;
    } else {
      super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
    }
  }
}
