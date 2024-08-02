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

import de.markusbordihn.easynpc.client.model.base.BaseHumanoidModel;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.client.model.AnimationUtils;
import net.minecraft.client.model.VillagerHeadModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;

public class StandardZombieVillagerModel<T extends LivingEntity> extends BaseHumanoidModel<T>
    implements VillagerHeadModel {

  private final ModelPart hatRim;

  public StandardZombieVillagerModel(ModelPart modelPart) {
    super(modelPart);
    this.hatRim = this.hat.getChild("hat_rim");
  }

  @Override
  public void setupAnim(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    this.setupAnimation(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
  }

  @Override
  public void animateAttackModelPose(
      T entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    AnimationUtils.animateZombieArms(
        this.leftArm,
        this.rightArm,
        entity instanceof Mob mob && mob.isAggressive(),
        this.attackTime,
        ageInTicks);
  }

  public void hatVisible(boolean visible) {
    this.head.visible = visible;
    this.hat.visible = visible;
    this.hatRim.visible = visible;
  }
}
