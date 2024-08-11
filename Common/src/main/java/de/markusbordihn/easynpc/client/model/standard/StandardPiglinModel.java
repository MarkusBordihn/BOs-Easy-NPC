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

import de.markusbordihn.easynpc.client.model.ModelPartType;
import de.markusbordihn.easynpc.client.model.base.BasePlayerModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.LivingEntity;

public class StandardPiglinModel<T extends LivingEntity> extends BasePlayerModel<T> {

  private final ModelPart rightEar;
  private final ModelPart leftEar;

  public StandardPiglinModel(ModelPart modelPart) {
    super(modelPart);
    this.rightEar = defineModelPart(ModelPartType.RIGHT_EAR, this.head, "right_ear");
    this.leftEar = defineModelPart(ModelPartType.LEFT_EAR, this.head, "left_ear");
  }

  @Override
  public void setupAnim(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
    float earSwing = ageInTicks * 0.1F + limbSwing * 0.5F;
    float earSwingAmount = 0.08F + limbSwingAmount * 0.4F;
    if (this.rightEar != null) {
      this.rightEar.zRot = (float) (Math.PI / 6) + Mth.cos(earSwing) * earSwingAmount;
    }
    if (this.leftEar != null) {
      this.leftEar.zRot = (float) (-Math.PI / 6) - Mth.cos(earSwing * 1.2F) * earSwingAmount;
    }
  }
}
