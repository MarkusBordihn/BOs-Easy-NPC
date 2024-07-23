/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.easynpc.client.renderer.layers;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.resources.model.ModelManager;
import net.minecraft.world.entity.LivingEntity;

public class CustomHumanoidArmorLayer<
        T extends LivingEntity, M extends HumanoidModel<T>, A extends HumanoidModel<T>>
    extends HumanoidArmorLayer<T, M, A> {

  private EasyNPC<?> easyNPC = null;

  public CustomHumanoidArmorLayer(
      RenderLayerParent<T, M> renderer, A innerModel, A outerModel, ModelManager modelManager) {
    super(renderer, innerModel, outerModel, modelManager);
  }

  @Override
  public void render(
      PoseStack poseStack,
      MultiBufferSource buffer,
      int lightLevel,
      T livingEntity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float ageInTicks2,
      float netHeadYaw,
      float headPitch) {
    if (easyNPC == null && livingEntity instanceof EasyNPC<?> easyNPCEntity) {
      this.easyNPC = easyNPCEntity;
    }
    super.render(
        poseStack,
        buffer,
        lightLevel,
        livingEntity,
        limbSwing,
        limbSwingAmount,
        ageInTicks,
        ageInTicks2,
        netHeadYaw,
        headPitch);
  }
}
