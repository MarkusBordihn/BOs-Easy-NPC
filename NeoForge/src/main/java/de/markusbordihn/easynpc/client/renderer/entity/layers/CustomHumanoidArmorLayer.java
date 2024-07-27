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

package de.markusbordihn.easynpc.client.renderer.entity.layers;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.resources.model.ModelManager;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;

public class CustomHumanoidArmorLayer<
        T extends LivingEntity, M extends HumanoidModel<T>, A extends HumanoidModel<T>>
    extends HumanoidArmorLayer<T, M, A> {

  public CustomHumanoidArmorLayer(
      RenderLayerParent<T, M> renderer, A innerModel, A outerModel, ModelManager modelManager) {
    super(renderer, innerModel, outerModel, modelManager);
  }

  @Override
  protected void renderArmorPiece(
      PoseStack poseStack,
      MultiBufferSource multiBufferSource,
      T entity,
      EquipmentSlot equipmentSlot,
      int light,
      A model) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      ModelData<?> modelData = easyNPC.getEasyNPCModelData();
      if (modelData != null
          && ((equipmentSlot == EquipmentSlot.CHEST && modelData.isModelChestplateVisible())
              || (equipmentSlot == EquipmentSlot.LEGS && modelData.isModelLeggingsVisible())
              || (equipmentSlot == EquipmentSlot.FEET && modelData.isModelBootsVisible())
              || (equipmentSlot == EquipmentSlot.HEAD && modelData.isModelHelmetVisible()))) {
        super.renderArmorPiece(poseStack, multiBufferSource, entity, equipmentSlot, light, model);
      }
    }
  }
}
