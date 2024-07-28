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

package de.markusbordihn.easynpc.client.model.base;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.ModelPartType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;
import net.minecraft.world.entity.Entity;

public class BaseEntityModel<E extends Entity> extends EntityModel<E> implements EasyNPCModel<E> {

  protected final Map<ModelPartType, CustomPosition> modelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, Rotations> modelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);

  public BaseEntityModel() {}

  @Override
  public void setupAnim(
      E entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    this.setupAnimation(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
  }

  @Override
  public void resetModelParts() {}

  @Override
  public void setDefaultModelPartPosition(
      ModelPartType modelPartType, CustomPosition customPosition) {
    this.modelPartPositionMap.put(modelPartType, customPosition);
  }

  @Override
  public void setDefaultModelPartRotation(ModelPartType modelPartType, Rotations rotations) {
    this.modelPartRotationMap.put(modelPartType, rotations);
  }

  @Override
  public void setDefaultModelPart(ModelPartType modelPartType, ModelPart modelPart) {
    this.modelPartMap.put(modelPartType, modelPart);
  }

  @Override
  public CustomPosition getDefaultModelPartPosition(ModelPartType modelPartType) {
    return this.modelPartPositionMap.getOrDefault(modelPartType, EMPTY_POSITION);
  }

  @Override
  public Rotations getDefaultModelPartRotation(ModelPartType modelPartType) {
    return this.modelPartRotationMap.getOrDefault(modelPartType, EMPTY_ROTATION);
  }

  @Override
  public ModelPart getDefaultModelPart(ModelPartType modelPartType) {
    return this.modelPartMap.getOrDefault(modelPartType, null);
  }

  @Override
  public void renderToBuffer(
      PoseStack poseStack, VertexConsumer vertexConsumer, int lightLevel, int overlay, int color) {
    // Mo use implemented renderToBuffer method
  }
}
