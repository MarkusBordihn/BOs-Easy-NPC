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

package de.markusbordihn.easynpc.mixin.model;

import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.EasyNPCModelManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.client.model.CatModel;
import net.minecraft.client.model.OcelotModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.animal.Cat;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(CatModel.class)
public class EasyNPCCatModelMixin<T extends Cat> extends OcelotModel<T> {

  @Unique private EasyNPCModelManager easyNPC$modelManager;

  public EasyNPCCatModelMixin(ModelPart modelPart) {
    super(modelPart);
  }

  @Inject(method = "<init>(Lnet/minecraft/client/model/geom/ModelPart;)V", at = @At("TAIL"))
  private void easyNpcModel(ModelPart modelPart, CallbackInfo callbackInfo) {
    this.easyNPC$modelManager =
        new EasyNPCModelManager(modelPart)
            .defineModelPart(ModelPartType.HEAD, this.head)
            .defineModelPart(ModelPartType.BODY, this.body)
            .defineModelPart(ModelPartType.RIGHT_FRONT_LEG, this.rightFrontLeg)
            .defineModelPart(ModelPartType.LEFT_FRONT_LEG, this.leftFrontLeg)
            .defineModelPart(ModelPartType.RIGHT_HIND_LEG, this.rightHindLeg)
            .defineModelPart(ModelPartType.LEFT_HIND_LEG, this.leftHindLeg)
            .defineModelPart(ModelPartType.TAIL1, this.tail1)
            .defineModelPart(ModelPartType.TAIL2, this.tail2);
  }

  @Inject(
      method = "setupAnim(Lnet/minecraft/world/entity/animal/Cat;FFFFF)V",
      at = @At("HEAD"),
      cancellable = true)
  private void setupNpcAnimStart(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch,
      CallbackInfo callbackInfo) {
    if (entity instanceof EasyNPC<?> easyNPC
        && EasyNPCModel.setupAnimationStart(easyNPC, this.easyNPC$modelManager)) {
      this.easyNPCAdjustTailToBody(easyNPC);
      callbackInfo.cancel();
    }
  }

  @Inject(method = "setupAnim(Lnet/minecraft/world/entity/animal/Cat;FFFFF)V", at = @At("TAIL"))
  private void setupNpcAnimEnd(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch,
      CallbackInfo callbackInfo) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      EasyNPCModel.setupAnimationEnd(easyNPC, this.easyNPC$modelManager);
    }
  }

  @Unique
  private void easyNPCAdjustTailToBody(EasyNPC<?> easyNPC) {
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return;
    }
    CustomPosition bodyPosition = modelData.getModelPartPosition(ModelPartType.BODY);
    CustomScale bodyScale = modelData.getModelPartScale(ModelPartType.BODY);
    CustomRotation bodyRotation = modelData.getModelPartRotation(ModelPartType.BODY);

    // Check for extreme scale - hide tail if > 0.5 deviation
    if (bodyScale != null
        && bodyScale.hasChanged()
        && Math.abs(bodyScale.x() - 1.0f)
                + Math.abs(bodyScale.y() - 1.0f)
                + Math.abs(bodyScale.z() - 1.0f)
            > 0.5f) {
      this.tail1.visible = false;
      this.tail2.visible = false;
      return;
    }

    // Check for rotation - hide tail if > ~5 degrees (0.1 radians)
    if (bodyRotation != null
        && bodyRotation.hasChanged()
        && Math.abs(bodyRotation.x()) + Math.abs(bodyRotation.y()) + Math.abs(bodyRotation.z())
            > 0.1f) {
      this.tail1.visible = false;
      this.tail2.visible = false;
      return;
    }

    // Only process if body has position change
    if (bodyPosition == null || !bodyPosition.hasChanged()) {
      return;
    }

    // Tail is visible and follows body position
    this.tail1.visible = true;
    this.tail2.visible = true;
    this.tail1.x += bodyPosition.x();
    this.tail1.y += bodyPosition.y();
    this.tail1.z += bodyPosition.z();
    this.tail2.x += bodyPosition.x();
    this.tail2.y += bodyPosition.y();
    this.tail2.z += bodyPosition.z();
  }
}
