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
import de.markusbordihn.easynpc.client.model.EasyNPCModelManagerAccessor;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.model.ChickenModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.Entity;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ChickenModel.class)
public class EasyNPCChickenModelMixin<T extends Entity> implements EasyNPCModelManagerAccessor {

  @Shadow @Final private ModelPart head;
  @Shadow @Final private ModelPart beak;
  @Shadow @Final private ModelPart redThing;
  @Shadow @Final private ModelPart body;
  @Shadow @Final private ModelPart rightLeg;
  @Shadow @Final private ModelPart leftLeg;
  @Shadow @Final private ModelPart rightWing;
  @Shadow @Final private ModelPart leftWing;

  @Unique private EasyNPCModelManager easyNPC$modelManager;

  @Override
  public EasyNPCModelManager easyNPC$getModelManager() {
    return this.easyNPC$modelManager;
  }

  @Inject(method = "<init>(Lnet/minecraft/client/model/geom/ModelPart;)V", at = @At("TAIL"))
  private void easyNpcModel(ModelPart modelPart, CallbackInfo callbackInfo) {
    this.easyNPC$modelManager =
        new EasyNPCModelManager(modelPart)
            .defineModelPart(ModelPartType.HEAD, this.head)
            .defineModelPart(ModelPartType.BODY, this.body)
            .defineModelPart(ModelPartType.RIGHT_WING, this.rightWing)
            .defineModelPart(ModelPartType.LEFT_WING, this.leftWing)
            .defineModelPart(ModelPartType.RIGHT_LEG, this.rightLeg)
            .defineModelPart(ModelPartType.LEFT_LEG, this.leftLeg);
  }

  @Inject(
      method = "setupAnim(Lnet/minecraft/world/entity/Entity;FFFFF)V",
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
      this.easyNPC$syncBeakToHead();
      callbackInfo.cancel();
    }
  }

  @Inject(method = "setupAnim(Lnet/minecraft/world/entity/Entity;FFFFF)V", at = @At("TAIL"))
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
      this.easyNPC$syncBeakToHead();
    }
  }

  @Unique
  private void easyNPC$syncBeakToHead() {
    this.beak.copyFrom(this.head);
    this.redThing.copyFrom(this.head);
  }
}
