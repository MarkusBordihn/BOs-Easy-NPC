/*
 * Copyright 2025 Markus Bordihn
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
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import java.util.function.Function;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.entity.state.HumanoidRenderState;
import net.minecraft.resources.ResourceLocation;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(HumanoidModel.class)
public class EasyNPCHumanoidModelMixin<T extends HumanoidRenderState> {

  @Shadow @Final public ModelPart head;
  @Shadow @Final public ModelPart hat;
  @Shadow @Final public ModelPart body;
  @Shadow @Final public ModelPart rightArm;
  @Shadow @Final public ModelPart leftArm;
  @Shadow @Final public ModelPart rightLeg;
  @Shadow @Final public ModelPart leftLeg;

  @Unique private EasyNPCModelManager easyNPC$modelManager;

  @Inject(
      method = "<init>(Lnet/minecraft/client/model/geom/ModelPart;Ljava/util/function/Function;)V",
      at = @At("TAIL"))
  private void easyNpcModel(
      final ModelPart modelPart,
      final Function<ResourceLocation, RenderType> renderType,
      final CallbackInfo callbackInfo) {
    this.easyNPC$modelManager =
        new EasyNPCModelManager(modelPart, renderType)
            .defineModelPart(ModelPartType.HAT, this.hat)
            .defineModelPart(ModelPartType.HEAD, this.head)
            .defineModelPart(ModelPartType.BODY, this.body)
            .defineModelPart(ModelPartType.RIGHT_ARM, this.rightArm)
            .defineModelPart(ModelPartType.LEFT_ARM, this.leftArm)
            .defineModelPart(ModelPartType.RIGHT_LEG, this.rightLeg)
            .defineModelPart(ModelPartType.LEFT_LEG, this.leftLeg);
  }

  @Inject(
      method = "setupAnim(Lnet/minecraft/client/renderer/entity/state/HumanoidRenderState;)V",
      at = @At("HEAD"),
      cancellable = true)
  private void setupNpcAnimStart(T renderState, CallbackInfo callbackInfo) {
    if (renderState instanceof EasyNPCRenderStateExtension extension
        && EasyNPCModel.setupAnimationStart(extension, this.easyNPC$modelManager)) {
      callbackInfo.cancel();
    }
  }

  @Inject(
      method = "setupAnim(Lnet/minecraft/client/renderer/entity/state/HumanoidRenderState;)V",
      at = @At("TAIL"))
  private void setupNpcAnimEnd(T renderState, CallbackInfo callbackInfo) {
    if (renderState instanceof EasyNPCRenderStateExtension extension) {
      EasyNPCModel.setupAnimationEnd(extension, this.easyNPC$modelManager);
    }
  }
}
