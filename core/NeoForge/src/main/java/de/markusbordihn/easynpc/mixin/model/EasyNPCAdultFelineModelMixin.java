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
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.client.model.animal.feline.AdultFelineModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.entity.state.FelineRenderState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(AdultFelineModel.class)
public class EasyNPCAdultFelineModelMixin<T extends FelineRenderState> {

  @Inject(
      method = "setupAnim(Lnet/minecraft/client/renderer/entity/state/FelineRenderState;)V",
      at = @At("HEAD"),
      cancellable = true)
  private void setupNpcAnimStart(T renderState, CallbackInfo callbackInfo) {
    if (renderState instanceof EasyNPCRenderStateExtension extension) {
      EasyNPCModelManager manager =
          ((EasyNPCModelManagerAccessor) (Object) this).easyNPC$getModelManager();
      if (EasyNPCModel.setupAnimationStart(extension, manager)) {
        this.easyNPC$adjustTailToBody(extension, manager);
        callbackInfo.cancel();
      }
    }
  }

  @Inject(
      method = "setupAnim(Lnet/minecraft/client/renderer/entity/state/FelineRenderState;)V",
      at = @At("TAIL"))
  private void setupNpcAnimEnd(T renderState, CallbackInfo callbackInfo) {
    if (renderState instanceof EasyNPCRenderStateExtension extension) {
      EasyNPCModelManager manager =
          ((EasyNPCModelManagerAccessor) (Object) this).easyNPC$getModelManager();
      EasyNPCModel.setupAnimationEnd(extension, manager);
    }
  }

  @Unique
  private void easyNPC$adjustTailToBody(
      EasyNPCRenderStateExtension extension, EasyNPCModelManager manager) {
    var easyNPC = EasyNPCModel.getEasyNPC(extension);
    if (easyNPC == null) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return;
    }

    CustomPosition bodyPosition = modelData.getModelPartPosition(ModelPartType.BODY);
    CustomScale bodyScale = modelData.getModelPartScale(ModelPartType.BODY);
    CustomRotation bodyRotation = modelData.getModelPartRotation(ModelPartType.BODY);

    ModelPart tail1 = manager.getModelPart(ModelPartType.TAIL1);
    ModelPart tail2 = manager.getModelPart(ModelPartType.TAIL2);
    if (tail1 == null || tail2 == null) {
      return;
    }

    if (bodyScale != null
        && bodyScale.hasChanged()
        && Math.abs(bodyScale.x() - 1.0f)
                + Math.abs(bodyScale.y() - 1.0f)
                + Math.abs(bodyScale.z() - 1.0f)
            > 0.5f) {
      tail1.visible = false;
      tail2.visible = false;
      return;
    }

    if (bodyRotation != null
        && bodyRotation.hasChanged()
        && Math.abs(bodyRotation.x()) + Math.abs(bodyRotation.y()) + Math.abs(bodyRotation.z())
            > 0.1f) {
      tail1.visible = false;
      tail2.visible = false;
      return;
    }

    if (bodyPosition == null || !bodyPosition.hasChanged()) {
      return;
    }

    tail1.visible = true;
    tail2.visible = true;
    tail1.x += bodyPosition.x();
    tail1.y += bodyPosition.y();
    tail1.z += bodyPosition.z();
    tail2.x += bodyPosition.x();
    tail2.y += bodyPosition.y();
    tail2.z += bodyPosition.z();
  }
}
