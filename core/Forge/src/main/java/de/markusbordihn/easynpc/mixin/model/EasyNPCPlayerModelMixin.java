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

import de.markusbordihn.easynpc.client.model.EasyNPCModelManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.LivingEntity;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(PlayerModel.class)
public abstract class EasyNPCPlayerModelMixin<T extends LivingEntity>
    extends EasyNPCHumanoidModelMixin<T> {

  @Shadow @Final public ModelPart leftSleeve;
  @Shadow @Final public ModelPart rightSleeve;
  @Shadow @Final public ModelPart leftPants;
  @Shadow @Final public ModelPart rightPants;
  @Shadow @Final public ModelPart jacket;

  @Inject(method = "<init>(Lnet/minecraft/client/model/geom/ModelPart;Z)V", at = @At("RETURN"))
  private void easyNpcPlayerModel(ModelPart modelPart, boolean slim, CallbackInfo callbackInfo) {
    EasyNPCModelManager modelManager = this.easyNPC$getModelManager();
    if (modelManager != null) {
      modelManager
          .defineModelPart(ModelPartType.LEFT_SLEEVE, this.leftSleeve)
          .defineModelPart(ModelPartType.RIGHT_SLEEVE, this.rightSleeve)
          .defineModelPart(ModelPartType.LEFT_PANTS, this.leftPants)
          .defineModelPart(ModelPartType.RIGHT_PANTS, this.rightPants)
          .defineModelPart(ModelPartType.BODY_JACKET, this.jacket);
    }
  }
}
