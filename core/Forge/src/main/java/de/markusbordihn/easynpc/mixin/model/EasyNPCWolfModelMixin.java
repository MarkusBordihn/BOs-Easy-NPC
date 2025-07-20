package de.markusbordihn.easynpc.mixin.model;

import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.EasyNPCModelManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.model.WolfModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.animal.Wolf;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(WolfModel.class)
public class EasyNPCWolfModelMixin<T extends Wolf> {

  @Shadow @Final private ModelPart head;
  @Shadow @Final private ModelPart body;
  @Shadow @Final private ModelPart rightFrontLeg;
  @Shadow @Final private ModelPart leftFrontLeg;
  @Shadow @Final private ModelPart rightHindLeg;
  @Shadow @Final private ModelPart leftHindLeg;
  @Shadow @Final private ModelPart tail;

  @Unique private EasyNPCModelManager easyNPCModelManager;

  @Inject(method = "<init>(Lnet/minecraft/client/model/geom/ModelPart;)V", at = @At("TAIL"))
  private void easyNpcModel(ModelPart modelPart, CallbackInfo callbackInfo) {
    this.easyNPCModelManager =
        new EasyNPCModelManager(modelPart)
            .defineModelPart(ModelPartType.HEAD, this.head)
            .defineModelPart(ModelPartType.BODY, this.body)
            .defineModelPart(ModelPartType.RIGHT_FRONT_LEG, this.rightFrontLeg)
            .defineModelPart(ModelPartType.LEFT_FRONT_LEG, this.leftFrontLeg)
            .defineModelPart(ModelPartType.RIGHT_HIND_LEG, this.rightHindLeg)
            .defineModelPart(ModelPartType.LEFT_HIND_LEG, this.leftHindLeg);
  }

  @Inject(
      method = "setupAnim(Lnet/minecraft/world/entity/animal/Wolf;FFFFF)V",
      at = @At("HEAD"),
      cancellable = true)
  private void setupNpcAnim(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch,
      CallbackInfo callbackInfo) {
    if (entity instanceof EasyNPC<?> easyNPC
        && EasyNPCModel.setupAnimationStart(easyNPC, this.easyNPCModelManager)) {
      callbackInfo.cancel();
    }
  }
}
