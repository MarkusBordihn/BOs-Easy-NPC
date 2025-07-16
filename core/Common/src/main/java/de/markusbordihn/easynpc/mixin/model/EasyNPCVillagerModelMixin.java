package de.markusbordihn.easynpc.mixin.model;

import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.EasyNPCModelManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.model.VillagerModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.Entity;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(VillagerModel.class)
public class EasyNPCVillagerModelMixin<T extends Entity> {

  @Shadow @Final private ModelPart head;
  @Shadow @Final private ModelPart hat;
  @Shadow @Final private ModelPart rightLeg;
  @Shadow @Final private ModelPart leftLeg;

  @Unique private EasyNPCModelManager easyNPC$modelManager;

  @Inject(method = "<init>(Lnet/minecraft/client/model/geom/ModelPart;)V", at = @At("TAIL"))
  private void easyNpcModel(ModelPart modelPart, CallbackInfo callbackInfo) {
    this.easyNPC$modelManager =
        new EasyNPCModelManager(modelPart)
            .defineModelPart(ModelPartType.HAT, this.hat)
            .defineModelPart(ModelPartType.HEAD, this.head)
            .defineModelPart(ModelPartType.BODY, "body")
            .defineModelPart(ModelPartType.ARMS, "arms")
            .defineModelPart(ModelPartType.RIGHT_LEG, this.rightLeg)
            .defineModelPart(ModelPartType.LEFT_LEG, this.leftLeg);
  }

  @Inject(
      method = "setupAnim(Lnet/minecraft/world/entity/Entity;FFFFF)V",
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
        && EasyNPCModel.setupAnimationStart(easyNPC, this.easyNPC$modelManager)) {
      callbackInfo.cancel();
    }
  }
}
