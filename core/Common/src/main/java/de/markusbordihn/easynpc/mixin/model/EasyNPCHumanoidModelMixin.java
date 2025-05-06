package de.markusbordihn.easynpc.mixin.model;

import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.EasyNPCModelManager;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.function.Function;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.HumanoidModel.ArmPose;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.LivingEntity;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(HumanoidModel.class)
public class EasyNPCHumanoidModelMixin<T extends LivingEntity> {

  @Shadow @Final public ModelPart head;
  @Shadow @Final public ModelPart hat;
  @Shadow @Final public ModelPart body;
  @Shadow @Final public ModelPart rightArm;
  @Shadow @Final public ModelPart leftArm;
  @Shadow @Final public ModelPart rightLeg;
  @Shadow @Final public ModelPart leftLeg;

  @Unique private EasyNPCModelManager modelManager;

  @Inject(
      method = "<init>(Lnet/minecraft/client/model/geom/ModelPart;Ljava/util/function/Function;)V",
      at = @At("TAIL"))
  private void easyNpcModel(
      ModelPart modelPart,
      Function<ResourceLocation, RenderType> renderType,
      CallbackInfo callbackInfo) {
    this.modelManager =
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
      method = "setupAnim(Lnet/minecraft/world/entity/LivingEntity;FFFFF)V",
      at = @At("HEAD"),
      cancellable = true)
  private void setupNpcAnim(
      T livingEntity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch,
      CallbackInfo callbackInfo) {
    if (!(livingEntity instanceof EasyNPC<?> easyNPC)) {
      return;
    }

    HumanoidModel<T> model = (HumanoidModel<T>) (Object) this;
    if (EasyNPCModel.setupAnimationStart(easyNPC, this.modelManager)) {
      callbackInfo.cancel();
    }
  }
}
