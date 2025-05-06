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
      method = "setupAnim(Lnet/minecraft/client/renderer/entity/state/HumanoidRenderState;)V",
      at = @At("HEAD"),
      cancellable = true)
  private void setupNpcAnim(T renderState, CallbackInfo callbackInfo) {
    if (renderState instanceof EasyNPCRenderStateExtension extension
        && EasyNPCModel.setupAnimationStart(extension, this.modelManager)) {
      callbackInfo.cancel();
    }
  }
}
