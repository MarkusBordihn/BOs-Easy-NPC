package de.markusbordihn.easynpc.mixin;

import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.EasyNPCModelManager;
import de.markusbordihn.easynpc.client.model.ModelPartType;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.UUID;
import net.minecraft.client.model.VillagerModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.entity.state.VillagerRenderState;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(VillagerModel.class)
public class EasyNPCVillagerModelMixin<T extends VillagerRenderState> {

  @Shadow @Final private ModelPart head;
  @Shadow @Final private ModelPart hat;
  @Shadow @Final private ModelPart rightLeg;
  @Shadow @Final private ModelPart leftLeg;

  @Unique private EasyNPCModelManager modelManager;

  @Inject(method = "<init>(Lnet/minecraft/client/model/geom/ModelPart;)V", at = @At("TAIL"))
  private void easyNpcModel(ModelPart modelPart, CallbackInfo callbackInfo) {
    this.modelManager = new EasyNPCModelManager(modelPart);
    this.modelManager.defineModelPart(ModelPartType.HAT, this.hat);
    this.modelManager.defineModelPart(ModelPartType.HEAD, this.head);
    this.modelManager.defineModelPart(ModelPartType.BODY, modelPart.getChild("body"));
    this.modelManager.defineModelPart(ModelPartType.ARMS, modelPart.getChild("arms"));
    this.modelManager.defineModelPart(ModelPartType.RIGHT_LEG, this.rightLeg);
    this.modelManager.defineModelPart(ModelPartType.LEFT_LEG, this.leftLeg);
  }

  @Inject(
      method = "setupAnim(Lnet/minecraft/client/renderer/entity/state/VillagerRenderState;)V",
      at = @At("HEAD"),
      cancellable = true)
  private void setupNpcAnim(T renderState, CallbackInfo callbackInfo) {
    if (!(renderState instanceof EasyNPCRenderStateExtension extension)) {
      return;
    }

    UUID uuid = extension.getEasyNpcUUID();
    if (uuid == null) {
      return;
    }

    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid);
    if (easyNPC == null) {
      return;
    }
    VillagerModel model = (VillagerModel) (Object) this;

    if (EasyNPCModel.setupAnimation(easyNPC, this.modelManager, model, renderState)) {
      callbackInfo.cancel();
    }
  }
}
