package de.markusbordihn.easynpc.mixin.renderer.state;

import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.data.model.ModelArmPose;
import java.util.UUID;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;

@Mixin(LivingEntityRenderState.class)
public class EasyNPCLivingEntityRenderStateMixin implements EasyNPCRenderStateExtension {

  @Unique private UUID easyNpcUUID;

  @Unique private ModelArmPose easyNpcLeftArmPose = ModelArmPose.DEFAULT;

  @Unique private ModelArmPose easyNpcRightArmPose = ModelArmPose.DEFAULT;

  @Override
  public UUID getEasyNpcUUID() {
    return this.easyNpcUUID;
  }

  @Override
  public void setEasyNpcUUID(UUID uuid) {
    this.easyNpcUUID = uuid;
  }

  @Override
  public ModelArmPose getEasyNpcLeftArmPose() {
    return this.easyNpcLeftArmPose;
  }

  @Override
  public void setEasyNpcLeftArmPose(ModelArmPose pose) {
    this.easyNpcLeftArmPose = pose != null ? pose : ModelArmPose.DEFAULT;
  }

  @Override
  public ModelArmPose getEasyNpcRightArmPose() {
    return this.easyNpcRightArmPose;
  }

  @Override
  public void setEasyNpcRightArmPose(ModelArmPose pose) {
    this.easyNpcRightArmPose = pose != null ? pose : ModelArmPose.DEFAULT;
  }
}
