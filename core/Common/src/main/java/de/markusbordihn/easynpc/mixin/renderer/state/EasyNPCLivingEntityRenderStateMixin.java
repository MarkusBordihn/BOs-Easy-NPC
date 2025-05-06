package de.markusbordihn.easynpc.mixin.renderer.state;

import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import java.util.UUID;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;

@Mixin(LivingEntityRenderState.class)
public class EasyNPCLivingEntityRenderStateMixin implements EasyNPCRenderStateExtension {

  @Unique private UUID easyNpcUUID;

  @Override
  public UUID getEasyNpcUUID() {
    return this.easyNpcUUID;
  }

  @Override
  public void setEasyNpcUUID(UUID uuid) {
    this.easyNpcUUID = uuid;
  }
}
