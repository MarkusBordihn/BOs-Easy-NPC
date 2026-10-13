package de.markusbordihn.easynpc.mixin.renderer.state;

import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.resources.Identifier;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;

@Mixin(LivingEntityRenderState.class)
public class EasyNPCLivingEntityRenderStateMixin implements EasyNPCRenderStateExtension {

  @Unique private EasyNPC<?> easyNPC;
  @Unique private Identifier easyNpcTexture;

  @Override
  public EasyNPC<?> getEasyNPC() {
    return this.easyNPC;
  }

  @Override
  public void setEasyNPC(EasyNPC<?> easyNPC) {
    this.easyNPC = easyNPC;
  }

  @Override
  public Identifier getEasyNpcTexture() {
    return this.easyNpcTexture;
  }

  @Override
  public void setEasyNpcTexture(Identifier texture) {
    this.easyNpcTexture = texture;
  }
}
