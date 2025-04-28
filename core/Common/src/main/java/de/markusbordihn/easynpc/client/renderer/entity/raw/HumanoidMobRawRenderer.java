package de.markusbordihn.easynpc.client.renderer.entity.raw;

import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.entity.state.HumanoidRenderState;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.resources.ResourceLocation;

public class HumanoidMobRawRenderer extends HumanoidMobRenderer {

  public HumanoidMobRawRenderer(
      EntityRendererProvider.Context context, HumanoidModel<?> model, float shadowRadius) {
    super(context, model, shadowRadius);
  }

  @Override
  public ResourceLocation getTextureLocation(LivingEntityRenderState renderState) {
    return null;
  }

  @Override
  public EntityRenderState createRenderState() {
    return new HumanoidRenderState();
  }
}
