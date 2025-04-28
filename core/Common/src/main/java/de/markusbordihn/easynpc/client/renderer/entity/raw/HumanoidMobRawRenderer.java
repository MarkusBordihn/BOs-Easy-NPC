package de.markusbordihn.easynpc.client.renderer.entity.raw;

import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;

public class HumanoidMobRawRenderer extends HumanoidMobRenderer {

  public HumanoidMobRawRenderer(
      EntityRendererProvider.Context context, HumanoidModel<?> model, float shadowRadius) {
    super(context, model, shadowRadius);
  }

  @Override
  public ResourceLocation getTextureLocation(Entity entity) {
    return null;
  }
}
