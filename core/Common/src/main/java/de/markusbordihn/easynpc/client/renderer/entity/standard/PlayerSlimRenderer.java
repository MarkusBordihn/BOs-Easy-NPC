package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.data.skin.variant.HumanoidSlimSkinVariant;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.resources.ResourceLocation;

public class PlayerSlimRenderer extends PlayerRenderer {

  protected static final ResourceLocation DEFAULT_TEXTURE =
      HumanoidSlimSkinVariant.ALEX.getTextureLocation();

  public PlayerSlimRenderer(EntityRendererProvider.Context context) {
    super(context, true);
  }

  @Override
  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }
}
