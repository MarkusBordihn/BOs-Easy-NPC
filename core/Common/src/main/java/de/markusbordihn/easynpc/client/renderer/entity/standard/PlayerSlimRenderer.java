package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.data.skin.variant.HumanoidSlimSkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.PathfinderMob;

public class PlayerSlimRenderer extends PlayerRenderer {

  protected static final ResourceLocation DEFAULT_TEXTURE =
      HumanoidSlimSkinVariant.ALEX.getTextureLocation();

  public PlayerSlimRenderer(EntityRendererProvider.Context context) {
    super(context, true);
  }

  @Override
  public ResourceLocation getTextureLocation(PathfinderMob entity) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      return getEntityPlayerTexture(easyNPC);
    }
    return DEFAULT_TEXTURE;
  }

  @Override
  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }
}
