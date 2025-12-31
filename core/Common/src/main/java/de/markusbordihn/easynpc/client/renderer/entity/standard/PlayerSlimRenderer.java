package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.data.skin.variant.HumanoidSlimSkinVariant;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.resources.Identifier;

public class PlayerSlimRenderer extends PlayerRenderer {

  protected static final Identifier DEFAULT_TEXTURE =
      HumanoidSlimSkinVariant.ALEX.getTextureLocation();

  public PlayerSlimRenderer(EntityRendererProvider.Context context) {
    super(context, true);
  }

  @Override
  public Identifier getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }
}
