package de.markusbordihn.easynpc.client.renderer.entity.custom;

import de.markusbordihn.easynpc.client.model.custom.FairyModel;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.data.skin.variant.FairySkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.npc.custom.Fairy;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.resources.ResourceLocation;

public class FairyRenderer<E extends Fairy> extends HumanoidMobRenderer<E, FairyModel<E>>
    implements EasyNPCEntityRenderer {

  protected static final ResourceLocation DEFAULT_TEXTURE =
      FairySkinVariant.GREEN.getTextureLocation();

  public FairyRenderer(
      EntityRendererProvider.Context context, ModelLayerLocation modelLayerLocation) {
    super(context, new FairyModel<>(context.bakeLayer(modelLayerLocation)), 0.3F);
  }

  @Override
  public ResourceLocation getTextureLocation(E entity) {
    return getEntityTexture(entity);
  }

  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }

  public ResourceLocation getCustomTexture(SkinDataCapable<?> entity) {
    return CustomTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  public ResourceLocation getRemoteTexture(SkinDataCapable<?> entity) {
    return RemoteTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }
}
