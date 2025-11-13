package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.data.skin.variant.HumanoidSkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import net.minecraft.client.model.HumanoidArmorModel;
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.PathfinderMob;

public class PlayerRenderer<T extends PathfinderMob> extends LivingEntityRenderer<T, PlayerModel<T>>
    implements EasyNPCEntityRenderer {

  protected static final ResourceLocation DEFAULT_TEXTURE =
      HumanoidSkinVariant.STEVE.getTextureLocation();

  public PlayerRenderer(EntityRendererProvider.Context context) {
    this(context, false);
  }

  public PlayerRenderer(EntityRendererProvider.Context context, boolean slim) {
    super(
        context,
        new PlayerModel<>(
            context.bakeLayer(slim ? ModelLayers.PLAYER_SLIM : ModelLayers.PLAYER), slim),
        0.5F);
    this.addLayer(
        new HumanoidArmorLayer<>(
            this,
            new HumanoidArmorModel(
                context.bakeLayer(
                    slim ? ModelLayers.PLAYER_SLIM_INNER_ARMOR : ModelLayers.PLAYER_INNER_ARMOR)),
            new HumanoidArmorModel(
                context.bakeLayer(
                    slim ? ModelLayers.PLAYER_SLIM_OUTER_ARMOR : ModelLayers.PLAYER_OUTER_ARMOR)),
            context.getModelManager()));
    this.addLayer(new ItemInHandLayer<>(this, context.getItemInHandRenderer()));
  }

  @Override
  public ResourceLocation getTextureLocation(T entity) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      return getEntityPlayerTexture(easyNPC);
    }
    return DEFAULT_TEXTURE;
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
