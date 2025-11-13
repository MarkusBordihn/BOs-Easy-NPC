package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.data.skin.variant.HumanoidSkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.model.HumanoidArmorModel;
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.renderer.entity.layers.PlayerItemInHandLayer;
import net.minecraft.client.renderer.entity.state.PlayerRenderState;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.PathfinderMob;

public class PlayerRenderer
    extends HumanoidMobRenderer<PathfinderMob, PlayerRenderState, PlayerModel>
    implements EasyNPCEntityRenderer {

  protected static final ResourceLocation DEFAULT_TEXTURE =
      HumanoidSkinVariant.STEVE.getTextureLocation();

  public PlayerRenderer(EntityRendererProvider.Context context) {
    this(context, false);
  }

  public PlayerRenderer(EntityRendererProvider.Context context, boolean slim) {
    super(
        context,
        new PlayerModel(
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
            context.getEquipmentRenderer()));
    this.addLayer(new PlayerItemInHandLayer<>(this, context.getItemRenderer()));
  }

  @Override
  public PlayerRenderState createRenderState() {
    return new PlayerRenderState();
  }

  @Override
  public ResourceLocation getTextureLocation(PlayerRenderState renderState) {
    EasyNPC<?> easyNPC = getEasyNPC(renderState);
    if (easyNPC != null) {
      return getEntityPlayerTexture(easyNPC);
    }
    return DEFAULT_TEXTURE;
  }

  @Override
  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }
}
