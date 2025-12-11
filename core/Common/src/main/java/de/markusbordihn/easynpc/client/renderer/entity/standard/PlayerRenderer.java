package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.skin.VariantTexture;
import de.markusbordihn.easynpc.data.skin.variant.HumanoidSkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import net.minecraft.client.model.HumanoidArmorModel;
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.renderer.entity.layers.PlayerItemInHandLayer;
import net.minecraft.client.renderer.entity.state.PlayerRenderState;
import net.minecraft.client.resources.PlayerSkin;
import net.minecraft.client.resources.PlayerSkin.Model;
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
    this.addLayer(new PlayerItemInHandLayer<>(this));
  }

  @Override
  public PlayerRenderState createRenderState() {
    return new PlayerRenderState();
  }

  @Override
  public void extractRenderState(
      PathfinderMob entity, PlayerRenderState renderState, float partialTicks) {
    super.extractRenderState(entity, renderState, partialTicks);

    if (entity instanceof EasyNPC<?> easyNPC) {
      applySkinToRenderState(easyNPC, renderState);
    }
  }

  private void applySkinToRenderState(EasyNPC<?> easyNPC, PlayerRenderState renderState) {
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null || skinData.getSkinType() == SkinType.NONE) {
      return;
    }

    Model playerSkinModel =
        skinData.getSkinModel() == SkinModel.HUMANOID_SLIM ? Model.SLIM : Model.WIDE;
    ResourceLocation defaultTexture = renderState.skin.texture();
    if (skinData.getSkinType() == SkinType.DEFAULT) {
      VariantDataCapable<?> variantData = easyNPC.getEasyNPCVariantData();
      if (variantData.getSkinVariantType() instanceof VariantTexture variantTexture) {
        renderState.skin =
            new PlayerSkin(
                variantTexture.getTextureLocation(), null, null, null, playerSkinModel, false);
      }
    } else if (skinData.getSkinType() == SkinType.CUSTOM) {
      renderState.skin =
          new PlayerSkin(
              CustomTextureManager.getOrCreateTextureWithDefault(skinData, defaultTexture),
              null,
              null,
              null,
              playerSkinModel,
              false);
    } else if (skinData.getSkinType() == SkinType.PLAYER_SKIN) {
      renderState.skin =
          new PlayerSkin(
              PlayerTextureManager.getOrCreateTextureWithDefault(skinData, defaultTexture),
              null,
              null,
              null,
              playerSkinModel,
              false);
    } else if (skinData.getSkinType() == SkinType.INSECURE_REMOTE_URL) {
      renderState.skin =
          new PlayerSkin(
              RemoteTextureManager.getOrCreateTextureWithDefault(skinData, defaultTexture),
              skinData.getSkinURL(),
              null,
              null,
              playerSkinModel,
              false);
    } else if (skinData.getSkinType() == SkinType.SECURE_REMOTE_URL) {
      renderState.skin =
          new PlayerSkin(
              RemoteTextureManager.getOrCreateTextureWithDefault(skinData, defaultTexture),
              skinData.getSkinURL(),
              null,
              null,
              playerSkinModel,
              true);
    }
  }

  @Override
  public ResourceLocation getTextureLocation(PlayerRenderState renderState) {
    return getTextureFromRenderState(renderState);
  }

  @Override
  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }

  @Override
  public boolean supportsPlayerSkins() {
    return true;
  }
}
