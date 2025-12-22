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
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.state.AvatarRenderState;
import net.minecraft.core.ClientAsset;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.player.PlayerModelType;
import net.minecraft.world.entity.player.PlayerSkin;

public class PlayerRenderer
    extends HumanoidMobRenderer<PathfinderMob, AvatarRenderState, PlayerModel>
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
  }

  @Override
  public AvatarRenderState createRenderState() {
    return new AvatarRenderState();
  }

  @Override
  public void extractRenderState(
      PathfinderMob entity, AvatarRenderState renderState, float partialTicks) {
    super.extractRenderState(entity, renderState, partialTicks);

    if (entity instanceof EasyNPC<?> easyNPC) {
      applySkinToRenderState(easyNPC, renderState);
    }
  }

  private void applySkinToRenderState(EasyNPC<?> easyNPC, AvatarRenderState renderState) {
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null || skinData.getSkinType() == SkinType.NONE) {
      return;
    }

    PlayerModelType playerModelType =
        skinData.getSkinModel() == SkinModel.HUMANOID_SLIM
            ? PlayerModelType.SLIM
            : PlayerModelType.WIDE;

    if (skinData.getSkinType() == SkinType.DEFAULT) {
      VariantDataCapable<?> variantData = easyNPC.getEasyNPCVariantData();
      if (variantData.getSkinVariantType() instanceof VariantTexture variantTexture) {
        renderState.skin =
            new PlayerSkin(variantTexture.getResourceTexture(), null, null, playerModelType, false);
      }
    } else if (skinData.getSkinType() == SkinType.CUSTOM) {
      ResourceLocation textureLocation =
          CustomTextureManager.getOrCreateTextureWithDefault(skinData, getDefaultTexture());
      renderState.skin =
          new PlayerSkin(
              new ClientAsset.ResourceTexture(textureLocation, textureLocation),
              null,
              null,
              playerModelType,
              false);
    } else if (skinData.getSkinType() == SkinType.PLAYER_SKIN) {
      ResourceLocation textureLocation =
          PlayerTextureManager.getOrCreateTextureWithDefault(skinData, getDefaultTexture());
      renderState.skin =
          new PlayerSkin(
              new ClientAsset.ResourceTexture(textureLocation, textureLocation),
              null,
              null,
              playerModelType,
              false);
    } else if (skinData.getSkinType() == SkinType.INSECURE_REMOTE_URL) {
      ResourceLocation textureLocation =
          RemoteTextureManager.getOrCreateTextureWithDefault(skinData, getDefaultTexture());
      renderState.skin =
          new PlayerSkin(
              new ClientAsset.ResourceTexture(textureLocation, textureLocation),
              null,
              null,
              playerModelType,
              false);
    } else if (skinData.getSkinType() == SkinType.SECURE_REMOTE_URL) {
      ResourceLocation textureLocation =
          RemoteTextureManager.getOrCreateTextureWithDefault(skinData, getDefaultTexture());
      renderState.skin =
          new PlayerSkin(
              new ClientAsset.ResourceTexture(textureLocation, textureLocation),
              null,
              null,
              playerModelType,
              true);
    }
  }

  @Override
  public ResourceLocation getTextureLocation(AvatarRenderState renderState) {
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
