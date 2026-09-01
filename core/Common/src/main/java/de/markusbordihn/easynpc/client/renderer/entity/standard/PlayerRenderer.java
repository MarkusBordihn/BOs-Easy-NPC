package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.api.skin.VariantTexture;
import de.markusbordihn.easynpc.client.model.EasyNPCModelManager;
import de.markusbordihn.easynpc.client.model.EasyNPCModelManagerAccessor;
import de.markusbordihn.easynpc.client.model.ModModelLayers;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.EasyNPCItemAttachmentLayer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.SkullHeadRenderLayer;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.skin.variant.HumanoidSkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.model.player.PlayerModel;
import net.minecraft.client.renderer.entity.ArmorModelSet;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.client.renderer.entity.state.AvatarRenderState;
import net.minecraft.core.ClientAsset;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.player.PlayerModelType;
import net.minecraft.world.entity.player.PlayerSkin;

public class PlayerRenderer
    extends HumanoidMobRenderer<PathfinderMob, AvatarRenderState, PlayerModel>
    implements EasyNPCEntityRenderer {

  protected static final Identifier DEFAULT_TEXTURE =
      HumanoidSkinVariant.STEVE.getTextureLocation();

  public PlayerRenderer(EntityRendererProvider.Context context) {
    this(context, false);
  }

  public PlayerRenderer(EntityRendererProvider.Context context, boolean slim) {
    this(context, slim, false);
  }

  public PlayerRenderer(
      EntityRendererProvider.Context context, boolean slim, boolean useVanillaItemInHandLayer) {
    super(
        context,
        new PlayerModel(
            context.bakeLayer(slim ? ModModelLayers.HUMANOID_SLIM : ModModelLayers.HUMANOID), slim),
        0.5F);
    this.addLayer(
        new HumanoidArmorLayer<>(
            this,
            ArmorModelSet.bake(
                slim ? ModelLayers.PLAYER_SLIM_ARMOR : ModelLayers.PLAYER_ARMOR,
                context.getModelSet(),
                modelPart -> new PlayerModel(modelPart, slim)),
            context.getEquipmentRenderer()));
    this.addLayer(new SkullHeadRenderLayer<>(this));
    if (useVanillaItemInHandLayer) {
      this.addLayer(new ItemInHandLayer<>(this));
    } else {
      this.addLayer(new EasyNPCItemAttachmentLayer<>(this));
    }
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
      if (this.getModel() instanceof EasyNPCModelManagerAccessor accessor) {
        EasyNPCModelManager modelManager = accessor.easyNPC$getModelManager();
        if (modelManager != null) {
          modelManager.validateModelPartsOnce(easyNPC);
        }
      }
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
      Identifier textureLocation =
          CustomTextureManager.getOrCreateTextureWithDefault(skinData, getDefaultTexture());
      renderState.skin =
          new PlayerSkin(
              new ClientAsset.ResourceTexture(textureLocation, textureLocation),
              null,
              null,
              playerModelType,
              false);
    } else if (skinData.getSkinType() == SkinType.PLAYER_SKIN) {
      Identifier textureLocation =
          PlayerTextureManager.getOrCreateTextureWithDefault(skinData, getDefaultTexture());
      renderState.skin =
          new PlayerSkin(
              new ClientAsset.ResourceTexture(textureLocation, textureLocation),
              null,
              null,
              playerModelType,
              false);
    } else if (skinData.getSkinType() == SkinType.INSECURE_REMOTE_URL) {
      Identifier textureLocation =
          RemoteTextureManager.getOrCreateTextureWithDefault(skinData, getDefaultTexture());
      renderState.skin =
          new PlayerSkin(
              new ClientAsset.ResourceTexture(textureLocation, textureLocation),
              null,
              null,
              playerModelType,
              false);
    } else if (skinData.getSkinType() == SkinType.SECURE_REMOTE_URL) {
      Identifier textureLocation =
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
  public Identifier getTextureLocation(AvatarRenderState renderState) {
    return getTextureFromRenderState(renderState);
  }

  @Override
  public Identifier getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }

  @Override
  public boolean supportsPlayerSkins() {
    return true;
  }
}
