package de.markusbordihn.easynpc.client.renderer.entity.custom;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.custom.FairyModel;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.npc.custom.Fairy;
import de.markusbordihn.easynpc.entity.easynpc.npc.custom.Fairy.VariantType;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.state.HumanoidRenderState;
import net.minecraft.resources.ResourceLocation;

public class FairyRenderer
    extends HumanoidMobRenderer<Fairy, HumanoidRenderState, FairyModel<HumanoidRenderState>>
    implements EasyNPCEntityRenderer {

  protected static final Map<VariantType, ResourceLocation> TEXTURE_BY_VARIANT_TYPE =
      Util.make(
          new EnumMap<>(VariantType.class),
          map -> {
            map.put(
                VariantType.BLUE,
                ResourceLocation.fromNamespaceAndPath(
                    Constants.MOD_ID, "textures/entity/fairy/fairy_blue.png"));
            map.put(
                VariantType.GREEN,
                ResourceLocation.fromNamespaceAndPath(
                    Constants.MOD_ID, "textures/entity/fairy/fairy_green.png"));
            map.put(
                VariantType.RED,
                ResourceLocation.fromNamespaceAndPath(
                    Constants.MOD_ID, "textures/entity/fairy/fairy_red.png"));
          });

  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT_TYPE.get(VariantType.GREEN);

  public FairyRenderer(
      EntityRendererProvider.Context context, ModelLayerLocation modelLayerLocation) {
    super(context, new FairyModel<>(context.bakeLayer(modelLayerLocation)), 0.3F);
  }

  @Override
  public ResourceLocation getTextureLocation(HumanoidRenderState renderState) {
    EasyNPC<?> easyNPC = getEasyNPC(renderState);
    if (easyNPC != null) {
      return getEntityTexture(easyNPC);
    }
    return DEFAULT_TEXTURE;
  }

  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }

  public ResourceLocation getCustomTexture(SkinData<?> entity) {
    return CustomTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  public ResourceLocation getRemoteTexture(SkinData<?> entity) {
    return RemoteTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  public ResourceLocation getTextureByVariant(Enum<?> variantType) {
    return TEXTURE_BY_VARIANT_TYPE != null
        ? TEXTURE_BY_VARIANT_TYPE.getOrDefault(variantType, DEFAULT_TEXTURE)
        : Constants.BLANK_ENTITY_TEXTURE;
  }

  public <N extends EasyNPC<?>> ResourceLocation getEntityTexture(N easyNPC) {
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    return switch (skinData.getSkinType()) {
      case NONE -> Constants.BLANK_ENTITY_TEXTURE;
      case CUSTOM -> getCustomTexture(skinData);
      case SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> getRemoteTexture(skinData);
      default -> getTextureByVariant(easyNPC.getEasyNPCVariantData().getVariantType());
    };
  }

  @Override
  public HumanoidRenderState createRenderState() {
    return new HumanoidRenderState();
  }
}
