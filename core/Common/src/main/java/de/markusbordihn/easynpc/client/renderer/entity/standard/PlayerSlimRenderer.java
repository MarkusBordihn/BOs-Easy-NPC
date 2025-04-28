package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.HumanoidSlimNPC.VariantType;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.state.PlayerRenderState;
import net.minecraft.resources.ResourceLocation;

public class PlayerSlimRenderer extends PlayerRenderer {

  protected static final Map<VariantType, ResourceLocation> TEXTURE_BY_VARIANT_TYPE =
      Util.make(
          new EnumMap<>(VariantType.class),
          map -> {
            map.put(
                VariantType.ALEX,
                ResourceLocation.withDefaultNamespace("textures/entity/player/slim/alex.png"));
            map.put(
                VariantType.ARI,
                ResourceLocation.withDefaultNamespace("textures/entity/player/slim/ari.png"));
            map.put(
                VariantType.EFE,
                ResourceLocation.withDefaultNamespace("textures/entity/player/slim/efe.png"));
            map.put(
                VariantType.KAI,
                ResourceLocation.withDefaultNamespace("textures/entity/player/slim/kai.png"));
            map.put(
                VariantType.MAKENA,
                ResourceLocation.withDefaultNamespace("textures/entity/player/slim/makena.png"));
            map.put(
                VariantType.NOOR,
                ResourceLocation.withDefaultNamespace("textures/entity/player/slim/noor.png"));
            map.put(
                VariantType.STEVE,
                ResourceLocation.withDefaultNamespace("textures/entity/player/slim/steve.png"));
            map.put(
                VariantType.SUNNY,
                ResourceLocation.withDefaultNamespace("textures/entity/player/slim/sunny.png"));
            map.put(
                VariantType.ZURI,
                ResourceLocation.withDefaultNamespace("textures/entity/player/slim/zuri.png"));
            map.put(
                VariantType.KAWORRU,
                ResourceLocation.fromNamespaceAndPath(
                    Constants.MOD_ID, "textures/entity/humanoid_slim/kaworru.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT_TYPE.get(VariantType.ALEX);

  public PlayerSlimRenderer(EntityRendererProvider.Context context) {
    super(context, true);
  }

  @Override
  public ResourceLocation getTextureLocation(PlayerRenderState renderState) {
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
}
