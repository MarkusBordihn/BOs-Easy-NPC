package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.texture.VariantTextureManager;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.HumanoidSlimNPC.VariantType;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.PathfinderMob;

public class PlayerSlimRenderer extends PlayerRenderer {

  protected static final Map<VariantType, ResourceLocation> TEXTURE_BY_VARIANT_TYPE =
      Util.make(
          new EnumMap<>(VariantType.class),
          map -> {
            map.put(
                VariantType.ALEX,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/slim/alex.png"));
            map.put(
                VariantType.ARI,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/slim/ari.png"));
            map.put(
                VariantType.EFE,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/slim/efe.png"));
            map.put(
                VariantType.KAI,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/slim/kai.png"));
            map.put(
                VariantType.MAKENA,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/slim/makena.png"));
            map.put(
                VariantType.NOOR,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/slim/noor.png"));
            map.put(
                VariantType.STEVE,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/slim/steve.png"));
            map.put(
                VariantType.SUNNY,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/slim/sunny.png"));
            map.put(
                VariantType.ZURI,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/slim/zuri.png"));
            map.put(
                VariantType.KAWORRU,
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/humanoid_slim/kaworru.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT_TYPE.get(VariantType.ALEX);

  static {
    VariantTextureManager.registerVariantTextures(SkinModel.HUMANOID_SLIM, TEXTURE_BY_VARIANT_TYPE);
  }

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

  @Override
  public ResourceLocation getTextureByVariant(Enum variantType) {
    return TEXTURE_BY_VARIANT_TYPE != null
        ? TEXTURE_BY_VARIANT_TYPE.getOrDefault(variantType, DEFAULT_TEXTURE)
        : Constants.BLANK_ENTITY_TEXTURE;
  }
}
