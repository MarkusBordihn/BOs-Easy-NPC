package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.HumanoidNPC.VariantType;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
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

  protected static final Map<VariantType, ResourceLocation> TEXTURE_BY_VARIANT_TYPE =
      Util.make(
          new EnumMap<>(VariantType.class),
          map -> {
            map.put(
                VariantType.ALEX,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/wide/alex.png"));
            map.put(
                VariantType.ARI,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/wide/ari.png"));
            map.put(
                VariantType.EFE,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/wide/efe.png"));
            map.put(
                VariantType.KAI,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/wide/kai.png"));
            map.put(
                VariantType.MAKENA,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/wide/makena.png"));
            map.put(
                VariantType.NOOR,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/wide/noor.png"));
            map.put(
                VariantType.STEVE,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/wide/steve.png"));
            map.put(
                VariantType.SUNNY,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/wide/sunny.png"));
            map.put(
                VariantType.ZURI,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/player/wide/zuri.png"));
            map.put(
                VariantType.JAYJASONBO,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/humanoid/jayjasonbo.png"));
            map.put(
                VariantType.PROFESSOR_01,
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/humanoid/professor_01.png"));
            map.put(
                VariantType.SECURITY_01,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/humanoid/security_01.png"));
            map.put(
                VariantType.KNIGHT_01,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/humanoid/knight_01.png"));
            map.put(
                VariantType.KNIGHT_02,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/humanoid/knight_02.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT_TYPE.get(VariantType.STEVE);

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

  public ResourceLocation getTextureByVariant(Enum variantType) {
    return TEXTURE_BY_VARIANT_TYPE != null
        ? TEXTURE_BY_VARIANT_TYPE.getOrDefault(variantType, DEFAULT_TEXTURE)
        : Constants.BLANK_ENTITY_TEXTURE;
  }
}
