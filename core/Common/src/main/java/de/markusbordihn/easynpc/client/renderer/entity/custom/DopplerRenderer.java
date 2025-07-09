package de.markusbordihn.easynpc.client.renderer.entity.custom;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.client.renderer.manager.RendererManager;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.npc.custom.Doppler.VariantType;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.HumanoidArmorModel;
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.PathfinderMob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DopplerRenderer<E extends PathfinderMob, M extends PlayerModel<E>>
    extends LivingEntityRenderer<E, M> implements EasyNPCEntityRenderer {

  protected static final Map<VariantType, ResourceLocation> TEXTURE_BY_VARIANT_TYPE =
      Util.make(
          new EnumMap<>(VariantType.class),
          map -> map.put(
              VariantType.DEFAULT,
              new ResourceLocation(Constants.MOD_ID, "textures/entity/doppler/doppler.png")));
  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT_TYPE.get(VariantType.DEFAULT);
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public DopplerRenderer(EntityRendererProvider.Context context) {
    this(context, false);
  }

  public DopplerRenderer(EntityRendererProvider.Context context, boolean slim) {
    super(
        context,
        (M)
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

  private boolean renderEntity(
      PathfinderMob entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {

    // We only take care of EasyNPC entities.
    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return false;
    }

    // Get render data.
    RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData == null
        || renderData.getRenderDataSet() == null
        || renderData.getRenderDataSet().getRenderType() != RenderType.CUSTOM_ENTITY) {
      return false;
    }

    // Get custom render data.
    EntityType<? extends Entity> renderEntityType =
        renderData.getRenderDataSet().getRenderEntityType();

    // Get custom entity for render custom .
    PathfinderMob customEntity =
        EntityTypeManager.getPathfinderMob(renderEntityType, entity.level());
    if (customEntity == null) {
      return false;
    }

    // Get entity type name.
    String entityTypeName = EntityTypeManager.getEntityTypeName(renderEntityType);

    // Render custom entity over living render, if supported.
    LivingEntityRenderer<E, M> livingEntityRenderer =
        (LivingEntityRenderer<E, M>)
            RendererManager.getLivingEntityRenderer(renderEntityType, customEntity);
    if (livingEntityRenderer != null) {
      try {
        RendererManager.copyCustomLivingEntityData(entity, customEntity, entityTypeName);
        livingEntityRenderer.render(
            (E) customEntity, entityYaw, partialTicks, poseStack, buffer, packedLight);
        return true;
      } catch (Exception exception) {
        log.error(
            "Failed to render custom living entity {} ({}):",
            customEntity,
            renderEntityType,
            exception);
        EntityTypeManager.addUnsupportedEntityType(renderEntityType);
        return false;
      }
    }

    // Alternative render custom entity over entity render, if supported.
    EntityRenderer<E> entityRenderer =
        (EntityRenderer<E>) RendererManager.getEntityRenderer(renderEntityType, customEntity);
    if (entityRenderer != null) {
      try {
        RendererManager.copyCustomLivingEntityData(entity, customEntity, entityTypeName);
        entityRenderer.render(
            (E) customEntity, entityYaw, partialTicks, poseStack, buffer, packedLight);
        return true;
      } catch (Exception exception) {
        log.error(
            "Failed to render custom entity {} ({}):", customEntity, renderEntityType, exception);
        EntityTypeManager.addUnsupportedEntityType(renderEntityType);
        return false;
      }
    }

    // Give up rendering, if no custom renderer is available.
    return false;
  }

  @Override
  public ResourceLocation getTextureLocation(E entity) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      return getEntityTexture(easyNPC);
    }
    return DEFAULT_TEXTURE;
  }

  @Override
  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }

  @Override
  public ResourceLocation getTextureByVariant(Enum<?> variantType) {
    return TEXTURE_BY_VARIANT_TYPE != null
        ? TEXTURE_BY_VARIANT_TYPE.getOrDefault(variantType, DEFAULT_TEXTURE)
        : Constants.BLANK_ENTITY_TEXTURE;
  }

  @Override
  public void render(
      E entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight) {
    if (renderEntity(entity, entityYaw, partialTicks, poseStack, bufferSource, packedLight)) {
      return;
    }

    super.render(entity, entityYaw, partialTicks, poseStack, bufferSource, packedLight);
  }
}
