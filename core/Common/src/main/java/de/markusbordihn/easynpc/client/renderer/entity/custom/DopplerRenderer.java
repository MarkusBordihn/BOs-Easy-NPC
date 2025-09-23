package de.markusbordihn.easynpc.client.renderer.entity.custom;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.client.renderer.manager.RendererManager;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.npc.custom.Doppler.VariantType;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.HumanoidArmorModel;
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.renderer.entity.layers.PlayerItemInHandLayer;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.client.renderer.entity.state.PlayerRenderState;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DopplerRenderer
    extends LivingEntityRenderer<PathfinderMob, PlayerRenderState, PlayerModel>
    implements EasyNPCEntityRenderer {

  protected static final Map<VariantType, ResourceLocation> TEXTURE_BY_VARIANT_TYPE =
      Util.make(
          new EnumMap<>(VariantType.class),
          map ->
              map.put(
                  VariantType.DEFAULT,
                  ResourceLocation.fromNamespaceAndPath(
                      Constants.MOD_ID, "textures/entity/doppler/doppler.png")));
  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT_TYPE.get(VariantType.DEFAULT);
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public DopplerRenderer(EntityRendererProvider.Context context) {
    this(context, false);
  }

  public DopplerRenderer(EntityRendererProvider.Context context, boolean slim) {
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

  private static boolean renderEntity(
      EasyNPC<?> entity,
      EntityModel<?> entityModel,
      PlayerRenderState renderState,
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
        EntityTypeManager.getPathfinderMob(renderEntityType, entity.getEntityLevel());
    if (customEntity == null) {
      return false;
    }

    // Get entity type name.
    String entityTypeName = EntityTypeManager.getEntityTypeName(renderEntityType);

    // Render custom entity over living render, if supported.
    LivingEntityRenderer<
            LivingEntity, LivingEntityRenderState, EntityModel<? super LivingEntityRenderState>>
        livingEntityRenderer =
            (LivingEntityRenderer<
                    LivingEntity,
                    LivingEntityRenderState,
                    EntityModel<? super LivingEntityRenderState>>)
                RendererManager.getLivingEntityRenderer(renderEntityType, customEntity);
    LivingEntityRenderState livingEntityRenderState =
        livingEntityRenderer.createRenderState(customEntity, 0);
    if (livingEntityRenderState instanceof EasyNPCRenderStateExtension extension) {
      extension.setEasyNpcUUID(easyNPC.getEntityUUID());
    }
    if (livingEntityRenderer != null) {
      try {
        RendererManager.copyCustomLivingEntityData(
            entity.getPathfinderMob(), customEntity, entityTypeName);
        livingEntityRenderer.render(livingEntityRenderState, poseStack, buffer, packedLight);
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
    EntityRenderer<Entity, EntityRenderState> entityRenderer =
        (EntityRenderer<Entity, EntityRenderState>)
            RendererManager.getEntityRenderer(renderEntityType, customEntity);
    EntityRenderState entityRenderState = entityRenderer.createRenderState(customEntity, 0);
    if (entityRenderState instanceof EasyNPCRenderStateExtension extension) {
      extension.setEasyNpcUUID(easyNPC.getEntityUUID());
    }
    if (entityRenderer != null) {
      try {
        RendererManager.copyCustomLivingEntityData(
            entity.getPathfinderMob(), customEntity, entityTypeName);
        entityRenderer.render(entityRenderState, poseStack, buffer, packedLight);
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
  public PlayerRenderState createRenderState() {
    return new PlayerRenderState();
  }

  @Override
  public ResourceLocation getTextureLocation(PlayerRenderState renderState) {
    EasyNPC<?> easyNPC = getEasyNPC(renderState);
    if (easyNPC != null) {
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
    return TEXTURE_BY_VARIANT_TYPE.getOrDefault(variantType, DEFAULT_TEXTURE);
  }

  @Override
  public void render(
      PlayerRenderState renderState,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    EasyNPC<?> easyNPC = getEasyNPC(renderState);
    if (renderEntity(easyNPC, this.getModel(), renderState, poseStack, buffer, packedLight)) {
      return;
    }

    super.render(renderState, poseStack, buffer, packedLight);
  }
}
