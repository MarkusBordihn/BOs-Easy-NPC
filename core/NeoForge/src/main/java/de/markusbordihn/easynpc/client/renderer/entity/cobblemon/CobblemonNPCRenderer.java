/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.client.renderer.entity.cobblemon;

import com.cobblemon.mod.common.CobblemonEntities;
import com.cobblemon.mod.common.api.pokemon.PokemonSpecies;
import com.cobblemon.mod.common.client.entity.PokemonClientDelegate;
import com.cobblemon.mod.common.client.render.VaryingRenderableResolver;
import com.cobblemon.mod.common.client.render.models.blockbench.PosableModel;
import com.cobblemon.mod.common.client.render.models.blockbench.repository.VaryingModelRepository;
import com.cobblemon.mod.common.entity.pokemon.PokemonEntity;
import com.cobblemon.mod.common.pokemon.Gender;
import com.cobblemon.mod.common.pokemon.Pokemon;
import com.cobblemon.mod.common.pokemon.Species;
import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.custom.DopplerModel;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCLivingEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.client.renderer.manager.RendererManager;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import de.markusbordihn.easynpc.compat.cobblemon.CobblemonSpeciesManager;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.skin.variant.DopplerSkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.mixin.renderer.MobRendererInvoker;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CobblemonNPCRenderer<E extends PathfinderMob>
    extends HumanoidMobRenderer<E, DopplerModel<E>> implements EasyNPCEntityRenderer {

  protected static final ResourceLocation DEFAULT_TEXTURE =
      DopplerSkinVariant.DOPPLER.getTextureLocation();
  private static final float GUI_PREVIEW_PROFILE_SCALE_FACTOR = 1.15F;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<ResourceLocation, PokemonEntity> cobblemonEntityCache = new HashMap<>();
  private static final Map<ResourceLocation, Boolean> invalidSpeciesCache = new HashMap<>();
  private static final Map<ResourceLocation, Float> guiPreviewScaleCache = new HashMap<>();

  public CobblemonNPCRenderer(
      EntityRendererProvider.Context context, ModelLayerLocation modelLayerLocation) {
    super(context, new DopplerModel<>(context.bakeLayer(modelLayerLocation)), 0.5F);
  }

  private static PokemonEntity getOrCreateCobblemonEntity(ResourceLocation modelKey, Level level) {
    if (invalidSpeciesCache.containsKey(modelKey)) {
      return null;
    }

    PokemonEntity cached = cobblemonEntityCache.get(modelKey);
    if (cached != null && cached.isAlive()) {
      return cached;
    }

    ResourceLocation speciesId = CobblemonSpeciesManager.getBaseSpeciesId(modelKey);
    Species species = PokemonSpecies.INSTANCE.getByIdentifier(speciesId);
    if (species == null) {
      log.warn("Unknown Cobblemon species: {}", speciesId);
      invalidSpeciesCache.put(modelKey, Boolean.TRUE);
      return null;
    }

    try {
      Pokemon cobblemonInstance = new Pokemon();
      cobblemonInstance.setSpecies(species);
      applyVariantAspects(cobblemonInstance, modelKey);
      PokemonEntity entity = new PokemonEntity(level, cobblemonInstance, CobblemonEntities.POKEMON);
      entity.setNoAi(true);
      entity.setSilent(true);
      entity.noPhysics = true;
      entity.setCustomNameVisible(false);
      entity.setInvulnerable(true);
      entity.setTicksLived(100);
      entity.hideNameRendering();
      syncEntityData(cobblemonInstance, entity);
      cobblemonEntityCache.put(modelKey, entity);
      return entity;
    } catch (Exception exception) {
      log.error("Failed to create Cobblemon entity for species {}", modelKey, exception);
      invalidSpeciesCache.put(modelKey, Boolean.TRUE);
      return null;
    }
  }

  private static void syncEntityData(Pokemon cobblemonInstance, PokemonEntity entity) {
    entity
        .getEntityData()
        .set(
            PokemonEntity.getSPECIES(),
            cobblemonInstance.getSpecies().getResourceIdentifier().toString());
    entity.getEntityData().set(PokemonEntity.getASPECTS(), cobblemonInstance.getAspects());
  }

  private static void applyVariantAspects(Pokemon cobblemonInstance, ResourceLocation modelKey) {
    Set<String> variantAspects = CobblemonSpeciesManager.getVariantAspects(modelKey);
    float maleRatio = cobblemonInstance.getSpecies().getMaleRatio();
    if (maleRatio > 0.0F && maleRatio < 1.0F) {
      cobblemonInstance.setGender(
          variantAspects.contains(CobblemonSpeciesManager.VARIANT_FEMALE)
              ? Gender.FEMALE
              : Gender.MALE);
    }
    if (variantAspects.contains(CobblemonSpeciesManager.VARIANT_SHINY)) {
      cobblemonInstance.setShiny(true);
    }
    cobblemonInstance.updateAspects();
  }

  private static float getGuiPreviewScale(
      ResourceLocation modelKey, PokemonEntity cobblemonEntity) {
    Float cachedScale = guiPreviewScaleCache.get(modelKey);
    if (cachedScale != null) {
      return cachedScale;
    }

    float previewScale =
        EntityTypeManager.calculateGuiPreviewScaleFactor(cobblemonEntity.getBbHeight());
    try {
      Pokemon pokemon = cobblemonEntity.getPokemon();
      VaryingRenderableResolver resolver =
          VaryingModelRepository.INSTANCE
              .getVariations()
              .get(pokemon.getSpecies().getResourceIdentifier());
      PosableModel poser = null;
      if (resolver != null) {
        for (PosableModel candidate : resolver.getPosers().values()) {
          poser = candidate;
          break;
        }
      }
      float baseScale = pokemon.getForm().getBaseScale();
      if (poser != null && baseScale > 0f) {
        previewScale =
            Math.min(
                previewScale,
                GUI_PREVIEW_PROFILE_SCALE_FACTOR * poser.getProfileScale() / baseScale);
      }
    } catch (Exception exception) {
      log.debug("Failed to resolve Cobblemon profile scale for {}", modelKey, exception);
    }
    guiPreviewScaleCache.put(modelKey, previewScale);
    return previewScale;
  }

  private static float getRootScaleY(EasyNPC<?> easyNPC) {
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return 1.0f;
    }
    return modelData.getModelRootData().scale().y();
  }

  private static void syncCobblemonRenderState(
      PathfinderMob sourceEntity, PokemonEntity cobblemonEntity, ResourceLocation modelKey) {
    int renderAge = sourceEntity.tickCount + 100;
    cobblemonEntity.setTicksLived(renderAge);
    if (cobblemonEntity.getDelegate() instanceof PokemonClientDelegate clientDelegate) {
      clientDelegate.setCurrentEntity(cobblemonEntity);
      // The client delegate is the PosableState that drives model and texture selection. Feed it
      // the
      // pokemon aspects plus the female / shiny variant tokens so the varying model repository
      // resolves the matching variant; this preview entity is never ticked, so Cobblemon never
      // fills
      // the delegate from the entity data on its own.
      Set<String> aspects = new HashSet<>(cobblemonEntity.getPokemon().getAspects());
      aspects.addAll(CobblemonSpeciesManager.getVariantAspects(modelKey));
      clientDelegate.setCurrentAspects(aspects);
      clientDelegate.updateAge(renderAge);
    }
  }

  private boolean renderCobblemon(
      E entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {

    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return false;
    }

    RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData == null
        || renderData.getRenderDataEntry() == null
        || renderData.getRenderDataEntry().getRenderType() != RenderType.COBBLEMON_ENTITY) {
      return false;
    }

    String modelString = renderData.getRenderDataEntry().getRenderEntityModel();
    if (modelString == null || modelString.isEmpty()) {
      modelString = "cobblemon:ditto";
    }

    ResourceLocation speciesId = ResourceLocation.tryParse(modelString);
    if (speciesId == null) {
      return false;
    }

    PokemonEntity cobblemonEntity = getOrCreateCobblemonEntity(speciesId, entity.level());
    if (cobblemonEntity == null) {
      return false;
    }

    LivingEntityRenderer<?, ?> livingEntityRenderer =
        RendererManager.getLivingEntityRenderer(CobblemonEntities.POKEMON, cobblemonEntity);
    if (livingEntityRenderer == null) {
      return false;
    }

    try {
      cobblemonEntity.setCustomNameVisible(false);
      syncCobblemonRenderState(entity, cobblemonEntity, speciesId);

      if (IntegrationRegistry.isGuiPreviewMode()) {
        float previewScale = getGuiPreviewScale(speciesId, cobblemonEntity);
        float yLift =
            Math.max(
                0f,
                (EntityTypeManager.GUI_PREVIEW_TARGET_HEIGHT
                        - previewScale * getRootScaleY(easyNPC) * cobblemonEntity.getBbHeight())
                    / 2f);
        poseStack.pushPose();
        poseStack.translate(0.0, yLift, 0.0);
        poseStack.scale(previewScale, previewScale, previewScale);
        EasyNPCLivingEntityRenderer.handleRotation(easyNPC, poseStack);
        EasyNPCLivingEntityRenderer.handleScale(easyNPC, poseStack);
        RendererManager.renderLivingEntity(
            entity,
            cobblemonEntity,
            modelString,
            livingEntityRenderer,
            entityYaw,
            partialTicks,
            poseStack,
            buffer,
            packedLight);
        poseStack.popPose();
      } else {
        poseStack.pushPose();
        EasyNPCLivingEntityRenderer.handleRotation(easyNPC, poseStack);
        EasyNPCLivingEntityRenderer.handleScale(easyNPC, poseStack);
        RendererManager.renderLivingEntity(
            entity,
            cobblemonEntity,
            modelString,
            livingEntityRenderer,
            entityYaw,
            partialTicks,
            poseStack,
            buffer,
            packedLight);
        poseStack.popPose();
      }
      return true;
    } catch (Exception exception) {
      log.error("Failed to render Cobblemon entity {} ({}):", modelString, speciesId, exception);
      invalidSpeciesCache.put(speciesId, Boolean.TRUE);
      cobblemonEntityCache.remove(speciesId);
      return false;
    }
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
  public void render(
      E entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight) {
    if (renderCobblemon(entity, entityYaw, partialTicks, poseStack, bufferSource, packedLight)) {
      if (this.shouldShowName(entity)) {
        this.renderNameTag(
            entity, entity.getDisplayName(), poseStack, bufferSource, packedLight, partialTicks);
      }
      Entity leashHolder = entity.getLeashHolder();
      if (leashHolder != null) {
        ((MobRendererInvoker) this)
            .invokeRenderLeash(entity, partialTicks, poseStack, bufferSource, leashHolder);
      }
      return;
    }
    super.render(entity, entityYaw, partialTicks, poseStack, bufferSource, packedLight);
  }
}
