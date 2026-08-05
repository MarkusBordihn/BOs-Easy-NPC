/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities;

import de.markusbordihn.easynpc.api.npc.raw.PathfinderMobRaw;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager.ProfileDimensions;
import de.markusbordihn.easynpc.data.attribute.NavigationType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationData;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelType;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import java.util.Objects;
import java.util.Set;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityDimensions;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.level.Level;

public class EasyModelNPC extends PathfinderMobRaw {

  public static final String ID = "easy_model_entities_npc";

  private String cachedProfileModel;
  private ResourceLocation cachedProfileId;
  private ResourceLocation dimensionsProfileId;
  private ProfileDimensions profileDimensions;

  public EasyModelNPC(EntityType<? extends PathfinderMob> entityType, Level level) {
    super(entityType, level, VariantType.EASY_MODEL_NPC);
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes()
        .add(Attributes.MAX_HEALTH, 20.0D)
        .add(Attributes.FOLLOW_RANGE, 32.0D)
        .add(Attributes.KNOCKBACK_RESISTANCE, 0.0D)
        .add(Attributes.MOVEMENT_SPEED, 0.4F)
        .add(Attributes.ATTACK_DAMAGE, 1.0D)
        .add(Attributes.ATTACK_KNOCKBACK, 0.0D)
        .add(Attributes.ATTACK_SPEED, 0.0D)
        .add(Attributes.ARMOR, 0.0D)
        .add(Attributes.ARMOR_TOUGHNESS, 0.0D);
  }

  @Override
  public void aiStep() {
    super.aiStep();

    ResourceLocation profileId = this.getEasyModelProfileId();
    ProfileDimensions dimensions = EasyModelEntitiesManager.getProfileDimensions(profileId);
    if (!Objects.equals(this.dimensionsProfileId, profileId)
        || !Objects.equals(this.profileDimensions, dimensions)) {
      this.dimensionsProfileId = profileId;
      this.profileDimensions = dimensions;
      this.refreshDimensions();
    }
  }

  @Override
  public EntityDimensions getDefaultDimensions(Pose pose) {
    ProfileDimensions dimensions =
        EasyModelEntitiesManager.getProfileDimensions(this.getEasyModelProfileId());
    if (dimensions == null) {
      return super.getDefaultDimensions(pose);
    }

    CustomScale rootScale = this.getModelRootData().scale();
    float height = dimensions.height() * rootScale.y();
    return EntityDimensions.scalable(dimensions.width() * rootScale.x(), height)
        .withEyeHeight(Math.min(dimensions.eyeHeight() * rootScale.y(), height));
  }

  public ResourceLocation getEasyModelProfileId() {
    RenderDataEntry renderDataEntry = this.getEasyNPCRenderData().getRenderDataEntry();
    String entityModel = renderDataEntry != null ? renderDataEntry.getRenderEntityModel() : null;
    if (this.cachedProfileId == null || !Objects.equals(this.cachedProfileModel, entityModel)) {
      this.cachedProfileModel = entityModel;
      this.cachedProfileId = EasyModelEntitiesManager.getProfileId(entityModel);
    }
    return this.cachedProfileId;
  }

  @Override
  public NavigationType defaultNavigationType() {
    return EasyModelEntitiesManager.isFloatingProfile(this.getEasyModelProfileId())
        ? NavigationType.FLYING
        : NavigationType.GROUND;
  }

  @Override
  public double defaultHoverHeight() {
    return EasyModelEntitiesManager.isFloatingProfile(this.getEasyModelProfileId())
        ? EasyModelEntitiesManager.DEFAULT_HOVER_HEIGHT
        : 0.0D;
  }

  @Override
  public ConfigurationData getConfigurationData() {
    Set<ModelPartType> modelParts = this.getModelType().getModelParts();
    if (modelParts.contains(ModelPartType.RIGHT_ARM)
        && modelParts.contains(ModelPartType.RIGHT_LEG)) {
      return ConfigurationData.EASY_MODEL_HUMANOID;
    }
    return ConfigurationData.EASY_MODEL;
  }

  @Override
  public void defineSynchedRenderData(SynchedEntityData.Builder builder) {
    this.defineSynchedEntityData(
        builder,
        SynchedDataIndex.RENDER_DATA,
        new RenderDataEntry(
            RenderType.EASY_MODEL_ENTITY, null, EasyModelEntitiesManager.DEFAULT_PROFILE));
  }

  @Override
  public ModelType getModelType() {
    RenderDataEntry renderDataEntry = this.getEasyNPCRenderData().getRenderDataEntry();
    ModelType renderModelType =
        renderDataEntry != null ? renderDataEntry.getRenderModelType() : null;
    return renderModelType != null ? renderModelType : ModelType.HUMANOID;
  }

  @Override
  public boolean canUseArmor() {
    return false;
  }

  @Override
  public Enum<?>[] getSkinVariantTypes() {
    return VariantType.values();
  }

  @Override
  public Enum<?> getDefaultSkinVariantType() {
    return VariantType.EASY_MODEL_NPC;
  }

  @Override
  public Enum<?> getSkinVariantType(String name) {
    try {
      return VariantType.valueOf(name);
    } catch (IllegalArgumentException e) {
      return this.getDefaultSkinVariantType();
    }
  }

  public enum VariantType {
    EASY_MODEL_NPC,
  }
}
