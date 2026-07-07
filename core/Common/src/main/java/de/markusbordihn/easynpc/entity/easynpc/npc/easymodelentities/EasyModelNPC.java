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
import de.markusbordihn.easynpc.data.configuration.ConfigurationData;
import de.markusbordihn.easynpc.data.model.ModelType;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import java.util.Objects;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.level.Level;

public class EasyModelNPC extends PathfinderMobRaw {

  public static final String ID = "easy_model_entities_npc";

  private String cachedProfileModel;
  private ResourceLocation cachedProfileId;

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
  public ConfigurationData getConfigurationData() {
    return ConfigurationData.EASY_MODEL;
  }

  @Override
  public void defineSynchedRenderData() {
    this.defineSynchedEntityData(
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
