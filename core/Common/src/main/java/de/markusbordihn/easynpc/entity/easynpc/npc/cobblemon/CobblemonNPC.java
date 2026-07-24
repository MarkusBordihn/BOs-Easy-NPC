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

package de.markusbordihn.easynpc.entity.easynpc.npc.cobblemon;

import de.markusbordihn.easynpc.api.npc.raw.PathfinderMobRaw;
import de.markusbordihn.easynpc.compat.cobblemon.CobblemonSpeciesManager;
import de.markusbordihn.easynpc.data.configuration.ConfigurationData;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.level.Level;

public class CobblemonNPC extends PathfinderMobRaw {

  public static final String ID = "cobblemon_npc";

  public CobblemonNPC(EntityType<? extends PathfinderMob> entityType, Level level) {
    this(entityType, level, VariantType.COBBLEMON_NPC);
  }

  public CobblemonNPC(
      EntityType<? extends PathfinderMob> entityType, Level level, Enum<?> variant) {
    super(entityType, level, variant);
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes()
        .add(Attributes.MAX_HEALTH, 20.0D)
        .add(Attributes.FOLLOW_RANGE, 32.0D)
        .add(Attributes.KNOCKBACK_RESISTANCE, 0.0D)
        .add(Attributes.MOVEMENT_SPEED, 0.6F)
        .add(Attributes.ATTACK_DAMAGE, 1.0D)
        .add(Attributes.ATTACK_KNOCKBACK, 0.0D)
        .add(Attributes.ATTACK_SPEED, 0.0D)
        .add(Attributes.ARMOR, 0.0D)
        .add(Attributes.ARMOR_TOUGHNESS, 0.0D);
  }

  @Override
  public void defineSynchedRenderData(SynchedEntityData.Builder builder) {
    this.defineSynchedEntityData(
        builder,
        SynchedDataIndex.RENDER_DATA,
        new RenderDataEntry(
            RenderType.COBBLEMON_ENTITY, null, CobblemonSpeciesManager.DEFAULT_MODEL));
  }

  @Override
  public ConfigurationData getConfigurationData() {
    return ConfigurationData.COBBLEMON;
  }

  @Override
  public Enum<?>[] getSkinVariantTypes() {
    return VariantType.values();
  }

  @Override
  public Enum<?> getDefaultSkinVariantType() {
    return VariantType.COBBLEMON_NPC;
  }

  @Override
  public Enum<?> getSkinVariantType(String name) {
    try {
      return VariantType.valueOf(name);
    } catch (IllegalArgumentException e) {
      return getDefaultSkinVariantType();
    }
  }

  public enum VariantType {
    COBBLEMON_NPC,
  }
}
