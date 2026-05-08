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

package de.markusbordihn.easynpc.api.npc.base.ghast;

import de.markusbordihn.easynpc.api.npc.BaseEasyNPC;
import de.markusbordihn.easynpc.api.npc.raw.GhastRaw;
import de.markusbordihn.easynpc.data.configuration.ConfigurationData;
import de.markusbordihn.easynpc.data.skin.variant.GhastSkinVariant;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.DifficultyInstance;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.SpawnGroupData;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.control.FlyingMoveControl;
import net.minecraft.world.entity.ai.navigation.FlyingPathNavigation;
import net.minecraft.world.entity.ai.navigation.PathNavigation;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.entity.monster.Ghast;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.ServerLevelAccessor;
import net.minecraft.world.phys.Vec3;

public class GhastBase extends GhastRaw implements BaseEasyNPC<GhastRaw> {

  public GhastBase(EntityType<? extends Ghast> entityType, Level level) {
    this(entityType, level, GhastSkinVariant.GHAST);
  }

  public GhastBase(EntityType<? extends Ghast> entityType, Level level, Enum<?> variantType) {
    super(entityType, level, variantType);
    this.setInvulnerable(true);
    this.moveControl = new FlyingMoveControl(this, 20, true);
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes()
        .add(Attributes.ARMOR_TOUGHNESS, 0.0D)
        .add(Attributes.ARMOR, 0.0D)
        .add(Attributes.ATTACK_DAMAGE, 0.0D)
        .add(Attributes.ATTACK_KNOCKBACK, 0.0D)
        .add(Attributes.ATTACK_SPEED, 0.0D)
        .add(Attributes.FOLLOW_RANGE, 32.0D)
        .add(Attributes.KNOCKBACK_RESISTANCE, 0.0D)
        .add(Attributes.MAX_HEALTH, 16.0D)
        .add(Attributes.MOVEMENT_SPEED, 0.3F)
        .add(Attributes.FLYING_SPEED, 0.4F)
        .add(Attributes.SPAWN_REINFORCEMENTS_CHANCE, 0.0D);
  }

  @Override
  protected PathNavigation createNavigation(Level level) {
    return new FlyingPathNavigation(this, level);
  }

  @Override
  public boolean canFly() {
    return true;
  }

  @Override
  public boolean canUseOffHand() {
    return false;
  }

  @Override
  public int getEntityDialogTop() {
    return -55;
  }

  @Override
  public ConfigurationData getConfigurationData() {
    return ConfigurationData.STANDARD;
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    soundDataSet.addDefaultSound(SoundType.AMBIENT, SoundEvents.GHAST_AMBIENT);
    soundDataSet.addDefaultSound(SoundType.DEATH, SoundEvents.GHAST_DEATH);
    soundDataSet.addDefaultSound(SoundType.HURT, SoundEvents.GHAST_HURT);
    soundDataSet.addDefaultSound(SoundType.TRADE, SoundEvents.VILLAGER_TRADE);
    soundDataSet.addDefaultSound(SoundType.TRADE_YES, SoundEvents.VILLAGER_YES);
    soundDataSet.addDefaultSound(SoundType.TRADE_NO, SoundEvents.VILLAGER_NO);
    return soundDataSet;
  }

  @Override
  protected void registerGoals() {
    // No default goals for base NPCs.
  }

  @Override
  public void travel(Vec3 vec3) {

    this.handleNavigationTravelEvent(vec3);

    // Handle movement for NPC for specific conditions.
    if (this.hasTravelTargetObjectives()) {
      // Allow travel for NPC, if travel objectives are used.
      super.travel(vec3);
    } else {
      // Make sure we only calculate animations for be as much as possible server-friendly.
      this.calculateEntityAnimation(this instanceof FlyingAnimal);
    }
  }

  @Override
  public SpawnGroupData finalizeSpawn(
      ServerLevelAccessor serverLevelAccessor,
      DifficultyInstance difficulty,
      MobSpawnType mobSpawnType,
      SpawnGroupData spawnGroupData) {
    this.setPos(this.getX(), this.getY() + 1.0D, this.getZ());
    SpawnGroupData result =
        super.finalizeSpawn(serverLevelAccessor, difficulty, mobSpawnType, spawnGroupData);
    this.refreshDimensions();
    return result;
  }
}
