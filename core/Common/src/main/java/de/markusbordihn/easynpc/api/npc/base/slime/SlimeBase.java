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

package de.markusbordihn.easynpc.api.npc.base.slime;

import de.markusbordihn.easynpc.api.npc.BaseEasyNPC;
import de.markusbordihn.easynpc.api.npc.raw.SlimeRaw;
import de.markusbordihn.easynpc.data.configuration.ConfigurationData;
import de.markusbordihn.easynpc.data.skin.variant.SlimeSkinVariant;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.easynpc.ai.control.JumpEasyNPCMoveControl;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.entity.monster.Slime;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

public class SlimeBase extends SlimeRaw implements BaseEasyNPC<SlimeRaw> {

  private boolean customParticlesEnabled = false;

  public SlimeBase(EntityType<? extends Slime> entityType, Level level) {
    this(entityType, level, SlimeSkinVariant.SLIME);
  }

  public SlimeBase(EntityType<? extends Slime> entityType, Level level, Enum<?> variantType) {
    this(entityType, level, variantType, 2);
  }

  public SlimeBase(
      EntityType<? extends Slime> entityType, Level level, Enum<?> variantType, int size) {
    super(entityType, level, variantType);
    this.setSize(size, false);
    this.setInvulnerable(true);
    this.moveControl = new JumpEasyNPCMoveControl(this);
    this.refreshGroundNavigation();
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
        .add(Attributes.MOVEMENT_SPEED, 0.6F)
        .add(Attributes.SPAWN_REINFORCEMENTS_CHANCE, 0.0D);
  }

  public boolean isCustomParticlesEnabled() {
    return this.customParticlesEnabled;
  }

  public void setCustomParticlesEnabled(boolean customParticlesEnabled) {
    this.customParticlesEnabled = customParticlesEnabled;
  }

  protected boolean spawnCustomParticles() {
    return this.customParticlesEnabled;
  }

  @Override
  public boolean canJump() {
    return true;
  }

  public int getSlimeJumpDelay() {
    return this.getJumpDelay();
  }

  public boolean shouldPlayJumpSound() {
    return this.doPlayJumpSound();
  }

  @Override
  public boolean canUseOffHand() {
    return false;
  }

  @Override
  public int getEntityDialogTop() {
    return -45;
  }

  @Override
  public ConfigurationData getConfigurationData() {
    return ConfigurationData.STANDARD;
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    soundDataSet.addDefaultSound(SoundType.DEATH, SoundEvents.SLIME_DEATH);
    soundDataSet.addDefaultSound(SoundType.HURT, SoundEvents.SLIME_HURT);
    soundDataSet.addDefaultSound(SoundType.STEP, SoundEvents.SLIME_SQUISH);
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
}
