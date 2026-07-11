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

package de.markusbordihn.easynpc.api.npc.base.piglin;

import de.markusbordihn.easynpc.api.npc.BaseEasyNPC;
import de.markusbordihn.easynpc.api.npc.raw.piglin.PiglinRaw;
import de.markusbordihn.easynpc.data.configuration.ConfigurationData;
import de.markusbordihn.easynpc.data.skin.variant.PiglinSkinVariant;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import java.util.List;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.Brain;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.entity.monster.piglin.Piglin;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import org.jspecify.annotations.Nullable;

public class PiglinBase extends PiglinRaw implements BaseEasyNPC<PiglinRaw> {

  public PiglinBase(EntityType<? extends Piglin> entityType, Level level) {
    this(entityType, level, PiglinSkinVariant.PIGLIN);
  }

  public PiglinBase(EntityType<? extends Piglin> entityType, Level level, Enum<?> variantType) {
    super(entityType, level, variantType);
    this.setInvulnerable(true);
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes()
        .add(Attributes.ARMOR_TOUGHNESS, 0.0D)
        .add(Attributes.ARMOR, 0.0D)
        .add(Attributes.ATTACK_DAMAGE, 1.0D)
        .add(Attributes.ATTACK_KNOCKBACK, 0.0D)
        .add(Attributes.ATTACK_SPEED, 0.0D)
        .add(Attributes.FOLLOW_RANGE, 32.0D)
        .add(Attributes.KNOCKBACK_RESISTANCE, 0.0D)
        .add(Attributes.MAX_HEALTH, 20.0D)
        .add(Attributes.MOVEMENT_SPEED, 0.5F)
        .add(Attributes.SPAWN_REINFORCEMENTS_CHANCE, 0.0D);
  }

  @Override
  public ConfigurationData getConfigurationData() {
    return ConfigurationData.STANDARD;
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    PiglinSkinVariant soundVariant = PiglinSkinVariant.valueOf(variantName);
    switch (soundVariant) {
      case PIGLIN_BRUTE:
        soundDataSet.addDefaultSound(SoundType.AMBIENT, SoundEvents.PIGLIN_BRUTE_AMBIENT);
        soundDataSet.addDefaultSound(SoundType.HURT, SoundEvents.PIGLIN_BRUTE_HURT);
        soundDataSet.addDefaultSound(SoundType.DEATH, SoundEvents.PIGLIN_BRUTE_DEATH);
        soundDataSet.addDefaultSound(SoundType.STEP, SoundEvents.PIGLIN_BRUTE_STEP);
        break;
      case ZOMBIFIED_PIGLIN:
        soundDataSet.addDefaultSound(SoundType.AMBIENT, SoundEvents.ZOMBIFIED_PIGLIN_AMBIENT);
        soundDataSet.addDefaultSound(SoundType.HURT, SoundEvents.ZOMBIFIED_PIGLIN_HURT);
        soundDataSet.addDefaultSound(SoundType.DEATH, SoundEvents.ZOMBIFIED_PIGLIN_DEATH);
        soundDataSet.addDefaultSound(SoundType.STEP, SoundEvents.PIGLIN_STEP);
        break;
      default:
        soundDataSet.addDefaultSound(SoundType.AMBIENT, SoundEvents.PIGLIN_AMBIENT);
        soundDataSet.addDefaultSound(SoundType.HURT, SoundEvents.PIGLIN_HURT);
        soundDataSet.addDefaultSound(SoundType.DEATH, SoundEvents.PIGLIN_DEATH);
        soundDataSet.addDefaultSound(SoundType.STEP, SoundEvents.PIGLIN_STEP);
    }
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
  @SuppressWarnings("deprecation")
  protected Brain<Piglin> makeBrain(Brain.Packed packedBrain) {
    return Brain.<Piglin>provider(
            List.of(MemoryModuleType.ATTACK_TARGET, MemoryModuleType.NEARBY_ADULT_PIGLINS),
            List.of(),
            entity -> List.of())
        .makeBrain(this, packedBrain);
  }

  @Override
  public void setTarget(@Nullable LivingEntity target) {
    super.setTarget(target);
    if (target != null) {
      this.getBrain().setMemory(MemoryModuleType.ATTACK_TARGET, target);
    } else {
      this.getBrain().eraseMemory(MemoryModuleType.ATTACK_TARGET);
    }
  }

  @Override
  public boolean isConverting() {
    return false;
  }

  @Override
  public void travel(Vec3 vec3) {

    this.handleNavigationTravelEvent(vec3);

    // Handle movement for NPC for specific conditions.
    if (this.hasTravelTargetObjectives()) {
      // Allow travel for NPC, if travel objectives are used.
      super.travel(vec3);
    } else {
      this.calculateEntityAnimation(this instanceof FlyingAnimal);
    }
  }
}
