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

package de.markusbordihn.easynpc.entity.easynpc.npc;

import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.EasyNPCBaseModelEntity;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.entity.EntityDimensions;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.level.Level;

public class Humanoid extends EasyNPCBaseModelEntity<Humanoid> {

  public static final String ID = "humanoid";

  public Humanoid(EntityType<? extends PathfinderMob> entityType, Level level) {
    this(entityType, level, Variant.STEVE);
  }

  public Humanoid(EntityType<? extends PathfinderMob> entityType, Level level, Enum<?> variant) {
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
  public Enum<?>[] getVariants() {
    return Variant.values();
  }

  @Override
  public Enum<?> getDefaultVariant() {
    return Variant.STEVE;
  }

  @Override
  public Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

  @Override
  public boolean supportsPlayerSkinConfiguration() {
    return true;
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    soundDataSet.addSound(SoundType.DEATH, SoundEvents.PLAYER_DEATH);
    soundDataSet.addSound(SoundType.HURT, SoundEvents.PLAYER_HURT);
    soundDataSet.addSound(SoundType.EAT, SoundEvents.GENERIC_EAT);
    soundDataSet.addSound(SoundType.TRADE, SoundEvents.VILLAGER_TRADE);
    soundDataSet.addSound(SoundType.TRADE_YES, SoundEvents.VILLAGER_YES);
    soundDataSet.addSound(SoundType.TRADE_NO, SoundEvents.VILLAGER_NO);
    return soundDataSet;
  }

  @Override
  public float getStandingEyeHeight(Pose pose, EntityDimensions dimensions) {
    return switch (pose) {
      case SWIMMING, FALL_FLYING, SPIN_ATTACK -> 0.4F;
      case CROUCHING -> 1.27F;
      default -> 1.62F;
    };
  }

  public enum Variant {
    ALEX,
    ARI,
    EFE,
    KAI,
    MAKENA,
    NOOR,
    STEVE,
    SUNNY,
    ZURI,
    JAYJASONBO,
    PROFESSOR_01,
    SECURITY_01,
    KNIGHT_01,
    KNIGHT_02,
  }
}
