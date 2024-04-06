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

import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.EasyNPCBaseEntity;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.level.Level;

public class Zombie extends EasyNPCBaseEntity<Zombie> {

  public static final String ID = "zombie";
  public static final String ID_DROWNED = "drowned";
  public static final String ID_HUSK = "husk";
  public static final String NAME = "Zombie";

  public Zombie(EntityType<? extends PathfinderMob> entityType, Level level) {
    this(entityType, level, Variant.ZOMBIE);
  }

  public Zombie(EntityType<? extends PathfinderMob> entityType, Level level, Enum<?> variant) {
    super(entityType, level, variant);
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes()
        .add(Attributes.MAX_HEALTH, 20.0D)
        .add(Attributes.FOLLOW_RANGE, 32.0D)
        .add(Attributes.KNOCKBACK_RESISTANCE, 0.0D)
        .add(Attributes.MOVEMENT_SPEED, 0.7F)
        .add(Attributes.ATTACK_DAMAGE, 1.0D)
        .add(Attributes.ATTACK_KNOCKBACK, 0.0D)
        .add(Attributes.ATTACK_SPEED, 0.0D)
        .add(Attributes.ARMOR, 0.0D)
        .add(Attributes.ARMOR_TOUGHNESS, 0.0D);
  }

  @Override
  public SkinModel getSkinModel() {
    return SkinModel.ZOMBIE;
  }

  @Override
  public Enum<?>[] getVariants() {
    return Variant.values();
  }

  @Override
  public Enum<?> getDefaultVariant() {
    return Variant.ZOMBIE;
  }

  @Override
  public Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    Variant soundVariant = Variant.valueOf(variantName);
    switch (soundVariant) {
      case DROWNED:
        soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.DROWNED_AMBIENT);
        soundDataSet.addSound(SoundType.HURT, SoundEvents.DROWNED_HURT);
        soundDataSet.addSound(SoundType.DEATH, SoundEvents.DROWNED_DEATH);
        soundDataSet.addSound(SoundType.STEP, SoundEvents.DROWNED_STEP);
        break;
      case HUSK:
        soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.HUSK_AMBIENT);
        soundDataSet.addSound(SoundType.HURT, SoundEvents.HUSK_HURT);
        soundDataSet.addSound(SoundType.DEATH, SoundEvents.HUSK_DEATH);
        soundDataSet.addSound(SoundType.STEP, SoundEvents.HUSK_STEP);
        break;
      case ZOMBIE:
      default:
        soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.ZOMBIE_AMBIENT);
        soundDataSet.addSound(SoundType.HURT, SoundEvents.ZOMBIE_HURT);
        soundDataSet.addSound(SoundType.DEATH, SoundEvents.ZOMBIE_DEATH);
        soundDataSet.addSound(SoundType.STEP, SoundEvents.ZOMBIE_STEP);
        break;
    }
    return soundDataSet;
  }

  // Skin Details
  public enum Variant {
    DROWNED,
    HUSK,
    ZOMBIE
  }
}
