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

public class Skeleton extends EasyNPCBaseEntity<Skeleton> {

  public static final String ID = "skeleton";
  public static final String ID_STRAY = "stray";
  public static final String ID_WITHER_SKELETON = "wither_skeleton";
  public static final String NAME = "Skeleton";

  public Skeleton(EntityType<? extends PathfinderMob> entityType, Level level) {
    this(entityType, level, Variant.SKELETON);
  }

  public Skeleton(EntityType<? extends PathfinderMob> entityType, Level level, Enum<?> variant) {
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
    return SkinModel.SKELETON;
  }

  @Override
  public Enum<?>[] getVariants() {
    return Variant.values();
  }

  @Override
  public Enum<?> getDefaultVariant() {
    return Variant.SKELETON;
  }

  @Override
  public Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    Variant soundVariant = Variant.valueOf(variantName);
    switch (soundVariant) {
      case STRAY:
        soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.STRAY_AMBIENT);
        soundDataSet.addSound(SoundType.DEATH, SoundEvents.STRAY_DEATH);
        soundDataSet.addSound(SoundType.HURT, SoundEvents.STRAY_HURT);
        soundDataSet.addSound(SoundType.STEP, SoundEvents.STRAY_STEP);
        break;
      case WITHER_SKELETON:
        soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.WITHER_SKELETON_AMBIENT);
        soundDataSet.addSound(SoundType.DEATH, SoundEvents.WITHER_SKELETON_DEATH);
        soundDataSet.addSound(SoundType.HURT, SoundEvents.WITHER_SKELETON_HURT);
        soundDataSet.addSound(SoundType.STEP, SoundEvents.WITHER_SKELETON_STEP);
        break;
      case SKELETON:
      default:
        soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.SKELETON_AMBIENT);
        soundDataSet.addSound(SoundType.DEATH, SoundEvents.SKELETON_DEATH);
        soundDataSet.addSound(SoundType.HURT, SoundEvents.SKELETON_HURT);
        soundDataSet.addSound(SoundType.STEP, SoundEvents.SKELETON_STEP);
    }
    return soundDataSet;
  }

  // Skin Details
  public enum Variant {
    SKELETON,
    STRAY,
    WITHER_SKELETON
  }
}
