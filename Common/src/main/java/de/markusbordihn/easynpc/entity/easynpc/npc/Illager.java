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

public class Illager extends EasyNPCBaseEntity<Illager> {

  public static final String ID = "illager";
  public static final String ID_EVOKER = "evoker";
  public static final String ID_ILLUSIONER = "illusioner";
  public static final String ID_PILLAGER = "pillager";
  public static final String ID_VINDICATOR = "vindicator";
  public static final String NAME = "Illager";

  public Illager(EntityType<? extends PathfinderMob> entityType, Level level, Enum<?> variant) {
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
  public boolean canUseArmor() {
    return false;
  }

  @Override
  public boolean hasArmsModelPart() {
    return true;
  }

  @Override
  public SkinModel getSkinModel() {
    return SkinModel.ILLAGER;
  }

  @Override
  public Enum<?>[] getVariants() {
    return Variant.values();
  }

  @Override
  public Enum<?> getDefaultVariant() {
    return Variant.PILLAGER;
  }

  @Override
  public Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    Variant soundVariant = Variant.valueOf(variantName);
    switch (soundVariant) {
      case EVOKER, EVOKER_CROSSED_ARMS:
        soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.EVOKER_AMBIENT);
        soundDataSet.addSound(SoundType.DEATH, SoundEvents.EVOKER_DEATH);
        soundDataSet.addSound(SoundType.HURT, SoundEvents.EVOKER_HURT);
        soundDataSet.addSound(SoundType.CAST_SPELL, SoundEvents.EVOKER_CAST_SPELL);
        soundDataSet.addSound(SoundType.CELEBRATE, SoundEvents.EVOKER_CELEBRATE);
        break;
      case ILLUSIONER, ILLUSIONER_CROSSED_ARMS:
        soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.ILLUSIONER_AMBIENT);
        soundDataSet.addSound(SoundType.DEATH, SoundEvents.ILLUSIONER_DEATH);
        soundDataSet.addSound(SoundType.HURT, SoundEvents.ILLUSIONER_HURT);
        soundDataSet.addSound(SoundType.CAST_SPELL, SoundEvents.ILLUSIONER_CAST_SPELL);
        soundDataSet.addSound(SoundType.CELEBRATE, SoundEvents.PILLAGER_CELEBRATE);
        break;
      case VINDICATOR, VINDICATOR_CROSSED_ARMS:
        soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.VINDICATOR_AMBIENT);
        soundDataSet.addSound(SoundType.DEATH, SoundEvents.VINDICATOR_DEATH);
        soundDataSet.addSound(SoundType.HURT, SoundEvents.VINDICATOR_HURT);
        soundDataSet.addSound(SoundType.CELEBRATE, SoundEvents.VINDICATOR_CELEBRATE);
        break;
      case PILLAGER:
      default:
        soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.PILLAGER_AMBIENT);
        soundDataSet.addSound(SoundType.DEATH, SoundEvents.PILLAGER_DEATH);
        soundDataSet.addSound(SoundType.HURT, SoundEvents.PILLAGER_HURT);
        soundDataSet.addSound(SoundType.CELEBRATE, SoundEvents.PILLAGER_CELEBRATE);
        break;
    }
    return soundDataSet;
  }

  // Skin Details
  public enum Variant {
    EVOKER,
    EVOKER_CROSSED_ARMS,
    ILLUSIONER,
    ILLUSIONER_CROSSED_ARMS,
    PILLAGER,
    VINDICATOR,
    VINDICATOR_CROSSED_ARMS
  }
}
