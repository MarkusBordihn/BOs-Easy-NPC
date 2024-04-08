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

public class Horse extends EasyNPCBaseEntity<Horse> {

  public static final String ID = "horse";
  public static final String ID_ZOMBIE = "zombie_horse";
  public static final String ID_SKELETON = "skeleton_horse";
  public static final String NAME = "Horse";

  public Horse(EntityType<? extends PathfinderMob> entityType, Level level) {
    this(entityType, level, Variant.WHITE);
  }

  public Horse(EntityType<? extends PathfinderMob> entityType, Level level, Enum<?> variant) {
    super(entityType, level, variant);
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes()
        .add(Attributes.MAX_HEALTH, 8.0D)
        .add(Attributes.FOLLOW_RANGE, 32.0D)
        .add(Attributes.KNOCKBACK_RESISTANCE, 0.0D)
        .add(Attributes.MOVEMENT_SPEED, 0.3F)
        .add(Attributes.ATTACK_DAMAGE, 2.0D)
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
  public boolean canUseOffHand() {
    return false;
  }

  @Override
  public boolean canUseMainHand() {
    return false;
  }

  @Override
  public SkinModel getSkinModel() {
    return SkinModel.WOLF;
  }

  @Override
  public Enum<?>[] getVariants() {
    return Variant.values();
  }

  @Override
  public Enum<?> getDefaultVariant() {
    return Variant.WHITE;
  }

  @Override
  public Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

  @Override
  public int getEntityGuiTop() {
    return -15;
  }

  @Override
  public int getEntityDialogTop() {
    return -37;
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.HORSE_AMBIENT);
    soundDataSet.addSound(SoundType.DEATH, SoundEvents.HORSE_DEATH);
    soundDataSet.addSound(SoundType.EAT, SoundEvents.HORSE_EAT);
    soundDataSet.addSound(SoundType.HURT, SoundEvents.HORSE_HURT);
    soundDataSet.addSound(SoundType.STEP, SoundEvents.HORSE_STEP);
    soundDataSet.addSound(SoundType.TRADE, SoundEvents.VILLAGER_TRADE);
    soundDataSet.addSound(SoundType.TRADE_YES, SoundEvents.VILLAGER_YES);
    soundDataSet.addSound(SoundType.TRADE_NO, SoundEvents.VILLAGER_NO);
    return soundDataSet;
  }

  public enum Variant {
    WHITE,
    WHITE_SADDLED,
    CREAMY,
    CREAMY_SADDLED,
    CHESTNUT,
    CHESTNUT_SADDLED,
    BROWN,
    BROWN_SADDLED,
    BLACK,
    BLACK_SADDLED,
    GRAY,
    GRAY_SADDLED,
    DARKBROWN,
    DARKBROWN_SADDLED,
    SKELETON,
    SKELETON_SADDLED,
    ZOMBIE,
    ZOMBIE_SADDLED,
    DONKEY,
    DONKEY_SADDLED,
    MULE,
    MULE_SADDLED,
  }
}
