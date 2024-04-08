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

public class Cat extends EasyNPCBaseEntity<Cat> {

  public static final String ID = "cat";
  public static final String NAME = "Cat";

  public Cat(EntityType<? extends PathfinderMob> entityType, Level level) {
    super(entityType, level, Variant.BLACK);
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes()
        .add(Attributes.MAX_HEALTH, 16.0D)
        .add(Attributes.FOLLOW_RANGE, 32.0D)
        .add(Attributes.KNOCKBACK_RESISTANCE, 0.0D)
        .add(Attributes.MOVEMENT_SPEED, 0.5F)
        .add(Attributes.ATTACK_DAMAGE, 0.0D)
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
  public SkinModel getSkinModel() {
    return SkinModel.CAT;
  }

  @Override
  public Enum<?>[] getVariants() {
    return Variant.values();
  }

  @Override
  public Enum<?> getDefaultVariant() {
    return Variant.BLACK;
  }

  @Override
  public Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

  @Override
  public int getEntityGuiScaling() {
    return 55;
  }

  @Override
  public int getEntityGuiTop() {
    return -10;
  }

  @Override
  public int getEntityDialogTop() {
    return -37;
  }

  @Override
  public int getEntityDialogScaling() {
    return 65;
  }

  @Override
  public boolean supportsSmartAnimations() {
    return false;
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.CAT_AMBIENT);
    soundDataSet.addSound(SoundType.AMBIENT_TAMED, SoundEvents.CAT_PURREOW);
    soundDataSet.addSound(SoundType.AMBIENT_STRAY, SoundEvents.CAT_STRAY_AMBIENT);
    soundDataSet.addSound(SoundType.DEATH, SoundEvents.CAT_DEATH);
    soundDataSet.addSound(SoundType.HURT, SoundEvents.CAT_HURT);
    soundDataSet.addSound(SoundType.EAT, SoundEvents.CAT_EAT);
    soundDataSet.addSound(SoundType.TRADE, SoundEvents.VILLAGER_TRADE);
    soundDataSet.addSound(SoundType.TRADE_YES, SoundEvents.VILLAGER_YES);
    soundDataSet.addSound(SoundType.TRADE_NO, SoundEvents.VILLAGER_NO);
    return soundDataSet;
  }

  public enum Variant {
    ALL_BLACK,
    BLACK,
    BRITISH_SHORTHAIR,
    CALICO,
    JELLIE,
    OCELOT,
    PERSIAN,
    RAGDOLL,
    RED,
    SIAMESE,
    TABBY,
    WHITE
  }
}
