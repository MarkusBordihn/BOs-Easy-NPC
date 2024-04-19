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
import de.markusbordihn.easynpc.entity.EasyNPCBaseModelEntity;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.level.Level;

public class IronGolem extends EasyNPCBaseModelEntity<IronGolem> {

  public static final String ID = "iron_golem";

  public IronGolem(EntityType<? extends PathfinderMob> entityType, Level level) {
    super(entityType, level, Variant.IRON_GOLEM);
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes()
        .add(Attributes.MAX_HEALTH, 100.0D)
        .add(Attributes.FOLLOW_RANGE, 32.0D)
        .add(Attributes.KNOCKBACK_RESISTANCE, 1.0D)
        .add(Attributes.MOVEMENT_SPEED, 0.25F)
        .add(Attributes.FLYING_SPEED, 0.3F)
        .add(Attributes.ATTACK_DAMAGE, 15.0D)
        .add(Attributes.ATTACK_KNOCKBACK, 1.0D)
        .add(Attributes.ATTACK_SPEED, 4.0D)
        .add(Attributes.ARMOR, 0.0D)
        .add(Attributes.ARMOR_TOUGHNESS, 0.0D);
  }

  @Override
  public boolean canUseArmor() {
    return false;
  }

  @Override
  public SkinModel getSkinModel() {
    return SkinModel.IRON_GOLEM;
  }

  @Override
  public Enum<?>[] getVariants() {
    return Variant.values();
  }

  @Override
  public Enum<?> getDefaultVariant() {
    return Variant.IRON_GOLEM;
  }

  @Override
  public Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

  @Override
  public int getEntityGuiScaling() {
    return 40;
  }

  @Override
  public int getEntityDialogScaling() {
    return 38;
  }

  @Override
  public int getEntitySkinScaling() {
    return 25;
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    soundDataSet.addSound(SoundType.DAMAGE, SoundEvents.IRON_GOLEM_DAMAGE);
    soundDataSet.addSound(SoundType.DEATH, SoundEvents.IRON_GOLEM_DEATH);
    soundDataSet.addSound(SoundType.HURT, SoundEvents.IRON_GOLEM_HURT);
    soundDataSet.addSound(SoundType.STEP, SoundEvents.IRON_GOLEM_STEP);
    soundDataSet.addSound(SoundType.TRADE, SoundEvents.VILLAGER_TRADE);
    soundDataSet.addSound(SoundType.TRADE_YES, SoundEvents.VILLAGER_YES);
    soundDataSet.addSound(SoundType.TRADE_NO, SoundEvents.VILLAGER_NO);
    return soundDataSet;
  }

  public enum Variant {
    IRON_GOLEM,
    IRON_GOLEM_CRACKINESS_HIGH,
    IRON_GOLEM_CRACKINESS_MEDIUM,
    IRON_GOLEM_CRACKINESS_LOW,
  }
}
