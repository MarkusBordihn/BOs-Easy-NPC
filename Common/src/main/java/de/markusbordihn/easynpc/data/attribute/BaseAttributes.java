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

package de.markusbordihn.easynpc.data.attribute;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attributes;

public class BaseAttributes {

  public static final String ATTACK_DAMAGE_TAG = BaseAttributeType.ATTACK_DAMAGE.getTagName();
  public static final String ATTACK_KNOCKBACK_TAG = BaseAttributeType.ATTACK_KNOCKBACK.getTagName();
  // Base Attributes Tags
  public static final String BASE_ATTRIBUTES_TAG = "BaseAttributes";
  public static final String FOLLOW_RANGE_TAG = BaseAttributeType.FOLLOW_RANGE.getTagName();
  public static final String KNOCKBACK_RESISTANCE_TAG =
      BaseAttributeType.KNOCKBACK_RESISTANCE.getTagName();
  private double attackDamage = 2.0F;
  private double attackKnockback = 0.0F;
  // Base Attributes
  private double followRange = 32.0F;
  private double knockbackResistance = 0.0F;

  public BaseAttributes() {}

  public BaseAttributes(final CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public BaseAttributes(LivingEntity livingEntity) {
    if (livingEntity == null) {
      return;
    }
    if (livingEntity.getAttribute(Attributes.FOLLOW_RANGE) != null) {
      this.setFollowRange(livingEntity.getAttribute(Attributes.FOLLOW_RANGE).getBaseValue());
    }
    if (livingEntity.getAttribute(Attributes.KNOCKBACK_RESISTANCE) != null) {
      this.setKnockbackResistance(
          livingEntity.getAttribute(Attributes.KNOCKBACK_RESISTANCE).getBaseValue());
    }
    if (livingEntity.getAttribute(Attributes.ATTACK_DAMAGE) != null) {
      this.setAttackDamage(livingEntity.getAttribute(Attributes.ATTACK_DAMAGE).getBaseValue());
    }
    if (livingEntity.getAttribute(Attributes.ATTACK_KNOCKBACK) != null) {
      this.setAttackKnockback(
          livingEntity.getAttribute(Attributes.ATTACK_KNOCKBACK).getBaseValue());
    }
  }

  public void load(final CompoundTag compoundTag) {
    if (!compoundTag.contains(BASE_ATTRIBUTES_TAG)) {
      return;
    }
    CompoundTag baseAttributesTag = compoundTag.getCompound(BASE_ATTRIBUTES_TAG);
    if (baseAttributesTag.contains(FOLLOW_RANGE_TAG)) {
      this.setFollowRange(baseAttributesTag.getDouble(FOLLOW_RANGE_TAG));
    }
    if (baseAttributesTag.contains(KNOCKBACK_RESISTANCE_TAG)) {
      this.setKnockbackResistance(baseAttributesTag.getDouble(KNOCKBACK_RESISTANCE_TAG));
    }
    if (baseAttributesTag.contains(ATTACK_DAMAGE_TAG)) {
      this.setAttackDamage(baseAttributesTag.getDouble(ATTACK_DAMAGE_TAG));
    }
    if (baseAttributesTag.contains(ATTACK_KNOCKBACK_TAG)) {
      this.setAttackKnockback(baseAttributesTag.getDouble(ATTACK_KNOCKBACK_TAG));
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    CompoundTag baseAttributesTag = new CompoundTag();
    baseAttributesTag.putDouble(FOLLOW_RANGE_TAG, (float) this.getFollowRange());
    baseAttributesTag.putDouble(KNOCKBACK_RESISTANCE_TAG, (float) this.getKnockbackResistance());
    baseAttributesTag.putDouble(ATTACK_DAMAGE_TAG, (float) this.getAttackDamage());
    baseAttributesTag.putDouble(ATTACK_KNOCKBACK_TAG, (float) this.getAttackKnockback());
    compoundTag.put(BASE_ATTRIBUTES_TAG, baseAttributesTag);
    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  public double getFollowRange() {
    return this.followRange;
  }

  public void setFollowRange(final double followRange) {
    this.followRange = followRange;
  }

  public double getKnockbackResistance() {
    return this.knockbackResistance;
  }

  public void setKnockbackResistance(final double knockbackResistance) {
    this.knockbackResistance = knockbackResistance;
  }

  public double getAttackDamage() {
    return this.attackDamage;
  }

  public void setAttackDamage(final double attackDamage) {
    this.attackDamage = attackDamage;
  }

  public double getAttackKnockback() {
    return this.attackKnockback;
  }

  public void setAttackKnockback(final double attackKnockback) {
    this.attackKnockback = attackKnockback;
  }
}
