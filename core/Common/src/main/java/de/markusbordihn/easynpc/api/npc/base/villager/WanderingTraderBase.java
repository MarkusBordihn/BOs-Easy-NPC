/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.api.npc.base.villager;

import de.markusbordihn.easynpc.api.npc.BaseEasyNPC;
import de.markusbordihn.easynpc.api.npc.raw.villager.WanderingTraderRaw;
import de.markusbordihn.easynpc.data.configuration.ConfigurationData;
import de.markusbordihn.easynpc.data.skin.variant.WanderingTraderSkinVariant;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.entity.npc.WanderingTrader;
import net.minecraft.world.item.trading.MerchantOffers;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

public class WanderingTraderBase extends WanderingTraderRaw
    implements BaseEasyNPC<WanderingTraderRaw> {

  public WanderingTraderBase(EntityType<? extends WanderingTrader> entityType, Level level) {
    this(entityType, level, WanderingTraderSkinVariant.WANDERING_TRADER);
  }

  public WanderingTraderBase(
      EntityType<? extends WanderingTrader> entityType, Level level, Enum<?> variantType) {
    super(entityType, level, variantType);
    this.setInvulnerable(true);
    this.refreshGroundNavigation();
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes()
        .add(Attributes.ARMOR_TOUGHNESS, 0.0D)
        .add(Attributes.ARMOR, 0.0D)
        .add(Attributes.ATTACK_DAMAGE, 0.5D)
        .add(Attributes.ATTACK_KNOCKBACK, 0.0D)
        .add(Attributes.ATTACK_SPEED, 0.0D)
        .add(Attributes.FOLLOW_RANGE, 32.0D)
        .add(Attributes.KNOCKBACK_RESISTANCE, 0.0D)
        .add(Attributes.MAX_HEALTH, 20.0D)
        .add(Attributes.MOVEMENT_SPEED, 0.5D)
        .add(Attributes.SPAWN_REINFORCEMENTS_CHANCE, 0.0D);
  }

  @Override
  public boolean canUseOffHand() {
    return false;
  }

  @Override
  public ConfigurationData getConfigurationData() {
    return ConfigurationData.STANDARD;
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    soundDataSet.addDefaultSound(SoundType.AMBIENT, SoundEvents.WANDERING_TRADER_AMBIENT);
    soundDataSet.addDefaultSound(SoundType.DEATH, SoundEvents.WANDERING_TRADER_DEATH);
    soundDataSet.addDefaultSound(SoundType.HURT, SoundEvents.WANDERING_TRADER_HURT);
    soundDataSet.addDefaultSound(SoundType.TRADE, SoundEvents.WANDERING_TRADER_TRADE);
    soundDataSet.addDefaultSound(SoundType.TRADE_YES, SoundEvents.WANDERING_TRADER_YES);
    soundDataSet.addDefaultSound(SoundType.TRADE_NO, SoundEvents.WANDERING_TRADER_NO);
    return soundDataSet;
  }

  @Override
  protected void registerGoals() {
    super.registerGoals();
    this.goalSelector.removeAllGoals(goal -> true);
    this.targetSelector.removeAllGoals(goal -> true);
  }

  @Override
  public MerchantOffers getOffers() {
    if (this.getMerchantTradingOffers() == null) {
      this.updateMerchantTradingOffers();
    }
    return this.getMerchantTradingOffers();
  }

  @Override
  protected void updateTrades() {
    this.updateMerchantTradingOffers();
  }

  @Override
  public void travel(Vec3 vec3) {

    this.handleNavigationTravelEvent(vec3);

    if (this.hasTravelTargetObjectives()) {
      super.travel(vec3);
    } else {
      this.calculateEntityAnimation(this instanceof FlyingAnimal);
    }
  }
}
