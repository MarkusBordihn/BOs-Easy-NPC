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

package de.markusbordihn.easynpc.entity.easynpc.npc.standard;

import de.markusbordihn.easynpc.data.configuration.ConfigurationData;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.variant.HorseSkinVariant;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.HorseRaw;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.entity.animal.horse.Horse;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

public class HorseNPC extends HorseRaw implements StandardEasyNPC<HorseRaw> {

  public static final String ID = "horse";
  public static final String ID_SKELETON = "horse_skeleton";
  public static final String ID_ZOMBIE = "horse_zombie";

  public HorseNPC(EntityType<? extends Horse> entityType, Level level) {
    this(entityType, level, HorseSkinVariant.WHITE);
  }

  public HorseNPC(EntityType<? extends Horse> entityType, Level level, Enum<?> variantType) {
    super(entityType, level, variantType);
  }

  public static AttributeSupplier.Builder createAttributes() {
    return Mob.createMobAttributes()
        .add(Attributes.ARMOR_TOUGHNESS, 0.0D)
        .add(Attributes.ARMOR, 0.0D)
        .add(Attributes.ATTACK_DAMAGE, 2.0D)
        .add(Attributes.ATTACK_KNOCKBACK, 0.0D)
        .add(Attributes.ATTACK_SPEED, 0.0D)
        .add(Attributes.FOLLOW_RANGE, 32.0D)
        .add(Attributes.KNOCKBACK_RESISTANCE, 0.0D)
        .add(Attributes.MAX_HEALTH, 8.0D)
        .add(Attributes.MOVEMENT_SPEED, 0.3F)
        .add(Attributes.SPAWN_REINFORCEMENTS_CHANCE, 0.0D);
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
  public ConfigurationData getConfigurationData() {
    return ConfigurationData.STANDARD;
  }

  @Override
  public SkinModel getSkinModel() {
    return SkinModel.WOLF;
  }

  @Override
  public int getEntityDialogTop() {
    return -37;
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    soundDataSet.addDefaultSound(SoundType.AMBIENT, SoundEvents.HORSE_AMBIENT);
    soundDataSet.addDefaultSound(SoundType.DEATH, SoundEvents.HORSE_DEATH);
    soundDataSet.addDefaultSound(SoundType.EAT, SoundEvents.HORSE_EAT);
    soundDataSet.addDefaultSound(SoundType.HURT, SoundEvents.HORSE_HURT);
    soundDataSet.addDefaultSound(SoundType.STEP, SoundEvents.HORSE_STEP);
    soundDataSet.addDefaultSound(SoundType.TRADE, SoundEvents.VILLAGER_TRADE);
    soundDataSet.addDefaultSound(SoundType.TRADE_YES, SoundEvents.VILLAGER_YES);
    soundDataSet.addDefaultSound(SoundType.TRADE_NO, SoundEvents.VILLAGER_NO);
    return soundDataSet;
  }

  @Override
  protected void registerGoals() {
    // No default goals for NPCs.
  }

  @Override
  public void travel(Vec3 vec3) {

    this.handleNavigationTravelEvent(vec3);

    // Handle movement for NPC for specific conditions.
    if (this.hasTravelTargetObjectives()) {
      // Allow travel for NPC, if travel objectives are used.
      super.travel(vec3);
    } else {
      // Make sure we only calculate animations for be as much as possible server-friendly.
      this.calculateEntityAnimation(this instanceof FlyingAnimal);
    }
  }
}
