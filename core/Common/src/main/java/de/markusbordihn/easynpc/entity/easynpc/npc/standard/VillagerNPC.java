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

import com.google.common.collect.ImmutableList;
import de.markusbordihn.easynpc.data.configuration.ConfigurationData;
import de.markusbordihn.easynpc.data.skin.variant.VillagerSkinVariant;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.VillagerRaw;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.TextUtils;
import net.minecraft.network.chat.Component;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.Brain;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.sensing.Sensor;
import net.minecraft.world.entity.ai.sensing.SensorType;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.entity.npc.Villager;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

public class VillagerNPC extends VillagerRaw implements StandardEasyNPC<VillagerRaw> {

  public static final String ID = "villager";

  protected static final ImmutableList<MemoryModuleType<?>> MEMORY_TYPES =
      ImmutableList.of(
          MemoryModuleType.ANGRY_AT,
          MemoryModuleType.ATTACK_TARGET,
          MemoryModuleType.CANT_REACH_WALK_TARGET_SINCE,
          MemoryModuleType.HEARD_BELL_TIME,
          MemoryModuleType.HOME,
          MemoryModuleType.HURT_BY_ENTITY,
          MemoryModuleType.HURT_BY,
          MemoryModuleType.JOB_SITE,
          MemoryModuleType.LOOK_TARGET,
          MemoryModuleType.MEETING_POINT,
          MemoryModuleType.NEAREST_PLAYERS,
          MemoryModuleType.NEAREST_VISIBLE_LIVING_ENTITIES,
          MemoryModuleType.NEAREST_VISIBLE_PLAYER,
          MemoryModuleType.PATH,
          MemoryModuleType.POTENTIAL_JOB_SITE,
          MemoryModuleType.WALK_TARGET);

  private static final ImmutableList<SensorType<? extends Sensor<? super Villager>>> SENSOR_TYPES =
      ImmutableList.of(
          SensorType.GOLEM_DETECTED,
          SensorType.HURT_BY,
          SensorType.NEAREST_BED,
          SensorType.NEAREST_ITEMS,
          SensorType.NEAREST_LIVING_ENTITIES,
          SensorType.NEAREST_PLAYERS,
          SensorType.SECONDARY_POIS,
          SensorType.VILLAGER_BABIES,
          SensorType.VILLAGER_HOSTILES);

  public VillagerNPC(EntityType<? extends Villager> entityType, Level level) {
    this(entityType, level, VillagerSkinVariant.DEFAULT);
  }

  public VillagerNPC(EntityType<? extends Villager> entityType, Level level, Enum<?> variantType) {
    super(entityType, level, variantType);
    this.setInvulnerable(true);
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
        .add(Attributes.MOVEMENT_SPEED, 0.6F)
        .add(Attributes.SPAWN_REINFORCEMENTS_CHANCE, 0.0D);
  }

  @Override
  public boolean canUseOffHand() {
    return false;
  }

  @Override
  public Component getName() {
    Component component = this.getCustomName();
    if (component != null) {
      return TextUtils.removeAction(component);
    }
    Component professionName = getProfessionName();
    Component variantName = getSkinVariantTypeName();
    return TextComponent.getText(variantName.getString() + " (" + professionName.getString() + ")");
  }

  @Override
  public boolean wantsToSpawnGolem(long gameTime) {
    return false;
  }

  @Override
  public boolean hasProfessions() {
    return true;
  }

  @Override
  public ConfigurationData getConfigurationData() {
    return ConfigurationData.STANDARD;
  }

  @Override
  public SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    soundDataSet.addDefaultSound(SoundType.AMBIENT, SoundEvents.VILLAGER_AMBIENT);
    soundDataSet.addDefaultSound(SoundType.DEATH, SoundEvents.VILLAGER_DEATH);
    soundDataSet.addDefaultSound(SoundType.HURT, SoundEvents.VILLAGER_HURT);
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
  protected Brain.Provider<Villager> brainProvider() {
    return Brain.provider(MEMORY_TYPES, SENSOR_TYPES);
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
