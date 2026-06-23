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

package de.markusbordihn.easynpc.entity;

import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EntityTypes;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.animal.golem.IronGolem;
import net.minecraft.world.entity.monster.Witch;
import net.minecraft.world.entity.monster.illager.Evoker;
import net.minecraft.world.entity.monster.illager.Illusioner;
import net.minecraft.world.entity.monster.illager.Pillager;
import net.minecraft.world.entity.monster.illager.Vindicator;
import net.minecraft.world.entity.monster.skeleton.AbstractSkeleton;
import net.minecraft.world.entity.monster.skeleton.Stray;
import net.minecraft.world.entity.monster.skeleton.WitherSkeleton;
import net.minecraft.world.entity.monster.zombie.Drowned;
import net.minecraft.world.entity.monster.zombie.Husk;
import net.minecraft.world.entity.monster.zombie.Zombie;
import net.minecraft.world.entity.monster.zombie.ZombieVillager;
import net.minecraft.world.entity.npc.villager.Villager;
import net.minecraft.world.entity.player.Player;

public class VanillaEntityAttributeHelper {

  private VanillaEntityAttributeHelper() {}

  public static AttributeSupplier.Builder getVanillaAttributesForEntityType(
      EntityType<?> entityType) {
    if (entityType == EntityTypes.VILLAGER) {
      return Villager.createAttributes();
    } else if (entityType == EntityTypes.ZOMBIE) {
      return Zombie.createAttributes();
    } else if (entityType == EntityTypes.SKELETON) {
      return AbstractSkeleton.createAttributes();
    } else if (entityType == EntityTypes.PLAYER) {
      return Player.createAttributes();
    } else if (entityType == EntityTypes.IRON_GOLEM) {
      return IronGolem.createAttributes();
    } else if (entityType == EntityTypes.WITHER_SKELETON) {
      return WitherSkeleton.createAttributes();
    } else if (entityType == EntityTypes.STRAY) {
      return Stray.createAttributes();
    } else if (entityType == EntityTypes.HUSK) {
      return Husk.createAttributes();
    } else if (entityType == EntityTypes.DROWNED) {
      return Drowned.createAttributes();
    } else if (entityType == EntityTypes.ZOMBIE_VILLAGER) {
      return ZombieVillager.createAttributes();
    } else if (entityType == EntityTypes.PILLAGER) {
      return Pillager.createAttributes();
    } else if (entityType == EntityTypes.VINDICATOR) {
      return Vindicator.createAttributes();
    } else if (entityType == EntityTypes.EVOKER) {
      return Evoker.createAttributes();
    } else if (entityType == EntityTypes.ILLUSIONER) {
      return Illusioner.createAttributes();
    } else if (entityType == EntityTypes.WITCH) {
      return Witch.createAttributes();
    }

    return null;
  }
}
