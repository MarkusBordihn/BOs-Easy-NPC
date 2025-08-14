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
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.animal.IronGolem;
import net.minecraft.world.entity.monster.AbstractSkeleton;
import net.minecraft.world.entity.monster.Drowned;
import net.minecraft.world.entity.monster.Evoker;
import net.minecraft.world.entity.monster.Husk;
import net.minecraft.world.entity.monster.Illusioner;
import net.minecraft.world.entity.monster.Pillager;
import net.minecraft.world.entity.monster.Stray;
import net.minecraft.world.entity.monster.Vindicator;
import net.minecraft.world.entity.monster.Witch;
import net.minecraft.world.entity.monster.WitherSkeleton;
import net.minecraft.world.entity.monster.Zombie;
import net.minecraft.world.entity.monster.ZombieVillager;
import net.minecraft.world.entity.npc.Villager;
import net.minecraft.world.entity.player.Player;

public class VanillaEntityAttributeHelper {

  private VanillaEntityAttributeHelper() {}

  public static AttributeSupplier.Builder getVanillaAttributesForEntityType(
      EntityType<?> entityType) {
    if (entityType == EntityType.VILLAGER) {
      return Villager.createAttributes();
    } else if (entityType == EntityType.ZOMBIE) {
      return Zombie.createAttributes();
    } else if (entityType == EntityType.SKELETON) {
      return AbstractSkeleton.createAttributes();
    } else if (entityType == EntityType.PLAYER) {
      return Player.createAttributes();
    } else if (entityType == EntityType.IRON_GOLEM) {
      return IronGolem.createAttributes();
    } else if (entityType == EntityType.WITHER_SKELETON) {
      return WitherSkeleton.createAttributes();
    } else if (entityType == EntityType.STRAY) {
      return Stray.createAttributes();
    } else if (entityType == EntityType.HUSK) {
      return Husk.createAttributes();
    } else if (entityType == EntityType.DROWNED) {
      return Drowned.createAttributes();
    } else if (entityType == EntityType.ZOMBIE_VILLAGER) {
      return ZombieVillager.createAttributes();
    } else if (entityType == EntityType.PILLAGER) {
      return Pillager.createAttributes();
    } else if (entityType == EntityType.VINDICATOR) {
      return Vindicator.createAttributes();
    } else if (entityType == EntityType.EVOKER) {
      return Evoker.createAttributes();
    } else if (entityType == EntityType.ILLUSIONER) {
      return Illusioner.createAttributes();
    } else if (entityType == EntityType.WITCH) {
      return Witch.createAttributes();
    }

    return null;
  }
}
