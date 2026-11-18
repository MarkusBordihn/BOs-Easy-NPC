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

package de.markusbordihn.easynpc.entity.easynpc.handlers;

import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.SpawnGroupData;
import net.minecraft.world.entity.monster.Spider;
import net.minecraft.world.entity.monster.piglin.Piglin;
import net.minecraft.world.item.ItemStack;

public class SpawnRandomnessHandler {

  private SpawnRandomnessHandler() {}

  // Only occupied slots are kept, so spawn egg NPCs still get their vanilla default weapon.
  public static SpawnGroupData finalizeSpawnKeepingEquipment(
      Mob mob, Supplier<SpawnGroupData> finalizeSpawn) {
    Map<EquipmentSlot, ItemStack> equipmentBeforeSpawn = new EnumMap<>(EquipmentSlot.class);
    for (EquipmentSlot equipmentSlot : EquipmentSlot.values()) {
      ItemStack itemStack = mob.getItemBySlot(equipmentSlot);
      if (!itemStack.isEmpty()) {
        equipmentBeforeSpawn.put(equipmentSlot, itemStack.copy());
      }
    }

    SpawnGroupData spawnGroupData = finalizeSpawn.get();
    equipmentBeforeSpawn.forEach((equipmentSlot, itemStack) -> {
      if (!ItemStack.matches(mob.getItemBySlot(equipmentSlot), itemStack)) {
        mob.setItemSlot(equipmentSlot, itemStack);
      }
    });
    return spawnGroupData;
  }

  public static SpawnGroupData finalizeSpawnKeepingAgeAndEquipment(
      Piglin piglin, Supplier<SpawnGroupData> finalizeSpawn) {
    boolean baby = piglin.isBaby();
    Map<EquipmentSlot, ItemStack> equipmentBeforeSpawn = new EnumMap<>(EquipmentSlot.class);
    for (EquipmentSlot equipmentSlot : EquipmentSlot.values()) {
      equipmentBeforeSpawn.put(equipmentSlot, piglin.getItemBySlot(equipmentSlot).copy());
    }

    SpawnGroupData spawnGroupData = finalizeSpawn.get();
    if (piglin.isBaby() != baby) {
      piglin.setBaby(baby);
    }
    equipmentBeforeSpawn.forEach((equipmentSlot, itemStack) -> {
      if (!ItemStack.matches(piglin.getItemBySlot(equipmentSlot), itemStack)) {
        piglin.setItemSlot(equipmentSlot, itemStack);
      }
    });
    return spawnGroupData;
  }

  public static SpawnGroupData finalizeSpawnWithoutJockeyOrEffect(
      Spider spider, SpawnGroupData spawnGroupData, UnaryOperator<SpawnGroupData> finalizeSpawn) {
    List<Entity> passengersBeforeSpawn = List.copyOf(spider.getPassengers());
    SpawnGroupData spawnGroupDataWithoutEffect = spawnGroupData;
    // Spider.finalizeSpawn only rolls its random effect when no group data is passed.
    if (spawnGroupDataWithoutEffect == null) {
      spawnGroupDataWithoutEffect = new Spider.SpiderEffectsGroupData();
    }

    SpawnGroupData result = finalizeSpawn.apply(spawnGroupDataWithoutEffect);
    for (Entity passenger : List.copyOf(spider.getPassengers())) {
      if (!passengersBeforeSpawn.contains(passenger)) {
        passenger.stopRiding();
        passenger.discard();
      }
    }
    return result;
  }
}
