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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface InventoryDataCapable<E extends Mob> extends EasyNPC<E> {

  String LEGACY_ARMOR_ITEMS_TAG = "ArmorItems";
  String LEGACY_HAND_ITEMS_TAG = "HandItems";

  default void addAdditionalInventoryData(ValueOutput valueOutput) {
    // No additional data to save - equipment is handled by vanilla
  }

  default void readAdditionalInventoryData(ValueInput valueInput) {
    convertLegacyArmorItems(valueInput);
    convertLegacyHandItems(valueInput);
  }

  default void convertLegacyArmorItems(ValueInput valueInput) {
    valueInput
        .list(LEGACY_ARMOR_ITEMS_TAG, CompoundTag.CODEC)
        .ifPresent(
            list -> {
              var entity = getLivingEntity();
              var itemsArray = list.stream().toArray(CompoundTag[]::new);
              if (itemsArray.length >= 4) {
                if (!itemsArray[0].isEmpty()) {
                  entity.setItemSlot(
                      EquipmentSlot.FEET,
                      ItemStack.OPTIONAL_CODEC
                          .parse(NbtOps.INSTANCE, itemsArray[0])
                          .resultOrPartial(error -> {})
                          .orElse(ItemStack.EMPTY));
                }
                if (!itemsArray[1].isEmpty()) {
                  entity.setItemSlot(
                      EquipmentSlot.LEGS,
                      ItemStack.OPTIONAL_CODEC
                          .parse(NbtOps.INSTANCE, itemsArray[1])
                          .resultOrPartial(error -> {})
                          .orElse(ItemStack.EMPTY));
                }
                if (!itemsArray[2].isEmpty()) {
                  entity.setItemSlot(
                      EquipmentSlot.CHEST,
                      ItemStack.OPTIONAL_CODEC
                          .parse(NbtOps.INSTANCE, itemsArray[2])
                          .resultOrPartial(error -> {})
                          .orElse(ItemStack.EMPTY));
                }
                if (!itemsArray[3].isEmpty()) {
                  entity.setItemSlot(
                      EquipmentSlot.HEAD,
                      ItemStack.OPTIONAL_CODEC
                          .parse(NbtOps.INSTANCE, itemsArray[3])
                          .resultOrPartial(error -> {})
                          .orElse(ItemStack.EMPTY));
                }
              }
            });
  }

  default void convertLegacyHandItems(ValueInput valueInput) {
    valueInput
        .list(LEGACY_HAND_ITEMS_TAG, CompoundTag.CODEC)
        .ifPresent(
            list -> {
              var entity = getLivingEntity();
              var itemsArray = list.stream().toArray(CompoundTag[]::new);
              if (itemsArray.length >= 2) {
                if (!itemsArray[0].isEmpty()) {
                  entity.setItemSlot(
                      EquipmentSlot.MAINHAND,
                      ItemStack.OPTIONAL_CODEC
                          .parse(NbtOps.INSTANCE, itemsArray[0])
                          .resultOrPartial(error -> {})
                          .orElse(ItemStack.EMPTY));
                }
                if (!itemsArray[1].isEmpty()) {
                  entity.setItemSlot(
                      EquipmentSlot.OFFHAND,
                      ItemStack.OPTIONAL_CODEC
                          .parse(NbtOps.INSTANCE, itemsArray[1])
                          .resultOrPartial(error -> {})
                          .orElse(ItemStack.EMPTY));
                }
              }
            });
  }
}
