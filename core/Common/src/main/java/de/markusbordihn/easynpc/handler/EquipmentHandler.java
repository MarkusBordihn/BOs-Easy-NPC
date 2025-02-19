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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.data.animation.SmartAnimations;
import de.markusbordihn.easynpc.debug.Logger;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;

public class EquipmentHandler {

  private EquipmentHandler() {}

  public static void setHandSlotItem(
      EasyNPC<?> easyNPC, InteractionHand interactionHand, ItemStack itemStack) {
    setEquipmentSlotItem(
        easyNPC,
        interactionHand == InteractionHand.MAIN_HAND
            ? EquipmentSlot.MAINHAND
            : EquipmentSlot.OFFHAND,
        itemStack);
  }

  public static void setArmorSlotItem(
      EasyNPC<?> easyNPC, EquipmentSlot equipmentSlot, ItemStack itemStack) {
    setEquipmentSlotItem(easyNPC, equipmentSlot, itemStack);
  }

  public static boolean setEquipmentSlotItem(
      EasyNPC<?> easyNPC, EquipmentSlot equipmentSlot, ItemStack itemStack) {
    if (easyNPC == null || equipmentSlot == null || itemStack == null) {
      Logger.INSTANCE.error("[{}] Error setting owner ", easyNPC);
      return false;
    }

    LivingEntity livingEntity = easyNPC.getLivingEntity();
    Logger.INSTANCE.debug("[{}] Setting equipment slot {} to {}", easyNPC, equipmentSlot, itemStack);

    switch (equipmentSlot) {
      case MAINHAND:
        livingEntity.setItemInHand(InteractionHand.MAIN_HAND, itemStack);
        break;
      case OFFHAND:
        livingEntity.setItemInHand(InteractionHand.OFF_HAND, itemStack);
        break;
      case HEAD, CHEST, LEGS, FEET:
        livingEntity.setItemSlot(equipmentSlot, itemStack);
        break;
      default:
        Logger.INSTANCE.error("[{}] Equipment slot {} is not supported!", easyNPC, equipmentSlot);
        return false;
    }

    // Handle smart Animations based on the used item.
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    Item item = itemStack.getItem();
    if (modelData != null
        && modelData.supportsSmartAnimations()
        && modelData.getModelSupportsSmartAnimations()) {
      boolean hasItemInMainHand =
          !livingEntity.getMainHandItem().isEmpty() || !livingEntity.getOffhandItem().isEmpty();
      if (hasItemInMainHand
          && !Items.AIR.equals(item)
          && !SmartAnimations.itemSupportSmartAnimation(item)) {
        Logger.INSTANCE.debug("[{}] Disable smart animations for item {}", easyNPC, item);
        modelData.setItemSupportsSmartAnimations(false);
      } else if (!hasItemInMainHand && !modelData.getItemSupportsSmartAnimations()) {
        Logger.INSTANCE.debug("[{}] Re-Enable smart animations for item {}", easyNPC, item);
        modelData.setItemSupportsSmartAnimations(true);
      }
    }

    return true;
  }
}
