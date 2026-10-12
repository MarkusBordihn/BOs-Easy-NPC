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

import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.attribute.InventoryAttributes;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.core.NonNullList;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.world.ContainerHelper;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.ItemStack;

public interface InventoryDataCapable<E extends Mob> extends EasyNPC<E> {

  // Vanilla inventory carriers (villagers, piglins, allays) already use "Inventory".
  String DATA_INVENTORY_TAG = "EasyNPCInventory";
  int INVENTORY_SIZE = 27;

  SimpleContainer getEasyNPCInventory();

  default InventoryAttributes getInventoryAttributes() {
    AttributeDataCapable<E> attributeData = this.getEasyNPCAttributeData();
    EntityAttributes entityAttributes =
        attributeData != null ? attributeData.getEntityAttributes() : null;
    return entityAttributes != null
        ? entityAttributes.getInventoryAttributes()
        : InventoryAttributes.EMPTY;
  }

  default boolean canPickUpItemsIntoInventory() {
    return this.getInventoryAttributes().pickUpItems();
  }

  default void dropInventoryOnDeath() {
    if (!this.getInventoryAttributes().dropOnDeath()) {
      return;
    }

    for (ItemStack itemStack : this.getEasyNPCInventory().removeAllItems()) {
      this.getEntity().spawnAtLocation(itemStack);
    }
  }

  default void addAdditionalInventoryData(CompoundTag compoundTag) {
    SimpleContainer inventory = this.getEasyNPCInventory();
    if (inventory == null || inventory.isEmpty()) {
      return;
    }

    NonNullList<ItemStack> items =
        NonNullList.withSize(inventory.getContainerSize(), ItemStack.EMPTY);
    for (int i = 0; i < inventory.getContainerSize(); i++) {
      items.set(i, inventory.getItem(i));
    }
    compoundTag.put(DATA_INVENTORY_TAG, ContainerHelper.saveAllItems(new CompoundTag(), items));
  }

  default void readAdditionalInventoryData(CompoundTag compoundTag) {
    SimpleContainer inventory = this.getEasyNPCInventory();
    if (inventory == null || !compoundTag.contains(DATA_INVENTORY_TAG, Tag.TAG_COMPOUND)) {
      return;
    }

    NonNullList<ItemStack> items =
        NonNullList.withSize(inventory.getContainerSize(), ItemStack.EMPTY);
    ContainerHelper.loadAllItems(compoundTag.getCompound(DATA_INVENTORY_TAG), items);
    for (int i = 0; i < inventory.getContainerSize(); i++) {
      inventory.setItem(i, items.get(i));
    }
  }
}
