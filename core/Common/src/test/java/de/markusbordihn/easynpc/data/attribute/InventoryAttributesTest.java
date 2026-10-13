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

package de.markusbordihn.easynpc.data.attribute;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class InventoryAttributesTest {

  @Test
  @DisplayName("Unchanged inventory attributes are not stored")
  void testDefaultsAreNotStored() {
    CompoundTag compoundTag = InventoryAttributes.EMPTY.encode(new CompoundTag());

    assertTrue(compoundTag.isEmpty());
  }

  @Test
  @DisplayName("A missing tag reads back as disabled")
  void testMissingTagsReadAsDisabled() {
    assertEquals(InventoryAttributes.EMPTY, InventoryAttributes.decode(new CompoundTag()));
  }

  @Test
  @DisplayName("Changed inventory attributes survive a round trip")
  void testChangedAttributesSurviveRoundTrip() {
    InventoryAttributes inventoryAttributes =
        InventoryAttributes.EMPTY.withDropOnDeath(true).withPickUpItems(true);

    InventoryAttributes restored =
        InventoryAttributes.decode(inventoryAttributes.encode(new CompoundTag()));

    assertEquals(inventoryAttributes, restored);
  }

  @Test
  @DisplayName("An attribute back on its default is stored no more")
  void testAttributeBackOnDefaultIsNotStored() {
    CompoundTag compoundTag =
        InventoryAttributes.EMPTY
            .withPickUpItems(true)
            .withPickUpItems(false)
            .encode(new CompoundTag());

    assertFalse(compoundTag.contains(InventoryAttributeType.PICK_UP_ITEMS.getTagName()));
  }
}
