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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class InteractionAttributesTest {

  @Test
  @DisplayName("Unchanged interaction attributes are not stored")
  void testDefaultsAreNotStored() {
    CompoundTag compoundTag = new InteractionAttributes().encode(new CompoundTag());

    assertTrue(compoundTag.isEmpty());
  }

  @Test
  @DisplayName("A missing tag reads back as disabled")
  void testMissingTagsReadAsDisabled() {
    InteractionAttributes interactionAttributes = InteractionAttributes.decode(new CompoundTag());

    assertFalse(interactionAttributes.isPushable());
    assertFalse(interactionAttributes.canBeHitByProjectile());
    assertFalse(interactionAttributes.canBeLeashed());
    assertFalse(interactionAttributes.pushEntities());
  }

  @Test
  @DisplayName("Changed interaction attributes survive a round trip")
  void testChangedAttributesSurviveRoundTrip() {
    InteractionAttributes interactionAttributes =
        new InteractionAttributes().withIsPushable(true).withCanBeLeashed(true);

    InteractionAttributes restored =
        InteractionAttributes.decode(interactionAttributes.encode(new CompoundTag()));

    assertTrue(restored.isPushable());
    assertTrue(restored.canBeLeashed());
    assertFalse(restored.canBeHitByProjectile());
    assertFalse(restored.pushEntities());
  }

  @Test
  @DisplayName("An attribute back on its default is stored no more")
  void testAttributeBackOnDefaultIsNotStored() {
    CompoundTag compoundTag =
        new InteractionAttributes()
            .withCanBeLeashed(true)
            .withCanBeLeashed(false)
            .encode(new CompoundTag());

    assertFalse(compoundTag.contains(InteractionAttributes.CAN_BE_LEASHED_TAG));
  }
}
