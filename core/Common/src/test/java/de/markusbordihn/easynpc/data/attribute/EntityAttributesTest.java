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

class EntityAttributesTest {

  @Test
  @DisplayName("An unchanged NPC stores no entity attributes at all")
  void testDefaultsAreNotStored() {
    CompoundTag compoundTag = new EntityAttributes().createTag();

    assertFalse(compoundTag.contains(EntityAttributes.ENTITY_ATTRIBUTE_TAG));
  }

  @Test
  @DisplayName("An NPC without stored attributes stays invulnerable")
  void testMissingAttributesKeepTheirDefaults() {
    EntityAttributes entityAttributes = new EntityAttributes(new CompoundTag());

    assertTrue(entityAttributes.getCombatAttributes().isInvulnerable());
    assertEquals(NavigationType.DEFAULT, entityAttributes.getMovementAttributes().navigationType());
    assertFalse(entityAttributes.getEnvironmentalAttributes().noGravity());
  }

  @Test
  @DisplayName("An NPC with an empty attribute tag stays invulnerable")
  void testEmptyAttributeTagKeepsTheDefaults() {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.put(EntityAttributes.ENTITY_ATTRIBUTE_TAG, new CompoundTag());

    EntityAttributes entityAttributes = new EntityAttributes(compoundTag);

    assertTrue(entityAttributes.getCombatAttributes().isInvulnerable());
  }

  @Test
  @DisplayName("Changed attributes of every group survive a round trip together")
  void testChangedAttributesSurviveRoundTrip() {
    EntityAttributes entityAttributes = new EntityAttributes();
    entityAttributes.setCombatAttributes(
        entityAttributes.getCombatAttributes().withIsInvulnerable(false));
    entityAttributes.setEnvironmentalAttributes(
        entityAttributes.getEnvironmentalAttributes().withNoGravity(true));
    entityAttributes.setInteractionAttributes(
        entityAttributes.getInteractionAttributes().withCanBeLeashed(true));
    entityAttributes.setMovementAttributes(
        entityAttributes.getMovementAttributes().withCanPassDoor(true));

    EntityAttributes restored = new EntityAttributes(entityAttributes.createTag());

    assertFalse(restored.getCombatAttributes().isInvulnerable());
    assertTrue(restored.getEnvironmentalAttributes().noGravity());
    assertTrue(restored.getInteractionAttributes().canBeLeashed());
    assertTrue(restored.getMovementAttributes().canPassDoor());
  }
}
