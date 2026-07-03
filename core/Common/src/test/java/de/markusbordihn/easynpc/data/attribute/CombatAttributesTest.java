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
import org.junit.jupiter.api.Test;

class CombatAttributesTest {

  @Test
  void testDefaults() {
    CombatAttributes combatAttributes = new CombatAttributes();
    assertFalse(combatAttributes.isAttackableByPlayers());
    assertFalse(combatAttributes.isAttackableByMonsters());
    assertFalse(combatAttributes.isAttackableByFactions());
    assertTrue(combatAttributes.isInvulnerable());
    assertFalse(combatAttributes.isKnockbackResistant());
    assertFalse(combatAttributes.isExplosionResistant());
    assertEquals(0.0, combatAttributes.healthRegeneration());
  }

  @Test
  void testRoundTrip() {
    CombatAttributes combatAttributes =
        new CombatAttributes()
            .withIsAttackableByPlayers(true)
            .withIsAttackableByFactions(true)
            .withHealthRegeneration(2.0);

    CombatAttributes restored = CombatAttributes.decode(combatAttributes.encode(new CompoundTag()));
    assertTrue(restored.isAttackableByPlayers());
    assertFalse(restored.isAttackableByMonsters());
    assertTrue(restored.isAttackableByFactions());
    assertTrue(restored.isInvulnerable());
    assertEquals(2.0, restored.healthRegeneration());
  }

  @Test
  void testDecodeWithoutAttackableByFactionsTag() {
    CompoundTag legacyTag = new CombatAttributes().encode(new CompoundTag());
    legacyTag.remove(CombatAttributes.IS_ATTACKABLE_BY_FACTIONS_TAG);

    CombatAttributes restored = CombatAttributes.decode(legacyTag);
    assertFalse(restored.isAttackableByFactions());
    assertTrue(restored.isInvulnerable());
  }

  @Test
  void testWithersKeepOtherValues() {
    CombatAttributes combatAttributes =
        new CombatAttributes()
            .withIsAttackableByFactions(true)
            .withIsInvulnerable(false)
            .withIsKnockbackResistant(true);
    assertTrue(combatAttributes.isAttackableByFactions());
    assertFalse(combatAttributes.isInvulnerable());
    assertTrue(combatAttributes.isKnockbackResistant());
    assertFalse(combatAttributes.isAttackableByPlayers());
  }
}
