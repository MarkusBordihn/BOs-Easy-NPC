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

package de.markusbordihn.easynpc.data.faction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.ChatFormatting;
import org.junit.jupiter.api.Test;

class FactionDataEntryTest {

  @Test
  void testRoundTrip() {
    FactionDataEntry entry = new FactionDataEntry("guards");
    entry.setColor(ChatFormatting.BLUE);
    entry.addHostileFaction("bandits");
    entry.addHostileFaction("undead");

    FactionDataEntry restored = new FactionDataEntry(entry.createTag());
    assertEquals("guards", restored.getName());
    assertEquals(ChatFormatting.BLUE, restored.getColor());
    assertTrue(restored.isHostileTo("bandits"));
    assertTrue(restored.isHostileTo("undead"));
    assertFalse(restored.isHostileTo("guards"));
  }

  @Test
  void testRoundTripWithoutOptionalData() {
    FactionDataEntry entry = new FactionDataEntry("traders");

    FactionDataEntry restored = new FactionDataEntry(entry.createTag());
    assertEquals("traders", restored.getName());
    assertNull(restored.getColor());
    assertTrue(restored.getHostileFactions().isEmpty());
  }

  @Test
  void testDirectedHostility() {
    FactionDataEntry guards = new FactionDataEntry("guards");
    FactionDataEntry bandits = new FactionDataEntry("bandits");
    guards.addHostileFaction("bandits");

    assertTrue(guards.isHostileTo("bandits"));
    assertFalse(bandits.isHostileTo("guards"));
  }

  @Test
  void testAddAndRemoveHostileFaction() {
    FactionDataEntry entry = new FactionDataEntry("guards");
    assertTrue(entry.addHostileFaction("bandits"));
    assertFalse(entry.addHostileFaction("bandits"));
    assertFalse(entry.addHostileFaction(""));
    assertFalse(entry.addHostileFaction(null));

    assertTrue(entry.removeHostileFaction("bandits"));
    assertFalse(entry.removeHostileFaction("bandits"));
    assertFalse(entry.isHostileTo("bandits"));
  }
}
