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

package de.markusbordihn.easynpc.data.state;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class StateDataSetTest {

  private static final Identifier PHASE = Identifier.fromNamespaceAndPath("test_mod", "phase");
  private static final Identifier STAGE = Identifier.fromNamespaceAndPath("test_mod", "stage");

  @Test
  void testNumberAndTextRoundTrip() {
    StateDataSet stateDataSet = new StateDataSet();
    stateDataSet.set(PHASE, StateEntry.of(3));
    stateDataSet.set(STAGE, StateEntry.of("intro"));

    StateDataSet reloaded = new StateDataSet(stateDataSet.createTag());

    assertEquals(3, reloaded.get(PHASE).numberValue());
    assertTrue(reloaded.get(PHASE).isNumber());
    assertEquals("intro", reloaded.get(STAGE).textValue());
    assertTrue(reloaded.get(STAGE).isText());
  }

  @Test
  @DisplayName("An empty state set writes no tag at all")
  void testEmptySetIsNotWritten() {
    CompoundTag compoundTag = new StateDataSet().createTag();

    assertFalse(compoundTag.contains(StateDataSet.DATA_STATE_DATA_SET_TAG));
    assertTrue(new StateDataSet(compoundTag).isEmpty());
  }

  @Test
  void testRemoveAndSetNull() {
    StateDataSet stateDataSet = new StateDataSet();
    stateDataSet.set(PHASE, StateEntry.of(1));
    stateDataSet.remove(PHASE);

    assertFalse(stateDataSet.has(PHASE));
    assertNull(stateDataSet.get(PHASE));
  }

  @Test
  @DisplayName("New states beyond the per NPC limit are rejected, existing ones stay writable")
  void testEntryLimit() {
    StateDataSet stateDataSet = new StateDataSet();
    for (int i = 0; i < StateDataSet.MAX_STATE_ENTRIES; i++) {
      stateDataSet.set(Identifier.fromNamespaceAndPath("test_mod", "state_" + i), StateEntry.of(i));
    }

    stateDataSet.set(PHASE, StateEntry.of(1));
    assertFalse(stateDataSet.has(PHASE));

    Identifier existing = Identifier.fromNamespaceAndPath("test_mod", "state_0");
    stateDataSet.set(existing, StateEntry.of(99));
    assertEquals(99, stateDataSet.get(existing).numberValue());
  }

  @Test
  @DisplayName("States beyond the per NPC limit are dropped while loading instead of being kept")
  void testEntryLimitOnLoad() {
    StateDataSet stateDataSet = new StateDataSet();
    CompoundTag stateTag = new CompoundTag();
    for (int i = 0; i < StateDataSet.MAX_STATE_ENTRIES + 10; i++) {
      stateTag.putInt("test_mod:state_" + i, i);
    }
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.put(StateDataSet.DATA_STATE_DATA_SET_TAG, stateTag);

    stateDataSet.load(compoundTag);

    assertEquals(StateDataSet.MAX_STATE_ENTRIES, stateDataSet.keys().size());
  }

  @Test
  @DisplayName("A text value of only spaces is stored as an empty state and stays empty")
  void testWhitespaceTextValue() {
    StateEntry stateEntry = StateEntry.of("   ");

    assertEquals(StateEntry.EMPTY, stateEntry);

    StateDataSet stateDataSet = new StateDataSet();
    stateDataSet.set(STAGE, stateEntry);

    assertEquals(stateEntry, new StateDataSet(stateDataSet.createTag()).get(STAGE));
  }

  @Test
  void testTextValueIsTruncated() {
    String longText = "x".repeat(StateEntry.MAX_TEXT_VALUE_LENGTH + 10);

    assertEquals(StateEntry.MAX_TEXT_VALUE_LENGTH, StateEntry.of(longText).textValue().length());
  }

  @Test
  void testFlagValues() {
    assertTrue(StateEntry.of(true).asFlag());
    assertFalse(StateEntry.of(false).asFlag());
    assertTrue(StateEntry.of("intro").asFlag());
    assertFalse(StateEntry.of("").asFlag());
  }
}
