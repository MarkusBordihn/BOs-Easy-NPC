/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.data.condition;

import static org.junit.jupiter.api.Assertions.*;

import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("ConditionDataEntry Tests")
class ConditionDataEntryTest {

  @Test
  @DisplayName("Should create empty condition")
  void testEmptyCondition() {
    ConditionDataEntry entry = ConditionDataEntry.EMPTY;

    assertEquals(ConditionType.NONE, entry.conditionType());
    assertEquals(ConditionOperationType.NONE, entry.operationType());
    assertFalse(entry.isValid());
  }

  @Test
  @DisplayName("Should validate scoreboard condition requires name")
  void testScoreboardConditionRequiresName() {
    ConditionDataEntry entryWithoutName =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "", 5);
    assertFalse(entryWithoutName.isValid());

    ConditionDataEntry entryWithName =
        new ConditionDataEntry(
            ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test_score", 5);
    assertTrue(entryWithName.isValid());
  }

  @Test
  @DisplayName("Should validate scoreboard condition requires operation")
  void testScoreboardConditionRequiresOperation() {
    ConditionDataEntry entryWithoutOperation =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.NONE, "test", 5);
    assertFalse(entryWithoutOperation.isValid());

    ConditionDataEntry entryWithOperation =
        new ConditionDataEntry(
            ConditionType.SCOREBOARD, ConditionOperationType.GREATER_THAN, "test", 5);
    assertTrue(entryWithOperation.isValid());
  }

  @Test
  @DisplayName("Should detect if entry has name")
  void testHasName() {
    ConditionDataEntry withName = new ConditionDataEntry(ConditionType.SCOREBOARD).withName("test");
    assertTrue(withName.hasName());

    ConditionDataEntry withoutName = new ConditionDataEntry(ConditionType.SCOREBOARD).withName("");
    assertFalse(withoutName.hasName());

    ConditionDataEntry nullName = new ConditionDataEntry(ConditionType.SCOREBOARD);
    assertFalse(nullName.hasName());
  }

  @Test
  @DisplayName("Should detect if entry has string value")
  void testHasStringValue() {
    ConditionDataEntry withText =
        new ConditionDataEntry(ConditionType.SCOREBOARD).withStringValue("test");
    assertTrue(withText.hasStringValue());

    ConditionDataEntry withoutText =
        new ConditionDataEntry(ConditionType.SCOREBOARD).withStringValue("");
    assertFalse(withoutText.hasStringValue());
  }

  @Test
  @DisplayName("Should create entry with condition type")
  void testWithConditionType() {
    ConditionDataEntry original =
        new ConditionDataEntry(ConditionType.NONE, ConditionOperationType.EQUALS, "test", 5);
    ConditionDataEntry updated = original.withConditionType(ConditionType.SCOREBOARD);

    assertEquals(ConditionType.SCOREBOARD, updated.conditionType());
    assertEquals(ConditionOperationType.EQUALS, updated.operationType());
    assertEquals("test", updated.name());
    assertEquals(5, updated.value());
  }

  @Test
  @DisplayName("Should create entry with operation type")
  void testWithOperationType() {
    ConditionDataEntry original =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.NONE, "test", 5);
    ConditionDataEntry updated = original.withOperationType(ConditionOperationType.GREATER_THAN);

    assertEquals(ConditionType.SCOREBOARD, updated.conditionType());
    assertEquals(ConditionOperationType.GREATER_THAN, updated.operationType());
    assertEquals("test", updated.name());
    assertEquals(5, updated.value());
  }

  @Test
  @DisplayName("Should create entry with name")
  void testWithName() {
    ConditionDataEntry original = new ConditionDataEntry(ConditionType.SCOREBOARD);
    ConditionDataEntry updated = original.withName("test_score");

    assertEquals("test_score", updated.name());
  }

  @Test
  @DisplayName("Should create entry with value")
  void testWithValue() {
    ConditionDataEntry original = new ConditionDataEntry(ConditionType.SCOREBOARD);
    ConditionDataEntry updated = original.withValue(10);

    assertEquals(10, updated.value());
  }

  @Test
  @DisplayName("Should create entry with string value")
  void testWithStringValue() {
    ConditionDataEntry original = new ConditionDataEntry(ConditionType.SCOREBOARD);
    ConditionDataEntry updated = original.withStringValue("test text");

    assertEquals("test text", updated.text());
  }

  @Test
  @DisplayName("Should serialize to NBT correctly")
  void testWriteToNBT() {
    ConditionDataEntry entry =
        new ConditionDataEntry(
            ConditionType.SCOREBOARD, ConditionOperationType.GREATER_THAN, "test_score", 5);
    CompoundTag tag = entry.createTag();

    assertEquals("SCOREBOARD", tag.getString(ConditionDataEntry.DATA_TYPE_TAG));
    assertEquals("GREATER_THAN", tag.getString(ConditionDataEntry.DATA_OPERATION_TAG));
    assertEquals("test_score", tag.getString(ConditionDataEntry.DATA_NAME_TAG));
    assertEquals(5, tag.getInt(ConditionDataEntry.DATA_VALUE_TAG));
  }

  @Test
  @DisplayName("Should not serialize default values to NBT")
  void testWriteToNBTWithDefaults() {
    ConditionDataEntry entry =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.NONE, "", 0);
    CompoundTag tag = entry.createTag();

    assertEquals("SCOREBOARD", tag.getString(ConditionDataEntry.DATA_TYPE_TAG));
    assertFalse(tag.contains(ConditionDataEntry.DATA_OPERATION_TAG));
    assertFalse(tag.contains(ConditionDataEntry.DATA_NAME_TAG));
    assertFalse(tag.contains(ConditionDataEntry.DATA_VALUE_TAG));
  }

  @Test
  @DisplayName("Should deserialize from NBT correctly")
  void testReadFromNBT() {
    CompoundTag tag = new CompoundTag();
    tag.putString(ConditionDataEntry.DATA_TYPE_TAG, "SCOREBOARD");
    tag.putString(ConditionDataEntry.DATA_OPERATION_TAG, "EQUALS");
    tag.putString(ConditionDataEntry.DATA_NAME_TAG, "test_score");
    tag.putInt(ConditionDataEntry.DATA_VALUE_TAG, 10);

    ConditionDataEntry entry = new ConditionDataEntry(tag);

    assertEquals(ConditionType.SCOREBOARD, entry.conditionType());
    assertEquals(ConditionOperationType.EQUALS, entry.operationType());
    assertEquals("test_score", entry.name());
    assertEquals(10, entry.value());
  }

  @Test
  @DisplayName("Should handle missing optional fields in NBT")
  void testReadFromNBTWithMissingFields() {
    CompoundTag tag = new CompoundTag();
    tag.putString(ConditionDataEntry.DATA_TYPE_TAG, "SCOREBOARD");

    ConditionDataEntry entry = new ConditionDataEntry(tag);

    assertEquals(ConditionType.SCOREBOARD, entry.conditionType());
    assertEquals(ConditionOperationType.NONE, entry.operationType());
    assertEquals("", entry.name());
    assertEquals(0, entry.value());
  }

  @Test
  @DisplayName("Should serialize and deserialize with string value")
  void testSerializeWithStringValue() {
    ConditionDataEntry original =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5)
            .withStringValue("test text");

    CompoundTag tag = original.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);

    assertEquals(original.conditionType(), deserialized.conditionType());
    assertEquals(original.operationType(), deserialized.operationType());
    assertEquals(original.name(), deserialized.name());
    assertEquals(original.value(), deserialized.value());
  }

  @Test
  @DisplayName("Should test equality correctly")
  void testEquals() {
    ConditionDataEntry entry1 =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5);
    ConditionDataEntry entry2 =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5);
    ConditionDataEntry entry3 =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 10);

    assertEquals(entry1, entry2);
    assertNotEquals(entry1, entry3);
    assertNotEquals(null, entry1);
  }

  @Test
  @DisplayName("Should generate consistent hash codes")
  void testHashCode() {
    ConditionDataEntry entry1 =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5);
    ConditionDataEntry entry2 =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5);

    assertEquals(entry1.hashCode(), entry2.hashCode());
  }

  @Test
  @DisplayName("Should generate consistent UUID from hashCode")
  void testConsistentUUIDGeneration() {
    ConditionDataEntry entry1 =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5);
    ConditionDataEntry entry2 =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5);

    assertEquals(entry1.getId(), entry2.getId());
  }

  @Test
  @DisplayName("Should generate same UUID after encode/decode cycle")
  void testUUIDConsistencyAfterEncodeDecode() {
    ConditionDataEntry original =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5);

    CompoundTag tag = original.createTag();
    ConditionDataEntry decoded = new ConditionDataEntry(tag);

    assertEquals(original.getId(), decoded.getId());
  }

  @Test
  @DisplayName("Should handle non-existent scoreboard value (-1) in conditions")
  void testNonExistentScoreboardValue() {
    ConditionDataEntry checkExists =
        new ConditionDataEntry(
            ConditionType.SCOREBOARD,
            ConditionOperationType.GREATER_THAN_OR_EQUALS,
            "test_score",
            0);
    assertTrue(checkExists.isValid());

    ConditionDataEntry checkNotExists =
        new ConditionDataEntry(
            ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test_score", -1);
    assertTrue(checkNotExists.isValid());
  }
}
