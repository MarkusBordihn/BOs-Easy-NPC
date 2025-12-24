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

package de.markusbordihn.easynpc.data.action;

import static org.junit.jupiter.api.Assertions.*;

import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("ActionDataEntry Tests")
class ActionDataEntryTest {

  @Test
  @DisplayName("Should generate consistent hashCode for identical data")
  void testConsistentHashCode() {
    ActionDataEntry entry1 =
        new ActionDataEntry(ActionDataType.COMMAND, "test command", 2, false, false);
    ActionDataEntry entry2 =
        new ActionDataEntry(ActionDataType.COMMAND, "test command", 2, false, false);

    assertEquals(entry1.hashCode(), entry2.hashCode());
  }

  @Test
  @DisplayName("Should generate consistent UUID from hashCode")
  void testConsistentUUIDGeneration() {
    ActionDataEntry entry1 =
        new ActionDataEntry(ActionDataType.COMMAND, "test command", 2, false, false);
    ActionDataEntry entry2 =
        new ActionDataEntry(ActionDataType.COMMAND, "test command", 2, false, false);

    assertEquals(entry1.getId(), entry2.getId());
  }

  @Test
  @DisplayName("Should preserve data through encode/decode cycle")
  void testEncodeDecodeConsistency() {
    ActionDataEntry original =
        new ActionDataEntry(ActionDataType.COMMAND, "say Hello World", 2, true, false);

    CompoundTag tag = original.createTag();
    ActionDataEntry decoded = new ActionDataEntry(tag);

    assertEquals(original.actionDataType(), decoded.actionDataType());
    assertEquals(original.command(), decoded.command());
    assertEquals(original.permissionLevel(), decoded.permissionLevel());
    assertEquals(original.executeAsUser(), decoded.executeAsUser());
    assertEquals(original.enableDebug(), decoded.enableDebug());
  }

  @Test
  @DisplayName("Should generate same UUID after encode/decode cycle")
  void testUUIDConsistencyAfterEncodeDecode() {
    ActionDataEntry original =
        new ActionDataEntry(ActionDataType.COMMAND, "test command", 2, false, false);

    CompoundTag tag = original.createTag();
    ActionDataEntry decoded = new ActionDataEntry(tag);

    assertEquals(original.getId(), decoded.getId());
  }

  @Test
  @DisplayName("Should handle complex data with BlockPos and UUID")
  void testComplexDataEncodeDecode() {
    BlockPos testPos = new BlockPos(100, 64, 200);
    ActionDataEntry original =
        new ActionDataEntry(ActionDataType.COMMAND, "test", 2, false, false).withBlockPos(testPos);

    CompoundTag tag = original.createTag();
    ActionDataEntry decoded = new ActionDataEntry(tag);

    assertEquals(original.blockPos(), decoded.blockPos());
    assertEquals(original.getId(), decoded.getId());
  }

  @Test
  @DisplayName("Should generate different UUIDs for different data")
  void testDifferentUUIDsForDifferentData() {
    ActionDataEntry entry1 =
        new ActionDataEntry(ActionDataType.COMMAND, "command1", 2, false, false);
    ActionDataEntry entry2 =
        new ActionDataEntry(ActionDataType.COMMAND, "command2", 2, false, false);

    assertNotEquals(entry1.getId(), entry2.getId());
  }

  @Test
  @DisplayName("Should handle empty and default values correctly")
  void testEmptyAndDefaultValues() {
    ActionDataEntry defaultEntry = new ActionDataEntry();
    ActionDataEntry emptyCommand = new ActionDataEntry(ActionDataType.COMMAND, "", 2, false, false);

    CompoundTag tag1 = defaultEntry.createTag();
    ActionDataEntry decoded1 = new ActionDataEntry(tag1);
    assertEquals(defaultEntry.getId(), decoded1.getId());

    CompoundTag tag2 = emptyCommand.createTag();
    ActionDataEntry decoded2 = new ActionDataEntry(tag2);
    assertEquals(emptyCommand.getId(), decoded2.getId());
  }

  @Test
  @DisplayName("Should preserve permission level through encode/decode")
  void testPermissionLevelPreservation() {
    for (int permLevel = ActionDataEntry.MIN_PERMISSION_LEVEL;
        permLevel <= ActionDataEntry.MAX_PERMISSION_LEVEL;
        permLevel++) {
      ActionDataEntry original =
          new ActionDataEntry(ActionDataType.COMMAND, "test", permLevel, false, false);
      CompoundTag tag = original.createTag();
      ActionDataEntry decoded = new ActionDataEntry(tag);

      assertEquals(original.permissionLevel(), decoded.permissionLevel());
      assertEquals(original.getId(), decoded.getId());
    }
  }

  @Test
  @DisplayName("Should handle whitespace trimming consistently")
  void testWhitespaceTrimming() {
    ActionDataEntry withSpaces =
        new ActionDataEntry(ActionDataType.COMMAND, "  test command  ", 2, false, false);
    ActionDataEntry withoutSpaces =
        new ActionDataEntry(ActionDataType.COMMAND, "test command", 2, false, false);

    CompoundTag tag = withSpaces.createTag();
    ActionDataEntry decoded = new ActionDataEntry(tag);

    assertEquals("test command", decoded.command());
    assertNotEquals(withSpaces.hashCode(), withoutSpaces.hashCode());
  }
}
