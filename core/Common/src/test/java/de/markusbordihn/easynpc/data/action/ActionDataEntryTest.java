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

import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.Test;

class ActionDataEntryTest {

  private CompoundTag createLegacyActionTag(String command) {
    CompoundTag tag = new CompoundTag();
    tag.putString(ActionDataEntry.DATA_TYPE_TAG, ActionDataType.COMMAND.name());
    if (command != null) {
      tag.putString(ActionDataEntry.DATA_COMMAND_TAG, command);
    }
    return tag;
  }

  @Test
  void testConsistentHashCode() {
    ActionDataEntry entry =
        new ActionDataEntry(ActionDataType.COMMAND, "test command", 2, false, false);

    assertEquals(entry.hashCode(), entry.hashCode());
  }

  @Test
  void testUniqueIdPerEntry() {
    ActionDataEntry entry1 =
        new ActionDataEntry(ActionDataType.COMMAND, "test command", 2, false, false);
    ActionDataEntry entry2 =
        new ActionDataEntry(ActionDataType.COMMAND, "test command", 2, false, false);

    assertNotNull(entry1.id());
    assertNotNull(entry2.id());
    assertNotEquals(entry1.id(), entry2.id());
  }

  @Test
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
  void testUUIDConsistencyAfterEncodeDecode() {
    ActionDataEntry original =
        new ActionDataEntry(ActionDataType.COMMAND, "test command", 2, false, false);

    CompoundTag tag = original.createTag();
    ActionDataEntry decoded = new ActionDataEntry(tag);

    assertEquals(original.id(), decoded.id());
  }

  @Test
  void testComplexDataEncodeDecode() {
    BlockPos testPos = new BlockPos(100, 64, 200);
    ActionDataEntry original =
        new ActionDataEntry(ActionDataType.COMMAND, "test", 2, false, false).withBlockPos(testPos);

    CompoundTag tag = original.createTag();
    ActionDataEntry decoded = new ActionDataEntry(tag);

    assertEquals(original.blockPos(), decoded.blockPos());
    assertEquals(original.id(), decoded.id());
  }

  @Test
  void testDifferentUUIDsForDifferentData() {
    ActionDataEntry entry1 =
        new ActionDataEntry(ActionDataType.COMMAND, "command1", 2, false, false);
    ActionDataEntry entry2 =
        new ActionDataEntry(ActionDataType.COMMAND, "command2", 2, false, false);

    assertNotEquals(entry1.id(), entry2.id());
  }

  @Test
  void testEmptyAndDefaultValues() {
    ActionDataEntry defaultEntry = new ActionDataEntry();
    ActionDataEntry emptyCommand = new ActionDataEntry(ActionDataType.COMMAND, "", 2, false, false);

    CompoundTag tag1 = defaultEntry.createTag();
    ActionDataEntry decoded1 = new ActionDataEntry(tag1);
    assertEquals(defaultEntry.id(), decoded1.id());

    CompoundTag tag2 = emptyCommand.createTag();
    ActionDataEntry decoded2 = new ActionDataEntry(tag2);
    assertEquals(emptyCommand.id(), decoded2.id());
  }

  @Test
  void testPermissionLevelPreservation() {
    for (int permLevel = ActionDataEntry.MIN_PERMISSION_LEVEL;
        permLevel <= ActionDataEntry.MAX_PERMISSION_LEVEL;
        permLevel++) {
      ActionDataEntry original =
          new ActionDataEntry(ActionDataType.COMMAND, "test", permLevel, false, false);
      CompoundTag tag = original.createTag();
      ActionDataEntry decoded = new ActionDataEntry(tag);

      assertEquals(original.permissionLevel(), decoded.permissionLevel());
      assertEquals(original.id(), decoded.id());
    }
  }

  @Test
  void testAdminPermissionLevelPreservation() {
    ActionDataEntry original =
        new ActionDataEntry(
            ActionDataType.COMMAND,
            "say admin",
            CommandPermissionLevel.ADMINS.minecraftLevel(),
            false,
            false);

    ActionDataEntry decoded = new ActionDataEntry(original.createTag());

    assertEquals(CommandPermissionLevel.ADMINS.minecraftLevel(), decoded.permissionLevel());
    assertEquals(CommandPermissionLevel.ADMINS, decoded.commandPermissionLevel());
  }

  @Test
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

  @Test
  void testLegacyTagWithoutIdGeneratesRandomUuid() {
    ActionDataEntry legacyEntry = new ActionDataEntry(createLegacyActionTag("legacy"));

    assertNotNull(legacyEntry.id());
  }

  @Test
  void testIdenticalLegacyTagsWithoutIdGenerateDifferentUuids() {
    CompoundTag legacyTag = createLegacyActionTag("legacy");

    ActionDataEntry entry1 = new ActionDataEntry(legacyTag);
    ActionDataEntry entry2 = new ActionDataEntry(legacyTag);

    assertNotEquals(entry1.id(), entry2.id());
  }

  @Test
  void testLegacyTagGeneratedUuidStaysStableAfterSaveRoundTrip() {
    ActionDataEntry original = new ActionDataEntry(createLegacyActionTag("legacy"));

    ActionDataEntry decoded = new ActionDataEntry(original.createTag());

    assertEquals(original.id(), decoded.id());
  }

  @Test
  void testStoredUuidIsPreservedWhenPresent() {
    UUID expectedId = UUID.randomUUID();
    CompoundTag tag = createLegacyActionTag("legacy");
    CompoundTagUtils.writeUUID(tag, ActionDataEntry.DATA_ID_TAG, expectedId);

    ActionDataEntry decoded = new ActionDataEntry(tag);

    assertEquals(expectedId, decoded.id());
  }
}
