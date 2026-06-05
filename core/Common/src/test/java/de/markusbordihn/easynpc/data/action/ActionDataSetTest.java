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

import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.Test;

class ActionDataSetTest {

  private CompoundTag createLegacyActionTag(String command) {
    CompoundTag tag = new CompoundTag();
    tag.putString(ActionDataEntry.DATA_TYPE_TAG, ActionDataType.COMMAND.name());
    tag.putString(ActionDataEntry.DATA_COMMAND_TAG, command);
    return tag;
  }

  @Test
  void testLegacyEntryRemainsAddressableAfterSaveReload() {
    ActionDataSet actionDataSet = new ActionDataSet();
    ActionDataEntry legacyEntry = new ActionDataEntry(createLegacyActionTag("legacy"));
    UUID entryId = legacyEntry.id();
    actionDataSet.add(legacyEntry);

    ActionDataSet reloaded = new ActionDataSet(actionDataSet.createTag());

    assertTrue(reloaded.contains(entryId));
    assertEquals("legacy", reloaded.getEntry(entryId).command());

    ActionDataEntry updatedEntry = reloaded.getEntry(entryId).withCommand("updated");
    reloaded.put(entryId, updatedEntry);
    assertEquals("updated", reloaded.getEntry(entryId).command());

    reloaded.remove(entryId);
    assertFalse(reloaded.contains(entryId));
  }

  @Test
  void testIdenticalLegacyEntriesCanCoexistAfterSaveReload() {
    ActionDataSet actionDataSet = new ActionDataSet();
    ActionDataEntry legacyEntryOne = new ActionDataEntry(createLegacyActionTag("legacy"));
    ActionDataEntry legacyEntryTwo = new ActionDataEntry(createLegacyActionTag("legacy"));
    actionDataSet.add(legacyEntryOne);
    actionDataSet.add(legacyEntryTwo);

    ActionDataSet reloaded = new ActionDataSet(actionDataSet.createTag());
    Set<UUID> ids =
        reloaded.getEntries().stream().map(ActionDataEntry::id).collect(Collectors.toSet());

    assertEquals(2, reloaded.size());
    assertEquals(2, ids.size());
    assertTrue(ids.contains(legacyEntryOne.id()));
    assertTrue(ids.contains(legacyEntryTwo.id()));
  }
}
