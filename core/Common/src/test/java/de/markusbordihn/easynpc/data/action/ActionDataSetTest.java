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

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
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

  private ActionDataSet createOrderedActionDataSet(ActionDataEntry... entries) {
    ActionDataSet actionDataSet = new ActionDataSet();
    for (ActionDataEntry entry : entries) {
      actionDataSet.add(entry);
    }
    return actionDataSet;
  }

  private ActionDataEntry createCommandEntry(String command) {
    return new ActionDataEntry(ActionDataType.COMMAND, null, command);
  }

  @Test
  @DisplayName("Replacing an entry keeps it at its position")
  void testPutKeepsPosition() {
    ActionDataEntry firstEntry = createCommandEntry("/say first");
    ActionDataEntry secondEntry = createCommandEntry("/say second");
    ActionDataEntry thirdEntry = createCommandEntry("/say third");
    ActionDataSet actionDataSet = createOrderedActionDataSet(firstEntry, secondEntry, thirdEntry);

    ActionDataEntry updatedEntry = secondEntry.withCommand("/say updated");
    actionDataSet.put(secondEntry.id(), updatedEntry);

    assertEquals(3, actionDataSet.size());
    assertEquals(1, actionDataSet.getPosition(updatedEntry));
    assertEquals("/say updated", actionDataSet.getEntry(secondEntry.id()).command());
    assertEquals(2, actionDataSet.getPosition(thirdEntry));
  }

  @Test
  @DisplayName("An unknown entry is appended instead of replacing another one")
  void testPutAppendsUnknownEntry() {
    ActionDataEntry firstEntry = createCommandEntry("/say first");
    ActionDataEntry secondEntry = createCommandEntry("/say second");
    ActionDataSet actionDataSet = createOrderedActionDataSet(firstEntry);

    actionDataSet.put(secondEntry.id(), secondEntry);

    assertEquals(2, actionDataSet.size());
    assertEquals(0, actionDataSet.getPosition(firstEntry));
    assertEquals(1, actionDataSet.getPosition(secondEntry));
  }

  @Test
  @DisplayName("Moving an entry up and down swaps it with its neighbour")
  void testMoveUpAndMoveDownChangeOrder() {
    ActionDataEntry firstEntry = createCommandEntry("/say first");
    ActionDataEntry secondEntry = createCommandEntry("/say second");
    ActionDataEntry thirdEntry = createCommandEntry("/say third");
    ActionDataSet actionDataSet = createOrderedActionDataSet(firstEntry, secondEntry, thirdEntry);

    actionDataSet.moveUp(thirdEntry);
    assertEquals(1, actionDataSet.getPosition(thirdEntry));
    assertEquals(2, actionDataSet.getPosition(secondEntry));

    actionDataSet.moveDown(firstEntry);
    assertEquals(1, actionDataSet.getPosition(firstEntry));
    assertEquals(0, actionDataSet.getPosition(thirdEntry));
    assertEquals(3, actionDataSet.size());
  }

  @Test
  @DisplayName("Moving beyond the first or last position is ignored")
  void testMoveBeyondBoundsIsIgnored() {
    ActionDataEntry firstEntry = createCommandEntry("/say first");
    ActionDataEntry secondEntry = createCommandEntry("/say second");
    ActionDataSet actionDataSet = createOrderedActionDataSet(firstEntry, secondEntry);

    actionDataSet.moveUp(firstEntry);
    actionDataSet.moveDown(secondEntry);
    actionDataSet.moveUp(createCommandEntry("/say unknown"));

    assertEquals(2, actionDataSet.size());
    assertEquals(0, actionDataSet.getPosition(firstEntry));
    assertEquals(1, actionDataSet.getPosition(secondEntry));
  }

  @Test
  @DisplayName("The order of the actions survives a save/load round trip")
  void testOrderSurvivesSaveAndLoad() {
    ActionDataEntry firstEntry = createCommandEntry("/say first");
    ActionDataEntry secondEntry = createCommandEntry("/say second");
    ActionDataEntry thirdEntry = createCommandEntry("/say third");
    ActionDataSet actionDataSet = createOrderedActionDataSet(firstEntry, secondEntry, thirdEntry);
    actionDataSet.moveUp(thirdEntry);

    ActionDataSet reloaded = new ActionDataSet(actionDataSet.createTag());

    assertEquals(
        List.of("/say first", "/say third", "/say second"),
        reloaded.getEntries().stream().map(ActionDataEntry::command).toList());
  }

  @Test
  @DisplayName("The same entry is not added twice")
  void testIdenticalEntryIsNotAddedTwice() {
    ActionDataEntry entry = createCommandEntry("/say once");
    ActionDataSet actionDataSet = createOrderedActionDataSet(entry, entry);

    assertEquals(1, actionDataSet.size());
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
