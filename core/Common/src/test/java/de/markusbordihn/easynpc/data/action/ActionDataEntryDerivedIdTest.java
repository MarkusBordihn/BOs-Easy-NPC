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

package de.markusbordihn.easynpc.data.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import java.util.List;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ActionDataEntryDerivedIdTest {

  private CompoundTag createActionTag(String command) {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putString(ActionDataEntry.DATA_TYPE_TAG, ActionDataType.COMMAND.name());
    compoundTag.putString(ActionDataEntry.DATA_COMMAND_TAG, command);
    return compoundTag;
  }

  @Test
  @DisplayName("A datapack action without an id keeps the same id across reloads")
  void testDerivedIdIsStableAcrossReloads() {
    ListTag actionDataList = new ListTag();
    actionDataList.add(this.createActionTag("say hello"));

    ActionDataSet firstLoad = new ActionDataSet().load(actionDataList);
    ActionDataSet secondLoad = new ActionDataSet().load(actionDataList);

    assertEquals(
        firstLoad.getEntries().iterator().next().id(),
        secondLoad.getEntries().iterator().next().id());
  }

  @Test
  @DisplayName("Two identical actions in one list stay distinct through their position")
  void testDerivedIdDiffersPerPosition() {
    ListTag actionDataList = new ListTag();
    actionDataList.add(this.createActionTag("say hello"));
    actionDataList.add(this.createActionTag("say hello"));

    ActionDataSet actionDataSet = new ActionDataSet().load(actionDataList);

    assertEquals(2, actionDataSet.size());
    assertNotEquals(
        ActionDataEntry.deriveId(this.createActionTag("say hello"), 0),
        ActionDataEntry.deriveId(this.createActionTag("say hello"), 1));
  }

  @Test
  void testDerivedIdDiffersPerContent() {
    assertNotEquals(
        ActionDataEntry.deriveId(this.createActionTag("say hello"), 0),
        ActionDataEntry.deriveId(this.createActionTag("say goodbye"), 0));
  }

  @Test
  @DisplayName("Optional fields at their default value never reach the tag")
  void testDefaultFieldsAreNotWritten() {
    CompoundTag compoundTag = new ActionDataEntry(ActionDataType.COMMAND, "say hello").createTag();

    assertFalse(compoundTag.contains(ActionDataEntry.DATA_MESSAGE_TAG));
    assertFalse(compoundTag.contains(ActionDataEntry.DATA_PERMISSION_LEVEL_TAG));
    assertFalse(compoundTag.contains(ActionDataEntry.DATA_EXECUTE_AS_USER_TAG));
    assertFalse(compoundTag.contains(ActionDataEntry.DATA_DEBUG_TAG));
  }

  private CompoundTag createIdentityTag(ActionDataEntry actionDataEntry) {
    CompoundTag compoundTag = actionDataEntry.createTag();
    compoundTag.remove(ActionDataEntry.DATA_ID_TAG);
    return compoundTag;
  }

  @Test
  @DisplayName("Default message action data does not change an existing derived id")
  void testDerivedIdIsUnaffectedByTheDefaultMessageActionData() {
    ActionDataEntry entry =
        new ActionDataEntry(ActionDataType.COMMAND, "say hello")
            .withMessageActionData(MessageActionData.DEFAULT);

    assertEquals(
        ActionDataEntry.deriveId(this.createActionTag("say hello"), 0),
        ActionDataEntry.deriveId(this.createIdentityTag(entry), 0));
  }

  @Test
  @DisplayName("Non-default message action data is part of the identity")
  void testDerivedIdChangesWithNonDefaultMessageActionData() {
    ActionDataEntry entry =
        new ActionDataEntry(ActionDataType.MESSAGE)
            .withMessageActionData(MessageActionData.DEFAULT.withTexts(List.of("hello")));

    assertNotEquals(
        ActionDataEntry.deriveId(this.createIdentityTag(entry), 0),
        ActionDataEntry.deriveId(
            this.createIdentityTag(
                entry.withMessageActionData(
                    entry
                        .messageActionData()
                        .withShowInNearbyChat(false)
                        .withShowAsSpeechBubble(true))),
            0));
  }

  @Test
  @DisplayName("An authored id always wins over the derived one")
  void testAuthoredIdWins() {
    CompoundTag compoundTag = this.createActionTag("say hello");
    ActionDataEntry authoredEntry =
        new ActionDataEntry(ActionDataType.COMMAND).withCommand("say hello");

    ListTag actionDataList = new ListTag();
    actionDataList.add(authoredEntry.createTag());

    assertEquals(
        authoredEntry.id(),
        new ActionDataSet().load(actionDataList).getEntries().iterator().next().id());
    assertNotEquals(authoredEntry.id(), ActionDataEntry.deriveId(compoundTag, 0));
  }
}
