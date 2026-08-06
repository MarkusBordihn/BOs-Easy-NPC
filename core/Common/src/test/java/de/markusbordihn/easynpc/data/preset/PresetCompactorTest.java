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

package de.markusbordihn.easynpc.data.preset;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class PresetCompactorTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static CompoundTag actionData(CompoundTag actionEventSet) {
    CompoundTag actionDataTag = new CompoundTag();
    actionDataTag.put(ActionEventSet.DATA_ACTION_EVENT_SET_TAG, actionEventSet);

    CompoundTag entityData = new CompoundTag();
    entityData.put(ActionEventDataCapable.DATA_ACTION_DATA_TAG, actionDataTag);
    return entityData;
  }

  private static CompoundTag actionEvent(String actionEventName, String actionDataType) {
    CompoundTag actionEntry = new CompoundTag();
    actionEntry.putString("Type", actionDataType);

    ListTag actionEntries = new ListTag();
    actionEntries.add(actionEntry);

    CompoundTag actionEventSet = new CompoundTag();
    actionEventSet.put(actionEventName, actionEntries);
    return actionEventSet;
  }

  private static CompoundTag attribute(String name, double baseValue) {
    CompoundTag attribute = new CompoundTag();
    attribute.putString("id", name);
    attribute.putDouble("base", baseValue);
    return attribute;
  }

  private static ListTag attributes(CompoundTag... attributes) {
    ListTag attributeList = new ListTag();
    for (CompoundTag attribute : attributes) {
      attributeList.add(attribute);
    }
    return attributeList;
  }

  @Test
  @DisplayName("An attribute at the default of its NPC type is dropped without a reference entry")
  void testUnlistedDefaultAttributeIsDropped() {
    CompoundTag reference = new CompoundTag();
    reference.putString(PresetData.ID_TAG, "minecraft:pig");
    CompoundTag entityData = reference.copy();
    entityData.put("attributes", attributes(attribute("minecraft:max_health", 10.0D)));

    CompoundTag compacted = PresetCompactor.compact(entityData, reference);

    assertFalse(compacted.contains("attributes"));
  }

  @Test
  @DisplayName("An attribute above the default of its NPC type is kept without a reference entry")
  void testUnlistedChangedAttributeIsKept() {
    CompoundTag reference = new CompoundTag();
    reference.putString(PresetData.ID_TAG, "minecraft:pig");
    CompoundTag entityData = reference.copy();
    entityData.put("attributes", attributes(attribute("minecraft:max_health", 40.0D)));

    CompoundTag compacted = PresetCompactor.compact(entityData, reference);

    assertEquals(
        40.0D,
        compacted.getListOrEmpty("attributes").getCompoundOrEmpty(0).getDoubleOr("base", 0.0D));
  }

  @Test
  @DisplayName("A value equal to the reference is dropped")
  void testEqualValueIsDropped() {
    CompoundTag reference = new CompoundTag();
    reference.putBoolean("CanPickUpLoot", false);
    reference.putFloat("Health", 20.0F);

    CompoundTag entityData = reference.copy();
    entityData.putString(PresetData.ID_TAG, "easy_npc:humanoid");

    CompoundTag compacted = PresetCompactor.compact(entityData, reference);

    assertFalse(compacted.contains("CanPickUpLoot"));
    assertFalse(compacted.contains("Health"));
    assertEquals("easy_npc:humanoid", compacted.getStringOr(PresetData.ID_TAG, ""));
  }

  @Test
  @DisplayName("A value which differs from the reference is kept")
  void testDifferentValueIsKept() {
    CompoundTag reference = new CompoundTag();
    reference.putFloat("Health", 20.0F);

    CompoundTag entityData = new CompoundTag();
    entityData.putFloat("Health", 40.0F);

    CompoundTag compacted = PresetCompactor.compact(entityData, reference);

    assertEquals(40.0F, compacted.getFloatOr("Health", 0.0F));
  }

  @Test
  @DisplayName("A tag the reference does not know is kept")
  void testUnknownTagIsKept() {
    CompoundTag entityData = new CompoundTag();
    entityData.putString("CustomName", "Trader");

    CompoundTag compacted = PresetCompactor.compact(entityData, new CompoundTag());

    assertEquals("Trader", compacted.getStringOr("CustomName", ""));
  }

  @Test
  @DisplayName("A list is only dropped when it is equal as a whole")
  void testListIsComparedAsWhole() {
    ListTag referenceList = new ListTag();
    referenceList.add(StringTag.valueOf("one"));
    referenceList.add(StringTag.valueOf("two"));
    CompoundTag reference = new CompoundTag();
    reference.put("Values", referenceList);

    ListTag changedList = new ListTag();
    changedList.add(StringTag.valueOf("one"));
    changedList.add(StringTag.valueOf("three"));
    CompoundTag entityData = new CompoundTag();
    entityData.put("Values", changedList);

    CompoundTag compacted = PresetCompactor.compact(entityData, reference);

    assertEquals(changedList, compacted.get("Values"));
  }

  @Test
  @DisplayName("Only the changed part of a nested compound is kept")
  void testNestedCompoundIsCompacted() {
    CompoundTag referenceAttributes = new CompoundTag();
    referenceAttributes.putBoolean("CanFloat", false);
    referenceAttributes.putBoolean("CanOpenDoor", false);
    CompoundTag reference = new CompoundTag();
    reference.put("EntityAttribute", referenceAttributes);

    CompoundTag entityAttributes = new CompoundTag();
    entityAttributes.putBoolean("CanFloat", false);
    entityAttributes.putBoolean("CanOpenDoor", true);
    CompoundTag entityData = new CompoundTag();
    entityData.put("EntityAttribute", entityAttributes);

    CompoundTag compacted = PresetCompactor.compact(entityData, reference);

    CompoundTag compactedAttributes = compacted.getCompoundOrEmpty("EntityAttribute");
    assertFalse(compactedAttributes.contains("CanFloat"));
    assertTrue(compactedAttributes.getBooleanOr("CanOpenDoor", false));
  }

  @Test
  @DisplayName("The objective data is kept even when it is equal to the reference")
  void testObjectiveDataIsAlwaysKept() {
    CompoundTag objectiveData = new CompoundTag();
    objectiveData.putBoolean("HasObjectives", true);
    CompoundTag reference = new CompoundTag();
    reference.put(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG, objectiveData);

    CompoundTag compacted = PresetCompactor.compact(reference.copy(), reference);

    assertTrue(compacted.contains(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG));
  }

  @Test
  @DisplayName("The status timestamps are dropped but the finalized flag is kept")
  void testStatusTimestampsAreDropped() {
    CompoundTag statusData = new CompoundTag();
    statusData.putBoolean(StatusDataType.FINALIZED.getTagName(), true);
    statusData.putLong(StatusDataType.NPC_DATA_LAST_UPDATE.getTagName(), 1768693861652L);
    statusData.putLong(StatusDataType.NPC_DATA_LAST_SAVED.getTagName(), 1768693861652L);
    CompoundTag entityData = new CompoundTag();
    entityData.put(StatusDataCapable.DATA_STATUS_DATA_TAG, statusData);

    CompoundTag compacted = PresetCompactor.compact(entityData, new CompoundTag());

    CompoundTag compactedStatus =
        compacted.getCompoundOrEmpty(StatusDataCapable.DATA_STATUS_DATA_TAG);
    assertTrue(compactedStatus.getBooleanOr(StatusDataType.FINALIZED.getTagName(), false));
    assertFalse(compactedStatus.contains(StatusDataType.NPC_DATA_LAST_UPDATE.getTagName()));
    assertFalse(compactedStatus.contains(StatusDataType.NPC_DATA_LAST_SAVED.getTagName()));
  }

  @Test
  @DisplayName("A preset exported from a live NPC does not carry its fired spawn action")
  void testSpawnActionFiredIsNotExported() {
    CompoundTag statusData = new CompoundTag();
    statusData.putBoolean(StatusDataType.FINALIZED.getTagName(), true);
    statusData.putBoolean(StatusDataType.SPAWN_ACTION_FIRED.getTagName(), true);
    CompoundTag entityData = new CompoundTag();
    entityData.put(StatusDataCapable.DATA_STATUS_DATA_TAG, statusData);

    CompoundTag referenceStatusData = new CompoundTag();
    referenceStatusData.putBoolean(StatusDataType.FINALIZED.getTagName(), true);
    CompoundTag reference = new CompoundTag();
    reference.put(StatusDataCapable.DATA_STATUS_DATA_TAG, referenceStatusData);

    CompoundTag compacted = PresetCompactor.compact(entityData, reference);
    CompoundTag expanded = PresetCompactor.expand(compacted, reference);

    assertFalse(
        expanded
            .getCompoundOrEmpty(StatusDataCapable.DATA_STATUS_DATA_TAG)
            .contains(StatusDataType.SPAWN_ACTION_FIRED.getTagName()));
  }

  @Test
  @DisplayName("Expanding a compact preset restores the reference values")
  void testExpandRestoresReferenceValues() {
    CompoundTag reference = new CompoundTag();
    reference.putFloat("Health", 20.0F);
    reference.putBoolean("CanPickUpLoot", false);

    CompoundTag entityData = reference.copy();
    entityData.putString("CustomName", "Trader");
    entityData.putBoolean("CanPickUpLoot", true);

    CompoundTag compacted = PresetCompactor.compact(entityData, reference);
    CompoundTag expanded = PresetCompactor.expand(compacted, reference);

    assertEquals(20.0F, expanded.getFloatOr("Health", 0.0F));
    assertTrue(expanded.getBooleanOr("CanPickUpLoot", false));
    assertEquals("Trader", expanded.getStringOr("CustomName", ""));
  }

  @Test
  @DisplayName("An attribute equal to the NPC reference is dropped")
  void testNpcDefaultAttributeIsDropped() {
    CompoundTag reference = new CompoundTag();
    reference.put("attributes", attributes(attribute("minecraft:movement_speed", 0.6D)));

    CompoundTag compacted = PresetCompactor.compact(reference.copy(), reference);

    assertFalse(compacted.contains("attributes"));
  }

  @Test
  @DisplayName("A vanilla default differing from the NPC reference survives compact and expand")
  void testNpcSpecificAttributeDefaultIsUsed() {
    CompoundTag reference = new CompoundTag();
    reference.put(
        "attributes",
        attributes(
            attribute("minecraft:movement_speed", 0.6D), attribute("minecraft:max_health", 20.0D)));
    CompoundTag entityData = reference.copy();
    entityData.put(
        "attributes",
        attributes(
            attribute("minecraft:movement_speed", 0.7D), attribute("minecraft:max_health", 20.0D)));

    CompoundTag compacted = PresetCompactor.compact(entityData, reference);
    CompoundTag expanded = PresetCompactor.expand(compacted, reference);
    ListTag compactedAttributes = compacted.getListOrEmpty("attributes");
    ListTag expandedAttributes = expanded.getListOrEmpty("attributes");

    assertEquals(1, compactedAttributes.size());
    assertEquals(0.7D, compactedAttributes.getCompoundOrEmpty(0).getDoubleOr("base", 0.0D));
    assertEquals(2, expandedAttributes.size());
    assertEquals(0.7D, expandedAttributes.getCompoundOrEmpty(0).getDoubleOr("base", 0.0D));
    assertEquals(20.0D, expandedAttributes.getCompoundOrEmpty(1).getDoubleOr("base", 0.0D));
  }

  @Test
  @DisplayName("An action event removed from the preset does not come back from the reference")
  void testRemovedActionEventStaysRemoved() {
    CompoundTag reference = actionData(actionEvent("ON_INTERACTION", "OPEN_DEFAULT_DIALOG"));
    CompoundTag entityData = actionData(actionEvent("ON_DEATH", "COMMAND"));

    CompoundTag compacted = PresetCompactor.compact(entityData, reference);
    CompoundTag expanded = PresetCompactor.expand(compacted, reference);
    CompoundTag actionEventSet =
        expanded
            .getCompoundOrEmpty(ActionEventDataCapable.DATA_ACTION_DATA_TAG)
            .getCompoundOrEmpty(ActionEventSet.DATA_ACTION_EVENT_SET_TAG);

    assertTrue(actionEventSet.contains("ON_DEATH"));
    assertFalse(actionEventSet.contains("ON_INTERACTION"));
  }

  @Test
  @DisplayName("A preset without action data falls back to the actions of the reference")
  void testMissingActionDataUsesReference() {
    CompoundTag reference = actionData(actionEvent("ON_INTERACTION", "OPEN_DEFAULT_DIALOG"));
    CompoundTag entityData = new CompoundTag();
    entityData.putString(PresetData.ID_TAG, "easy_npc:humanoid");

    CompoundTag expanded =
        PresetCompactor.expand(PresetCompactor.compact(entityData, reference), reference);
    CompoundTag actionEventSet =
        expanded
            .getCompoundOrEmpty(ActionEventDataCapable.DATA_ACTION_DATA_TAG)
            .getCompoundOrEmpty(ActionEventSet.DATA_ACTION_EVENT_SET_TAG);

    assertTrue(actionEventSet.contains("ON_INTERACTION"));
  }

  @Test
  @DisplayName("Action data equal to the reference is dropped and restored unchanged")
  void testUnchangedActionDataIsDropped() {
    CompoundTag reference = actionData(actionEvent("ON_INTERACTION", "OPEN_DEFAULT_DIALOG"));
    CompoundTag entityData = reference.copy();

    CompoundTag compacted = PresetCompactor.compact(entityData, reference);

    assertFalse(compacted.contains(ActionEventDataCapable.DATA_ACTION_DATA_TAG));
    assertEquals(reference, PresetCompactor.expand(compacted, reference));
  }
}
