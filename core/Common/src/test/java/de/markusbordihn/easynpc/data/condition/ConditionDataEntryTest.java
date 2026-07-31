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

import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ConditionDataEntryTest {

  @Test
  void testEmptyCondition() {
    ConditionDataEntry entry = ConditionDataEntry.EMPTY;

    assertEquals(ConditionType.NONE, entry.conditionType());
    assertEquals(ConditionOperationType.NONE, entry.operationType());
    assertFalse(entry.isValid());
  }

  @Test
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
  @DisplayName("A target NPC survives the tag round-trip and stays optional")
  void testTargetUuidRoundTrip() {
    UUID targetUUID = UUID.fromString("11111111-2222-3333-4444-555555555555");
    ConditionDataEntry entry =
        new ConditionDataEntry(
                ConditionType.NPC_STATE, ConditionOperationType.EQUALS, "easy_npc:quest", 1)
            .withTargetUUID(targetUUID);

    assertTrue(entry.hasTargetUUID());
    assertEquals(targetUUID, new ConditionDataEntry(entry.createTag()).targetUUID());

    ConditionDataEntry entryWithoutTarget = entry.withTargetUUID(null);
    assertFalse(entryWithoutTarget.hasTargetUUID());
    assertFalse(entryWithoutTarget.createTag().contains(ConditionDataEntry.DATA_TARGET_UUID_TAG));
  }

  @Test
  @DisplayName("The condition id is derived from the content and stays the same in every JVM")
  void testIdIsContentBased() {
    ConditionDataEntry entry =
        new ConditionDataEntry(
            ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test_score", 5);

    assertEquals(UUID.fromString("3ffef54f-1bc5-3edf-804e-4bdc593a7e9b"), entry.getId());
    assertEquals(
        UUID.fromString("96d1160a-7cca-3094-a7a0-93d247bf972b"),
        new ConditionDataEntry(
                ConditionType.WEATHER, WeatherType.RAIN, ConditionOperationType.NONE, "", 0)
            .getId());
  }

  @Test
  @DisplayName("The condition id survives the tag round-trip")
  void testIdRoundTrip() {
    ConditionDataEntry entry =
        new ConditionDataEntry(
                ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, " test_score ", 5)
            .withTargetUUID(UUID.fromString("11111111-2222-3333-4444-555555555555"));

    assertEquals(entry.getId(), new ConditionDataEntry(entry.createTag()).getId());
  }

  @Test
  @DisplayName("Conditions that differ in any component get their own id")
  void testIdDiffersPerComponent() {
    ConditionDataEntry entry =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5);

    assertNotEquals(entry.getId(), entry.withValue(6).getId());
    assertNotEquals(entry.getId(), entry.withName("other").getId());
    assertNotEquals(entry.getId(), entry.withConditionType(ConditionType.PLAYER_TAG).getId());
    assertNotEquals(
        entry.getId(), entry.withOperationType(ConditionOperationType.GREATER_THAN).getId());
  }

  @Test
  void testHasName() {
    ConditionDataEntry withName = new ConditionDataEntry(ConditionType.SCOREBOARD).withName("test");
    assertTrue(withName.hasName());

    ConditionDataEntry withoutName = new ConditionDataEntry(ConditionType.SCOREBOARD).withName("");
    assertFalse(withoutName.hasName());

    ConditionDataEntry nullName = new ConditionDataEntry(ConditionType.SCOREBOARD);
    assertFalse(nullName.hasName());
  }

  @Test
  void testWithSubType() {
    ConditionDataEntry original = new ConditionDataEntry(ConditionType.HAS_ITEM_IN_HAND);
    ConditionDataEntry updated = original.withSubType(HandItemType.MAIN_HAND);

    assertEquals(HandItemType.MAIN_HAND, updated.subType());
    assertNull(original.subType());
  }

  @Test
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
  void testWithName() {
    ConditionDataEntry original = new ConditionDataEntry(ConditionType.SCOREBOARD);
    ConditionDataEntry updated = original.withName("test_score");

    assertEquals("test_score", updated.name());
  }

  @Test
  void testWithValue() {
    ConditionDataEntry original = new ConditionDataEntry(ConditionType.SCOREBOARD);
    ConditionDataEntry updated = original.withValue(10);

    assertEquals(10, updated.value());
  }

  @Test
  void testSubTypeAbsentInNBTWhenNull() {
    ConditionDataEntry entry =
        new ConditionDataEntry(ConditionType.HAS_ITEM_IN_INVENTORY).withName("minecraft:diamond");
    CompoundTag tag = entry.createTag();

    assertFalse(tag.contains(ConditionDataEntry.DATA_SUB_TYPE_TAG));
  }

  @Test
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
  void testExecutionLimitWithDurationTypeNBTRoundTrip() {
    ConditionDataEntry original =
        new ConditionDataEntry(
            ConditionType.EXECUTION_LIMIT,
            DurationType.PER_HOUR,
            ConditionOperationType.NONE,
            "",
            5);
    assertTrue(original.isValid());

    CompoundTag tag = original.createTag();
    assertEquals("PER_HOUR", tag.getString(ConditionDataEntry.DATA_SUB_TYPE_TAG));

    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.EXECUTION_LIMIT, deserialized.conditionType());
    assertEquals(DurationType.PER_HOUR, deserialized.subType());
    assertEquals(5, deserialized.value());
  }

  @Test
  void testExecutionLimitLegacyTextSubTypeNBT() {
    CompoundTag tag = new CompoundTag();
    tag.putString(ConditionDataEntry.DATA_TYPE_TAG, "EXECUTION_LIMIT");
    tag.putString(ConditionDataEntry.DATA_LEGACY_TEXT_TAG, "PER_DAY");
    tag.putInt(ConditionDataEntry.DATA_VALUE_TAG, 1);

    ConditionDataEntry deserialized = new ConditionDataEntry(tag);

    assertEquals(ConditionType.EXECUTION_LIMIT, deserialized.conditionType());
    assertEquals(DurationType.PER_DAY, deserialized.subType());
    assertEquals(1, deserialized.value());
    assertTrue(deserialized.isValid());
  }

  @Test
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
  void testHashCode() {
    ConditionDataEntry entry1 =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5);
    ConditionDataEntry entry2 =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5);

    assertEquals(entry1.hashCode(), entry2.hashCode());
  }

  @Test
  void testConsistentUUIDGeneration() {
    ConditionDataEntry entry1 =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5);
    ConditionDataEntry entry2 =
        new ConditionDataEntry(ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "test", 5);

    assertEquals(entry1.getId(), entry2.getId());
  }

  @Test
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

  @Test
  void testHasItemInInventoryConditionRequiresName() {
    ConditionDataEntry withoutName = new ConditionDataEntry(ConditionType.HAS_ITEM_IN_INVENTORY);
    assertFalse(withoutName.isValid());

    ConditionDataEntry withName =
        new ConditionDataEntry(ConditionType.HAS_ITEM_IN_INVENTORY).withName("minecraft:diamond");
    assertTrue(withName.isValid());
  }

  @Test
  void testHasItemInHandConditionRequiresName() {
    ConditionDataEntry withoutName = new ConditionDataEntry(ConditionType.HAS_ITEM_IN_HAND);
    assertFalse(withoutName.isValid());

    ConditionDataEntry withName =
        new ConditionDataEntry(ConditionType.HAS_ITEM_IN_HAND).withName("minecraft:torch");
    assertTrue(withName.isValid());
  }

  @Test
  void testHasItemInHandNBTRoundTripMainHand() {
    ConditionDataEntry original =
        new ConditionDataEntry(
            ConditionType.HAS_ITEM_IN_HAND,
            HandItemType.MAIN_HAND,
            ConditionOperationType.NOT_EQUALS,
            "minecraft:gold_ingot",
            0);
    CompoundTag tag = original.createTag();

    assertEquals("HAS_ITEM_IN_HAND", tag.getString(ConditionDataEntry.DATA_TYPE_TAG));
    assertEquals("MAIN_HAND", tag.getString(ConditionDataEntry.DATA_SUB_TYPE_TAG));
    assertEquals("NOT_EQUALS", tag.getString(ConditionDataEntry.DATA_OPERATION_TAG));

    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.HAS_ITEM_IN_HAND, deserialized.conditionType());
    assertEquals(HandItemType.MAIN_HAND, deserialized.subType());
    assertEquals(ConditionOperationType.NOT_EQUALS, deserialized.operationType());
    assertEquals("minecraft:gold_ingot", deserialized.name());
    assertEquals(original.getId(), deserialized.getId());
  }

  @Test
  void testHasItemInHandNBTRoundTripOffHand() {
    ConditionDataEntry original =
        new ConditionDataEntry(
            ConditionType.HAS_ITEM_IN_HAND,
            HandItemType.OFF_HAND,
            ConditionOperationType.EQUALS,
            "minecraft:shield",
            0);
    CompoundTag tag = original.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);

    assertEquals(HandItemType.OFF_HAND, deserialized.subType());
    assertEquals("minecraft:shield", deserialized.name());
  }

  @Test
  void testHasItemInHandNBTRoundTripBoth() {
    ConditionDataEntry original =
        new ConditionDataEntry(ConditionType.HAS_ITEM_IN_HAND)
            .withSubType(HandItemType.BOTH)
            .withName("minecraft:diamond");
    CompoundTag tag = original.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);

    assertEquals(HandItemType.BOTH, deserialized.subType());
    assertEquals("minecraft:diamond", deserialized.name());
  }

  @Test
  @DisplayName("Item quantity > 1 is serialized and round-trips")
  void testHasItemQuantityNBTRoundTrip() {
    ConditionDataEntry original =
        new ConditionDataEntry(ConditionType.HAS_ITEM_IN_INVENTORY)
            .withName("minecraft:diamond")
            .withValue(10);
    assertTrue(original.isValid());

    CompoundTag tag = original.createTag();
    assertEquals(10, tag.getInt(ConditionDataEntry.DATA_VALUE_TAG));

    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals("minecraft:diamond", deserialized.name());
    assertEquals(10, deserialized.value());
  }

  @Test
  @DisplayName("Item custom data is serialized and round-trips")
  void testHasItemCustomDataNBTRoundTrip() {
    String customData = "{display:{Name:'{\"text\":\"Quest Diamond\"}'}}";
    ConditionDataEntry original =
        new ConditionDataEntry(ConditionType.HAS_ITEM_IN_INVENTORY)
            .withName("minecraft:diamond")
            .withCustomData(customData);

    CompoundTag tag = original.createTag();
    assertEquals(customData, tag.getString(ConditionDataEntry.DATA_CUSTOM_DATA_TAG));

    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals("minecraft:diamond", deserialized.name());
    assertEquals(customData, deserialized.customData());
    assertEquals(original.getId(), deserialized.getId());
  }

  @Test
  @DisplayName("Missing item custom data keeps legacy conditions compatible")
  void testHasItemCustomDataBackwardCompatibility() {
    CompoundTag tag = new CompoundTag();
    tag.putString(ConditionDataEntry.DATA_TYPE_TAG, "HAS_ITEM_IN_INVENTORY");
    tag.putString(ConditionDataEntry.DATA_NAME_TAG, "minecraft:diamond");

    ConditionDataEntry deserialized = new ConditionDataEntry(tag);

    assertEquals("minecraft:diamond", deserialized.name());
    assertEquals("", deserialized.customData());
    assertFalse(deserialized.hasCustomData());
    assertTrue(deserialized.isValid());
  }

  @Test
  @DisplayName("Default item quantity (0 or 1) is not serialized")
  void testHasItemDefaultQuantityNotSerialized() {
    ConditionDataEntry defaultQuantity =
        new ConditionDataEntry(ConditionType.HAS_ITEM_IN_INVENTORY).withName("minecraft:diamond");
    assertFalse(defaultQuantity.createTag().contains(ConditionDataEntry.DATA_VALUE_TAG));

    ConditionDataEntry handDefault =
        new ConditionDataEntry(
            ConditionType.HAS_ITEM_IN_HAND,
            HandItemType.MAIN_HAND,
            ConditionOperationType.EQUALS,
            "minecraft:gold_ingot",
            0);
    assertFalse(handDefault.createTag().contains(ConditionDataEntry.DATA_VALUE_TAG));
  }

  @Test
  void testAdvancementConditionRequiresName() {
    ConditionDataEntry withoutName = new ConditionDataEntry(ConditionType.ADVANCEMENT);
    assertFalse(withoutName.isValid());

    ConditionDataEntry withName =
        new ConditionDataEntry(ConditionType.ADVANCEMENT).withName("minecraft:story/obtain_armor");
    assertTrue(withName.isValid());

    CompoundTag tag = withName.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.ADVANCEMENT, deserialized.conditionType());
    assertEquals("minecraft:story/obtain_armor", deserialized.name());
  }

  @Test
  void testExperienceLevelConditionRequiresValueAndOperation() {
    ConditionDataEntry withoutOperation =
        new ConditionDataEntry(ConditionType.EXPERIENCE_LEVEL, ConditionOperationType.NONE, "", 10);
    assertFalse(withoutOperation.isValid());

    ConditionDataEntry valid =
        new ConditionDataEntry(
            ConditionType.EXPERIENCE_LEVEL, ConditionOperationType.GREATER_THAN_OR_EQUALS, "", 10);
    assertTrue(valid.isValid());

    CompoundTag tag = valid.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.EXPERIENCE_LEVEL, deserialized.conditionType());
    assertEquals(ConditionOperationType.GREATER_THAN_OR_EQUALS, deserialized.operationType());
    assertEquals(10, deserialized.value());
  }

  @Test
  void testPlayerTagConditionRequiresName() {
    ConditionDataEntry withoutName = new ConditionDataEntry(ConditionType.PLAYER_TAG);
    assertFalse(withoutName.isValid());

    ConditionDataEntry withName =
        new ConditionDataEntry(ConditionType.PLAYER_TAG).withName("quest_completed");
    assertTrue(withName.isValid());

    CompoundTag tag = withName.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.PLAYER_TAG, deserialized.conditionType());
    assertEquals("quest_completed", deserialized.name());
    assertEquals(withName.getId(), deserialized.getId());
  }

  @Test
  void testTeamConditionNBTRoundTrip() {
    ConditionDataEntry withoutName = new ConditionDataEntry(ConditionType.TEAM);
    assertFalse(withoutName.isValid());

    ConditionDataEntry withName = new ConditionDataEntry(ConditionType.TEAM).withName("red_team");
    assertTrue(withName.isValid());

    CompoundTag tag = withName.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.TEAM, deserialized.conditionType());
    assertEquals("red_team", deserialized.name());
  }

  @Test
  void testGamemodeConditionRequiresName() {
    ConditionDataEntry withoutName = new ConditionDataEntry(ConditionType.GAMEMODE);
    assertFalse(withoutName.isValid());

    ConditionDataEntry withName =
        new ConditionDataEntry(ConditionType.GAMEMODE).withName("survival");
    assertTrue(withName.isValid());
  }

  @Test
  void testGamemodeConditionNBTRoundTrip() {
    ConditionDataEntry withoutName = new ConditionDataEntry(ConditionType.GAMEMODE);
    assertFalse(withoutName.isValid());

    ConditionDataEntry withName =
        new ConditionDataEntry(ConditionType.GAMEMODE).withName("survival");
    assertTrue(withName.isValid());

    CompoundTag tag = withName.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.GAMEMODE, deserialized.conditionType());
    assertEquals("survival", deserialized.name());
    assertEquals(withName.getId(), deserialized.getId());
  }

  @Test
  void testTimeOfDayConditionRequiresOperation() {
    ConditionDataEntry withoutOperation =
        new ConditionDataEntry(ConditionType.TIME_OF_DAY, ConditionOperationType.NONE, "", 13000);
    assertFalse(withoutOperation.isValid());

    ConditionDataEntry valid =
        new ConditionDataEntry(
            ConditionType.TIME_OF_DAY, ConditionOperationType.GREATER_THAN_OR_EQUALS, "", 13000);
    assertTrue(valid.isValid());
  }

  @Test
  void testTimeOfDayConditionNBTRoundTrip() {
    ConditionDataEntry original =
        new ConditionDataEntry(
            ConditionType.TIME_OF_DAY, ConditionOperationType.LESS_THAN, "", 6000);
    assertTrue(original.isValid());

    CompoundTag tag = original.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.TIME_OF_DAY, deserialized.conditionType());
    assertEquals(ConditionOperationType.LESS_THAN, deserialized.operationType());
    assertEquals(6000, deserialized.value());
    assertEquals(original.getId(), deserialized.getId());
  }

  @Test
  void testWeatherConditionRequiresSubType() {
    ConditionDataEntry withoutSubType = new ConditionDataEntry(ConditionType.WEATHER);
    assertFalse(withoutSubType.isValid());

    ConditionDataEntry valid =
        new ConditionDataEntry(ConditionType.WEATHER).withSubType(WeatherType.CLEAR);
    assertTrue(valid.isValid());
  }

  @Test
  void testWeatherConditionNBTRoundTrip() {
    ConditionDataEntry original =
        new ConditionDataEntry(ConditionType.WEATHER).withSubType(WeatherType.THUNDER);
    assertTrue(original.isValid());

    CompoundTag tag = original.createTag();
    assertEquals("THUNDER", tag.getString(ConditionDataEntry.DATA_SUB_TYPE_TAG));

    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.WEATHER, deserialized.conditionType());
    assertEquals(WeatherType.THUNDER, deserialized.subType());
    assertEquals(original.getId(), deserialized.getId());
  }

  @Test
  void testFallbackConditionIsAlwaysValid() {
    ConditionDataEntry fallback = new ConditionDataEntry(ConditionType.FALLBACK);
    assertTrue(fallback.isValid());

    CompoundTag tag = fallback.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.FALLBACK, deserialized.conditionType());
    assertEquals(fallback.getId(), deserialized.getId());
  }

  @Test
  void testPlayerHealthConditionRequiresOperation() {
    ConditionDataEntry withoutOperation =
        new ConditionDataEntry(ConditionType.PLAYER_HEALTH, ConditionOperationType.NONE, "", 50);
    assertFalse(withoutOperation.isValid());

    ConditionDataEntry valid =
        new ConditionDataEntry(
            ConditionType.PLAYER_HEALTH, ConditionOperationType.LESS_THAN, "", 50);
    assertTrue(valid.isValid());
  }

  @Test
  void testPlayerHealthConditionNBTRoundTrip() {
    ConditionDataEntry original =
        new ConditionDataEntry(
            ConditionType.PLAYER_HEALTH, ConditionOperationType.GREATER_THAN_OR_EQUALS, "", 50);
    assertTrue(original.isValid());

    CompoundTag tag = original.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.PLAYER_HEALTH, deserialized.conditionType());
    assertEquals(ConditionOperationType.GREATER_THAN_OR_EQUALS, deserialized.operationType());
    assertEquals(50, deserialized.value());
    assertEquals(original.getId(), deserialized.getId());
  }

  @Test
  void testNpcHealthConditionRequiresOperation() {
    ConditionDataEntry withoutOperation =
        new ConditionDataEntry(ConditionType.NPC_HEALTH, ConditionOperationType.NONE, "", 50);
    assertFalse(withoutOperation.isValid());

    ConditionDataEntry valid =
        new ConditionDataEntry(ConditionType.NPC_HEALTH, ConditionOperationType.LESS_THAN, "", 50);
    assertTrue(valid.isValid());
  }

  @Test
  void testNpcHealthConditionNBTRoundTrip() {
    ConditionDataEntry original =
        new ConditionDataEntry(ConditionType.NPC_HEALTH, ConditionOperationType.LESS_THAN, "", 50);
    assertTrue(original.isValid());

    CompoundTag tag = original.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.NPC_HEALTH, deserialized.conditionType());
    assertEquals(ConditionOperationType.LESS_THAN, deserialized.operationType());
    assertEquals(50, deserialized.value());
    assertEquals(original.getId(), deserialized.getId());
  }

  @Test
  void testEntityHealthConditionRequiresValidUuid() {
    ConditionDataEntry withoutName =
        new ConditionDataEntry(
            ConditionType.ENTITY_HEALTH, ConditionOperationType.LESS_THAN, "", 50);
    assertFalse(withoutName.isValid());

    ConditionDataEntry invalidUuid =
        new ConditionDataEntry(
            ConditionType.ENTITY_HEALTH, ConditionOperationType.LESS_THAN, "not-a-uuid", 50);
    assertFalse(invalidUuid.isValid());

    ConditionDataEntry validUuid =
        new ConditionDataEntry(
            ConditionType.ENTITY_HEALTH,
            ConditionOperationType.LESS_THAN,
            "12345678-1234-1234-1234-123456789abc",
            50);
    assertTrue(validUuid.isValid());

    ConditionDataEntry missingOperation =
        new ConditionDataEntry(
            ConditionType.ENTITY_HEALTH,
            ConditionOperationType.NONE,
            "12345678-1234-1234-1234-123456789abc",
            50);
    assertFalse(missingOperation.isValid());
  }

  @Test
  void testEntityHealthConditionNBTRoundTrip() {
    ConditionDataEntry original =
        new ConditionDataEntry(
            ConditionType.ENTITY_HEALTH,
            ConditionOperationType.LESS_THAN_OR_EQUALS,
            "12345678-1234-1234-1234-123456789abc",
            25);
    assertTrue(original.isValid());

    CompoundTag tag = original.createTag();
    ConditionDataEntry deserialized = new ConditionDataEntry(tag);
    assertEquals(ConditionType.ENTITY_HEALTH, deserialized.conditionType());
    assertEquals(ConditionOperationType.LESS_THAN_OR_EQUALS, deserialized.operationType());
    assertEquals("12345678-1234-1234-1234-123456789abc", deserialized.name());
    assertEquals(25, deserialized.value());
    assertEquals(original.getId(), deserialized.getId());
  }
}
