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

package de.markusbordihn.easynpc.data.objective;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import net.minecraft.world.phys.Vec3;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ObjectiveDataEntryTest {

  @Test
  void testTargetTeamNameRoundTrip() {
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.ATTACK_ENTITY_BY_TEAM);
    entry.setTargetTeamName("red_team");

    ObjectiveDataEntry restored = new ObjectiveDataEntry(entry.createTag());
    assertEquals(ObjectiveType.ATTACK_ENTITY_BY_TEAM, restored.getType());
    assertEquals("red_team", restored.getTargetTeamName());
  }

  @Test
  void testTargetEntityTagRoundTrip() {
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.ATTACK_ENTITY_BY_TAG);
    entry.setTargetEntityTag("bandit");

    ObjectiveDataEntry restored = new ObjectiveDataEntry(entry.createTag());
    assertEquals(ObjectiveType.ATTACK_ENTITY_BY_TAG, restored.getType());
    assertEquals("bandit", restored.getTargetEntityTag());
  }

  @Test
  void testTargetPlayerNameRoundTripForAttackPlayerByName() {
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.ATTACK_PLAYER_BY_NAME);
    entry.setTargetPlayerName("Steve");

    ObjectiveDataEntry restored = new ObjectiveDataEntry(entry.createTag());
    assertEquals("Steve", restored.getTargetPlayerName());
    assertTrue(restored.hasPlayerTarget());
  }

  @Test
  void testTargetEntityUUIDRoundTripForAttackEntityByUUID() {
    UUID targetUUID = UUID.randomUUID();
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.ATTACK_ENTITY_BY_UUID);
    entry.setTargetEntityUUID(targetUUID);

    ObjectiveDataEntry restored = new ObjectiveDataEntry(entry.createTag());
    assertEquals(targetUUID, restored.getTargetEntityUUID());
    assertTrue(restored.hasEntityTarget());
  }

  @Test
  void testBackwardCompatibleLoadWithoutNewFields() {
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.ATTACK_PLAYER);
    ObjectiveDataEntry restored = new ObjectiveDataEntry(entry.createTag());

    assertNull(restored.getTargetTeamName());
    assertNull(restored.getTargetEntityTag());
    assertFalse(restored.hasPlayerTarget());
  }

  @Test
  void testAttackPlayerSaveContainsNoNewTags() {
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.ATTACK_PLAYER);
    CompoundTag compoundTag = entry.createTag();

    assertFalse(compoundTag.contains(ObjectiveDataEntry.DATA_TARGET_TEAM_NAME_TAG));
    assertFalse(compoundTag.contains(ObjectiveDataEntry.DATA_TARGET_ENTITY_TAG_TAG));
  }

  @Test
  void testIdDefaultsToObjectiveTypeName() {
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.ATTACK_HOSTILE_FACTIONS);
    assertEquals(ObjectiveType.ATTACK_HOSTILE_FACTIONS.name(), entry.getId());

    ObjectiveDataEntry restored = new ObjectiveDataEntry(entry.createTag());
    assertEquals(ObjectiveType.ATTACK_HOSTILE_FACTIONS.name(), restored.getId());
  }

  @Test
  @DisplayName("A follow owner objective targets an owner even without an explicit owner UUID")
  void testOwnerTargetWithoutExplicitOwnerUUID() {
    ObjectiveDataEntry followOwner = new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER);
    assertNull(followOwner.getTargetOwnerUUID());
    assertTrue(followOwner.hasOwnerTarget());

    ObjectiveDataEntry lookAtOwner = new ObjectiveDataEntry(ObjectiveType.LOOK_AT_OWNER);
    assertTrue(lookAtOwner.hasOwnerTarget());
  }

  @Test
  @DisplayName("An unset target owner UUID stays unset through a save/load round trip")
  void testUnsetTargetOwnerUUIDIsNotPersisted() {
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER);
    CompoundTag compoundTag = entry.createTag();

    assertFalse(compoundTag.contains(ObjectiveDataEntry.DATA_TARGET_OWNER_UUID_TAG));
    assertNull(new ObjectiveDataEntry(compoundTag).getTargetOwnerUUID());
  }

  @Test
  @DisplayName("Objectives without an owner target are unaffected by the owner fallback")
  void testNonOwnerObjectivesHaveNoOwnerTarget() {
    assertFalse(new ObjectiveDataEntry(ObjectiveType.FOLLOW_PLAYER).hasOwnerTarget());
    assertFalse(new ObjectiveDataEntry(ObjectiveType.RANDOM_STROLL).hasOwnerTarget());
  }

  @Test
  @DisplayName("Follow tuning survives a save/load round trip")
  void testFollowTuningRoundTrip() {
    ObjectiveDataEntry entry =
        new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER)
            .setStopDistance(3.5F)
            .setStartDistance(48.0F)
            .setTeleportDistance(24.0F)
            .setFollowOffset(new Vec3(1.5D, 2.0D, -0.5D));

    ObjectiveDataEntry restored = new ObjectiveDataEntry(entry.createTag());
    assertEquals(3.5F, restored.getStopDistance());
    assertEquals(48.0F, restored.getStartDistance());
    assertEquals(24.0F, restored.getTeleportDistance());
    assertEquals(new Vec3(1.5D, 2.0D, -0.5D), restored.getFollowOffset());
  }

  @Test
  @DisplayName("Following has no distance limit and no follow offset by default")
  void testFollowDefaults() {
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER);

    assertEquals(0.0F, entry.getStartDistance());
    assertEquals(12.0F, entry.getTeleportDistance());
    assertEquals(Vec3.ZERO, entry.getFollowOffset());
    assertFalse(entry.createTag().contains(ObjectiveDataEntry.DATA_FOLLOW_OFFSET_TAG));
  }

  @Test
  @DisplayName("Negative distances are rejected instead of silently breaking the goal")
  void testNegativeValuesAreClamped() {
    ObjectiveDataEntry entry =
        new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER)
            .setStopDistance(-5.0F)
            .setTeleportDistance(-1.0F)
            .setProbability(2.5F);

    assertEquals(0.0F, entry.getStopDistance());
    assertEquals(0.0F, entry.getTeleportDistance());
    assertEquals(1.0F, entry.getProbability());
  }

  @Test
  @DisplayName("Values loaded from a tag are clamped like values set through the API")
  void testValuesFromTagAreClamped() {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putString(ObjectiveDataEntry.DATA_TYPE_TAG, ObjectiveType.FOLLOW_OWNER.name());
    compoundTag.putDouble(ObjectiveDataEntry.DATA_SPEED_MODIFIER_TAG, -1.0D);
    compoundTag.putFloat(ObjectiveDataEntry.DATA_START_DISTANCE_TAG, -3.0F);
    compoundTag.putFloat(ObjectiveDataEntry.DATA_STOP_DISTANCE_TAG, -5.0F);
    compoundTag.putFloat(ObjectiveDataEntry.DATA_TELEPORT_DISTANCE_TAG, -1.0F);
    compoundTag.putFloat(ObjectiveDataEntry.DATA_PROBABILITY_TAG, 2.5F);
    compoundTag.putInt(ObjectiveDataEntry.DATA_INTERVAL_TAG, 0);

    ObjectiveDataEntry entry = new ObjectiveDataEntry(compoundTag);

    assertEquals(0.0D, entry.getSpeedModifier());
    assertEquals(0.0F, entry.getStartDistance());
    assertEquals(0.0F, entry.getStopDistance());
    assertEquals(0.0F, entry.getTeleportDistance());
    assertEquals(1.0F, entry.getProbability());
    assertEquals(1, entry.getInterval());
  }

  @Test
  @DisplayName("A loaded objective no longer reads from the tag it was loaded from")
  void testLoadedValuesAreIndependentOfTheTag() {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putString(ObjectiveDataEntry.DATA_TYPE_TAG, ObjectiveType.RANDOM_STROLL.name());
    compoundTag.putBoolean(ObjectiveDataEntry.DATA_CAN_DEAL_WITH_DOORS_TAG, true);

    ObjectiveDataEntry entry = new ObjectiveDataEntry(compoundTag);
    compoundTag.putBoolean(ObjectiveDataEntry.DATA_CAN_DEAL_WITH_DOORS_TAG, false);

    assertTrue(entry.getCanDealWithDoors().getAsBoolean());
  }

  @Test
  @DisplayName("Built-in objectives always use the priority of their type")
  void testBuiltInObjectivesIgnoreStoredPriority() {
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.MOVE_BACK_TO_HOME);
    entry.setPriority(3);

    assertEquals(ObjectiveType.MOVE_BACK_TO_HOME.getDefaultPriority(), entry.getPriority());
    assertEquals(
        ObjectiveType.MOVE_BACK_TO_HOME.getDefaultPriority(),
        new ObjectiveDataEntry(entry.createTag()).getPriority());
  }

  @Test
  @DisplayName("Custom objectives keep the priority contributed by their mod")
  void testCustomObjectivePriorityIsClamped() {
    ObjectiveDataEntry entry =
        new ObjectiveDataEntry(Identifier.fromNamespaceAndPath("example", "objective"))
            .setPriority(-3);

    assertEquals(0, entry.getPriority());
  }

  @Test
  @DisplayName("Tempt options survive a save/load round trip")
  void testTemptOptionsRoundTrip() {
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.FOLLOW_ITEM);
    entry.setTargetItemTag("#minecraft:flowers");
    entry.setCanScare(true).setOnlyWithoutOwner(true);

    ObjectiveDataEntry restored = new ObjectiveDataEntry(entry.createTag());
    assertEquals("#minecraft:flowers", restored.getTargetItemTag());
    assertTrue(restored.getCanScare());
    assertTrue(restored.getOnlyWithoutOwner());
  }

  @Test
  @DisplayName("The looked at item survives a save/load round trip")
  void testLookAtItemRoundTrip() {
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.LOOK_AT_ITEM);
    entry.setTargetItemTag("minecraft:apple");

    ObjectiveDataEntry restored = new ObjectiveDataEntry(entry.createTag());
    assertEquals(ObjectiveType.LOOK_AT_ITEM, restored.getType());
    assertEquals("minecraft:apple", restored.getTargetItemTag());
    assertEquals(ObjectiveType.LOOK_AT_ITEM.getDefaultPriority(), restored.getPriority());
    assertFalse(restored.hasTravelObjective());
  }

  @Test
  @DisplayName("A changed value drops the cached goal so it gets rebuilt")
  void testSetterInvalidatesRegistration() {
    ObjectiveDataEntry entry = new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER);
    entry.setRegistered(true);

    entry.setStopDistance(4.0F);
    assertFalse(entry.isRegistered());
  }

  @Test
  @DisplayName("An objective type from a currently absent mod survives a save/load round trip")
  void testUnresolvedTypeSurvivesRoundTrip() {
    CompoundTag storedTag = new CompoundTag();
    storedTag.putString(ObjectiveDataEntry.DATA_TYPE_TAG, "othermod:hover_follow");
    storedTag.putInt(ObjectiveDataEntry.DATA_PRIORITY_TAG, 7);

    ObjectiveDataEntry entry = new ObjectiveDataEntry(storedTag);
    assertEquals(ObjectiveType.NONE, entry.getType());
    assertTrue(entry.hasUnresolvedType());
    assertEquals("othermod:hover_follow", entry.getTypeName());
    assertEquals("othermod:hover_follow", entry.getId());

    CompoundTag savedTag = entry.createTag();
    assertEquals(
        "othermod:hover_follow", savedTag.getString(ObjectiveDataEntry.DATA_TYPE_TAG).orElse(""));
    assertEquals(7, savedTag.getInt(ObjectiveDataEntry.DATA_PRIORITY_TAG).orElse(0));
  }

  @Test
  @DisplayName("An explicit NONE type is not mistaken for an unresolved type")
  void testExplicitNoneTypeIsNotUnresolved() {
    CompoundTag storedTag = new CompoundTag();
    storedTag.putString(ObjectiveDataEntry.DATA_TYPE_TAG, ObjectiveType.NONE.name());

    ObjectiveDataEntry entry = new ObjectiveDataEntry(storedTag);
    assertEquals(ObjectiveType.NONE, entry.getType());
    assertFalse(entry.hasUnresolvedType());
  }

  @Test
  @DisplayName("A lower case type name still resolves to its objective type")
  void testTypeLookupIsCaseInsensitive() {
    CompoundTag storedTag = new CompoundTag();
    storedTag.putString(ObjectiveDataEntry.DATA_TYPE_TAG, "follow_owner");

    ObjectiveDataEntry entry = new ObjectiveDataEntry(storedTag);
    assertEquals(ObjectiveType.FOLLOW_OWNER, entry.getType());
    assertFalse(entry.hasUnresolvedType());
  }
}
