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

import java.util.EnumSet;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ObjectiveGroupTest {

  private static void assertGroupMatches(
      Set<ObjectiveType> objectiveGroup,
      ObjectiveType objectiveType,
      boolean hasTarget,
      String targetName) {
    assertEquals(
        objectiveGroup.contains(objectiveType),
        hasTarget,
        objectiveType + " must agree with the " + targetName + " target group");
  }

  @Test
  @DisplayName("Owner target types are follow owner and look at owner")
  void testOwnerTargetTypes() {
    assertEquals(
        EnumSet.of(ObjectiveType.FOLLOW_OWNER, ObjectiveType.LOOK_AT_OWNER),
        ObjectiveGroup.OWNER_TARGET);
  }

  @Test
  @DisplayName("Player target types are follow player and attack player by name")
  void testPlayerTargetTypes() {
    assertEquals(
        EnumSet.of(ObjectiveType.FOLLOW_PLAYER, ObjectiveType.ATTACK_PLAYER_BY_NAME),
        ObjectiveGroup.PLAYER_TARGET);
  }

  @Test
  @DisplayName("Entity target types are the three types resolving an entity by UUID")
  void testEntityTargetTypes() {
    assertEquals(
        EnumSet.of(
            ObjectiveType.FOLLOW_ENTITY_BY_UUID,
            ObjectiveType.LOOK_AT_ENTITY_BY_UUID,
            ObjectiveType.ATTACK_ENTITY_BY_UUID),
        ObjectiveGroup.ENTITY_TARGET);
  }

  @Test
  @DisplayName("Attack types are the five weapon based attack objectives")
  void testAttackTypes() {
    assertEquals(
        EnumSet.of(
            ObjectiveType.MELEE_ATTACK,
            ObjectiveType.ZOMBIE_ATTACK,
            ObjectiveType.CROSSBOW_ATTACK,
            ObjectiveType.BOW_ATTACK,
            ObjectiveType.GUN_ATTACK),
        ObjectiveGroup.ATTACK_TYPE);
  }

  @Test
  @DisplayName("Follow types are the three entity based follow objectives")
  void testFollowTypes() {
    assertEquals(
        EnumSet.of(
            ObjectiveType.FOLLOW_ENTITY_BY_UUID,
            ObjectiveType.FOLLOW_OWNER,
            ObjectiveType.FOLLOW_PLAYER),
        ObjectiveGroup.FOLLOW);
  }

  @Test
  @DisplayName("Target group membership matches the target checks of an objective entry")
  void testTargetGroupsMatchObjectiveEntryChecks() {
    for (ObjectiveType objectiveType : ObjectiveType.values()) {
      ObjectiveDataEntry objectiveDataEntry = new ObjectiveDataEntry(objectiveType);
      objectiveDataEntry.setTargetPlayerName("TestPlayer");
      objectiveDataEntry.setTargetEntityUUID(UUID.randomUUID());

      assertGroupMatches(
          ObjectiveGroup.OWNER_TARGET, objectiveType, objectiveDataEntry.hasOwnerTarget(), "owner");
      assertGroupMatches(
          ObjectiveGroup.PLAYER_TARGET,
          objectiveType,
          objectiveDataEntry.hasPlayerTarget(),
          "player");
      assertGroupMatches(
          ObjectiveGroup.ENTITY_TARGET,
          objectiveType,
          objectiveDataEntry.hasEntityTarget(),
          "entity");
    }
  }

  @Test
  @DisplayName("Target checks stay false while the required target value is missing")
  void testTargetChecksRequireTheTargetValue() {
    ObjectiveDataEntry followPlayer = new ObjectiveDataEntry(ObjectiveType.FOLLOW_PLAYER);
    assertFalse(followPlayer.hasPlayerTarget(), "Follow player without a name has no target");

    ObjectiveDataEntry followEntity = new ObjectiveDataEntry(ObjectiveType.FOLLOW_ENTITY_BY_UUID);
    assertFalse(followEntity.hasEntityTarget(), "Follow entity without a UUID has no target");
  }
}
