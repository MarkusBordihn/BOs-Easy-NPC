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

package de.markusbordihn.easynpc.condition;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.RelationshipType;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class RelationshipConditionTest {

  private static final String OWN_FACTION = "guards";
  private static final String OTHER_FACTION = "bandits";

  @Test
  @DisplayName("Owner matches the owner and nobody else")
  void matchesOwner() {
    assertTrue(RelationshipCondition.matches(RelationshipType.OWNER, true, null, null, false));
    assertFalse(RelationshipCondition.matches(RelationshipType.OWNER, false, null, null, false));
  }

  @Test
  @DisplayName("Not owner also matches an NPC without any owner")
  void matchesNotOwner() {
    assertTrue(RelationshipCondition.matches(RelationshipType.NOT_OWNER, false, null, null, false));
    assertFalse(RelationshipCondition.matches(RelationshipType.NOT_OWNER, true, null, null, false));
  }

  @Test
  @DisplayName("Same faction needs a faction on both sides")
  void matchesSameFaction() {
    assertTrue(
        RelationshipCondition.matches(
            RelationshipType.SAME_FACTION, false, OWN_FACTION, OWN_FACTION, false));
    assertFalse(
        RelationshipCondition.matches(
            RelationshipType.SAME_FACTION, false, OWN_FACTION, OTHER_FACTION, false));
    assertFalse(
        RelationshipCondition.matches(
            RelationshipType.SAME_FACTION, false, OWN_FACTION, null, false));
    assertFalse(
        RelationshipCondition.matches(RelationshipType.SAME_FACTION, false, "", null, false));
  }

  @Test
  @DisplayName("Not same faction matches a player without a faction")
  void matchesNotSameFaction() {
    assertTrue(
        RelationshipCondition.matches(
            RelationshipType.NOT_SAME_FACTION, false, OWN_FACTION, null, false));
    assertTrue(
        RelationshipCondition.matches(
            RelationshipType.NOT_SAME_FACTION, false, OWN_FACTION, OTHER_FACTION, false));
    assertFalse(
        RelationshipCondition.matches(
            RelationshipType.NOT_SAME_FACTION, false, OWN_FACTION, OWN_FACTION, false));
  }

  @Test
  @DisplayName("Friendly and hostile faction both need a faction on the NPC")
  void matchesFactionHostility() {
    assertTrue(
        RelationshipCondition.matches(
            RelationshipType.FRIENDLY_FACTION, false, OWN_FACTION, OTHER_FACTION, false));
    assertFalse(
        RelationshipCondition.matches(
            RelationshipType.FRIENDLY_FACTION, false, OWN_FACTION, OTHER_FACTION, true));
    assertFalse(
        RelationshipCondition.matches(RelationshipType.FRIENDLY_FACTION, false, "", null, false));

    assertTrue(
        RelationshipCondition.matches(
            RelationshipType.HOSTILE_FACTION, false, OWN_FACTION, OTHER_FACTION, true));
    assertFalse(
        RelationshipCondition.matches(
            RelationshipType.HOSTILE_FACTION, false, OWN_FACTION, OTHER_FACTION, false));
    assertFalse(
        RelationshipCondition.matches(RelationshipType.HOSTILE_FACTION, false, "", null, true));
  }

  @Test
  @DisplayName("An entry without a player or without an NPC is never met")
  void evaluateWithoutContext() {
    ConditionDataEntry entry =
        new ConditionDataEntry(ConditionType.RELATIONSHIP).withSubType(RelationshipType.OWNER);
    assertFalse(RelationshipCondition.evaluate(entry, null, null));
    assertFalse(RelationshipCondition.evaluate(null, null, null));
  }

  @Test
  @DisplayName("The client answers faction hostility with true, because it cannot know it")
  void evaluateOnClientFailsOpenForHostility() {
    assertTrue(
        RelationshipCondition.evaluateOnClient(
            new ConditionDataEntry(ConditionType.RELATIONSHIP)
                .withSubType(RelationshipType.FRIENDLY_FACTION),
            null,
            null));
    assertTrue(
        RelationshipCondition.evaluateOnClient(
            new ConditionDataEntry(ConditionType.RELATIONSHIP)
                .withSubType(RelationshipType.HOSTILE_FACTION),
            null,
            null));
  }

  @Test
  @DisplayName("The relationship condition needs a valid sub type")
  void isValidNeedsSubType() {
    assertFalse(new ConditionDataEntry(ConditionType.RELATIONSHIP).isValid());
    assertTrue(
        new ConditionDataEntry(ConditionType.RELATIONSHIP)
            .withSubType(RelationshipType.OWNER)
            .isValid());
  }

  @Test
  @DisplayName("A relationship condition survives a tag round trip")
  void tagRoundTrip() {
    ConditionDataEntry entry =
        new ConditionDataEntry(ConditionType.RELATIONSHIP)
            .withSubType(RelationshipType.SAME_FACTION)
            .withName(OWN_FACTION);
    ConditionDataEntry restored = new ConditionDataEntry(entry.createTag());

    assertTrue(restored.isValid());
    assertEquals(RelationshipType.SAME_FACTION, restored.subType());
    assertEquals(OWN_FACTION, restored.name());
  }
}
