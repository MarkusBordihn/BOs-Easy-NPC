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
}
