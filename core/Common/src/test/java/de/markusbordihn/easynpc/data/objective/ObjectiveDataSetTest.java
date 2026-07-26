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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ObjectiveDataSetTest {

  private static ObjectiveDataEntry unresolvedEntry(String typeName) {
    CompoundTag storedTag = new CompoundTag();
    storedTag.putString(ObjectiveDataEntry.DATA_TYPE_TAG, typeName);
    return new ObjectiveDataEntry(storedTag);
  }

  @Test
  @DisplayName("Objectives from a currently absent mod are kept instead of being dropped on save")
  void testUnresolvedObjectivesSurviveSaveAndLoad() {
    ObjectiveDataSet objectiveDataSet = new ObjectiveDataSet();
    objectiveDataSet.addObjective(new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER));
    objectiveDataSet.addObjective(unresolvedEntry("othermod:hover_follow"));

    ObjectiveDataSet restored = new ObjectiveDataSet(objectiveDataSet.createTag());
    Set<ObjectiveDataEntry> objectives = restored.getObjectives();

    assertEquals(2, objectives.size());
    assertTrue(
        objectives.stream()
            .anyMatch(
                objective ->
                    objective.hasUnresolvedType()
                        && "othermod:hover_follow".equals(objective.getTypeName())));
  }

  @Test
  @DisplayName("An unresolved objective stays inert and is not returned by regular lookups")
  void testUnresolvedObjectiveIsNotReturnedByLookup() {
    ObjectiveDataSet objectiveDataSet = new ObjectiveDataSet();
    objectiveDataSet.addObjective(unresolvedEntry("othermod:hover_follow"));

    assertFalse(objectiveDataSet.hasObjective("othermod:hover_follow"));
    assertFalse(objectiveDataSet.hasObjective(ObjectiveType.NONE));
  }

  @Test
  @DisplayName("A follow owner objective without an owner UUID still registers owner interest")
  void testOwnerTargetFlagWithoutExplicitOwnerUUID() {
    ObjectiveDataSet objectiveDataSet = new ObjectiveDataSet();
    objectiveDataSet.addObjective(new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER));

    assertTrue(objectiveDataSet.hasOwnerTarget());
  }

  @Test
  @DisplayName("Custom objectives of other mods survive a save/load round trip")
  void testCustomObjectiveRoundTrip() {
    ResourceLocation customObjectiveId = new ResourceLocation("othermod", "hover_follow");
    ObjectiveDataSet objectiveDataSet = new ObjectiveDataSet();
    objectiveDataSet.addObjective(new ObjectiveDataEntry(customObjectiveId).setPriority(7));

    ObjectiveDataEntry restored =
        new ObjectiveDataSet(objectiveDataSet.createTag())
            .getObjective(customObjectiveId.toString());

    assertNotNull(restored);
    assertEquals(ObjectiveType.CUSTOM, restored.getType());
    assertEquals(customObjectiveId, restored.getCustomObjectiveId());
    assertEquals(7, restored.getPriority());
  }

  @Test
  @DisplayName("A custom objective without its mod stays retried instead of written off")
  void testUnregisteredCustomObjectiveAwaitsRegistration() {
    ObjectiveDataEntry entry =
        new ObjectiveDataEntry(new ResourceLocation("othermod", "hover_follow"));

    assertTrue(entry.isAwaitingRegistration());
    assertFalse(new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER).isAwaitingRegistration());
  }

  @Test
  @DisplayName("Two custom objectives of the same NPC do not overwrite each other")
  void testMultipleCustomObjectivesCoexist() {
    ObjectiveDataSet objectiveDataSet = new ObjectiveDataSet();
    objectiveDataSet.addObjective(
        new ObjectiveDataEntry(new ResourceLocation("othermod", "hover_follow")));
    objectiveDataSet.addObjective(
        new ObjectiveDataEntry(new ResourceLocation("othermod", "guard_chest")));

    assertEquals(2, objectiveDataSet.getObjectives().size());
  }

  @Test
  @DisplayName("An objective without a type is still dropped")
  void testEmptyObjectiveIsDropped() {
    ObjectiveDataSet objectiveDataSet = new ObjectiveDataSet();
    objectiveDataSet.addObjective(new ObjectiveDataEntry());

    assertTrue(objectiveDataSet.getObjectives().isEmpty());
  }
}
