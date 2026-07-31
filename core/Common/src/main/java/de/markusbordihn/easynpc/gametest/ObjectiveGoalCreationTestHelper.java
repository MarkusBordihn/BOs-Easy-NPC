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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.objective.ObjectiveUtils;
import de.markusbordihn.easynpc.data.objective.factory.BuiltInObjectiveFactories;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import java.util.EnumSet;
import java.util.Set;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.phys.Vec3;

public class ObjectiveGoalCreationTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);

  private static final Set<ObjectiveType> OBJECTIVES_WITHOUT_DEFAULT_TARGET =
      EnumSet.of(
          ObjectiveType.NONE,
          ObjectiveType.CUSTOM,
          ObjectiveType.FOLLOW_OWNER,
          ObjectiveType.FOLLOW_PLAYER,
          ObjectiveType.FOLLOW_ENTITY_BY_UUID,
          ObjectiveType.FOLLOW_ITEM,
          ObjectiveType.LOOK_AT_ENTITY_BY_UUID,
          ObjectiveType.LOOK_AT_OWNER,
          ObjectiveType.LOOK_AT_ITEM,
          ObjectiveType.ATTACK_ENTITY_BY_TAG,
          ObjectiveType.ATTACK_ENTITY_BY_TEAM,
          ObjectiveType.ATTACK_ENTITY_BY_UUID,
          ObjectiveType.ATTACK_PLAYER_BY_NAME);

  private ObjectiveGoalCreationTestHelper() {}

  public static void assertEveryObjectiveTypeCreatesItsGoal(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    for (ObjectiveType objectiveType : ObjectiveType.values()) {
      if (OBJECTIVES_WITHOUT_DEFAULT_TARGET.contains(objectiveType)) {
        continue;
      }

      ObjectiveDataEntry objectiveDataEntry = new ObjectiveDataEntry(objectiveType);
      if (BuiltInObjectiveFactories.isTargetObjective(objectiveType)) {
        Goal target = ObjectiveUtils.createObjectiveTarget(objectiveDataEntry, easyNPC);
        GameTestHelpers.assertNotNull(
            helper, "No target goal created for " + objectiveType + "!", target);
      } else {
        Goal goal = ObjectiveUtils.createObjectiveGoal(objectiveDataEntry, easyNPC);
        GameTestHelpers.assertNotNull(helper, "No goal created for " + objectiveType + "!", goal);
      }
    }
  }

  public static void assertEveryObjectiveTypeSurvivesTicking(
      GameTestHelper helper, EntityType<?> entityType) {
    for (ObjectiveType objectiveType : ObjectiveType.values()) {
      if (objectiveType == ObjectiveType.NONE || objectiveType == ObjectiveType.CUSTOM) {
        continue;
      }

      EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
      ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
      GameTestHelpers.assertNotNull(
          helper, "No objective data for " + entityType + "!", objectiveData);

      objectiveData.addOrUpdateCustomObjective(new ObjectiveDataEntry(objectiveType));
      tickEntity(easyNPC, 20);
      easyNPC.getEntity().discard();
    }
  }

  private static void tickEntity(EasyNPC<?> easyNPC, int ticks) {
    for (int i = 0; i < ticks; i++) {
      easyNPC.getEntity().tick();
    }
  }
}
