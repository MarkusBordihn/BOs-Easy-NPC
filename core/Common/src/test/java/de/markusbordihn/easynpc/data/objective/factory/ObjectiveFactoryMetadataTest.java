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

package de.markusbordihn.easynpc.data.objective.factory;

import static org.junit.jupiter.api.Assertions.assertEquals;

import de.markusbordihn.easynpc.data.objective.ObjectiveGoalFactory;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import java.util.EnumSet;
import java.util.Set;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ObjectiveFactoryMetadataTest {

  private static final Set<ObjectiveType> EXPECTED_TARGET_OBJECTIVES =
      EnumSet.of(
          ObjectiveType.ATTACK_ANIMAL,
          ObjectiveType.ATTACK_ENTITY_BY_TAG,
          ObjectiveType.ATTACK_ENTITY_BY_TEAM,
          ObjectiveType.ATTACK_ENTITY_BY_UUID,
          ObjectiveType.ATTACK_HOSTILE_FACTIONS,
          ObjectiveType.ATTACK_MOB,
          ObjectiveType.ATTACK_MOB_WITHOUT_CREEPER,
          ObjectiveType.ATTACK_MONSTER,
          ObjectiveType.ATTACK_PLAYER,
          ObjectiveType.ATTACK_PLAYER_BY_NAME,
          ObjectiveType.ATTACK_PLAYER_WITHOUT_OWNER,
          ObjectiveType.ATTACK_VILLAGER,
          ObjectiveType.FACTION_HURT_BY_TARGET,
          ObjectiveType.HURT_BY_TARGET,
          ObjectiveType.OWNER_HURT_BY_TARGET);

  private static final Set<ObjectiveType> EXPECTED_PATHFINDER_MOB_OBJECTIVES =
      EnumSet.of(
          ObjectiveType.AVOID_SUN,
          ObjectiveType.BOW_ATTACK,
          ObjectiveType.CROSSBOW_ATTACK,
          ObjectiveType.FLEE_CREEPER,
          ObjectiveType.FLEE_MOB,
          ObjectiveType.FLEE_MONSTER,
          ObjectiveType.FLEE_PLAYER,
          ObjectiveType.FLEE_SUN,
          ObjectiveType.FLEE_VILLAGER,
          ObjectiveType.FOLLOW_ITEM,
          ObjectiveType.GUN_ATTACK,
          ObjectiveType.HURT_BY_TARGET,
          ObjectiveType.MELEE_ATTACK,
          ObjectiveType.MOVE_BACK_TO_VILLAGE,
          ObjectiveType.MOVE_THROUGH_VILLAGE,
          ObjectiveType.RANDOM_STROLL_IN_VILLAGE,
          ObjectiveType.RANDOM_SWIMMING,
          ObjectiveType.ZOMBIE_ATTACK);

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @Test
  @DisplayName("Target objectives are registered on the target selector")
  void testTargetObjectiveClassification() {
    assertEquals(EXPECTED_TARGET_OBJECTIVES, BuiltInObjectiveFactories.getTargetObjectiveTypes());
  }

  @Test
  @DisplayName("Objectives needing a pathfinder mob are wrapped in the compatibility check")
  void testPathfinderMobClassification() {
    Set<ObjectiveType> pathfinderMobObjectives = EnumSet.noneOf(ObjectiveType.class);
    for (ObjectiveType objectiveType : BuiltInObjectiveFactories.getRegisteredTypes()) {
      ObjectiveGoalFactory objectiveGoalFactory = BuiltInObjectiveFactories.get(objectiveType);
      if (objectiveGoalFactory instanceof PathfinderMobObjectiveFactory) {
        pathfinderMobObjectives.add(objectiveType);
      }
    }

    assertEquals(EXPECTED_PATHFINDER_MOB_OBJECTIVES, pathfinderMobObjectives);
  }
}
