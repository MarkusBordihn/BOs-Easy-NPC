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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveGoalFactory;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Collections;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Map;
import java.util.Set;
import net.minecraft.world.entity.ai.goal.Goal;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class BuiltInObjectiveFactories {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final Map<ObjectiveType, ObjectiveGoalFactory> goalFactories =
      new EnumMap<>(ObjectiveType.class);
  private static final Set<ObjectiveType> targetObjectiveTypes =
      EnumSet.noneOf(ObjectiveType.class);
  private static final Set<ObjectiveType> UNSUPPORTED_TYPES =
      Set.of(ObjectiveType.NONE, ObjectiveType.CUSTOM);

  static {
    AttributeObjectiveFactories.registerFactories();
    BasicObjectiveFactories.registerFactories();
    FollowObjectiveFactories.registerFactories();
    AttackObjectiveFactories.registerFactories();
    TargetObjectiveFactories.registerFactories();
    FleeObjectiveFactories.registerFactories();
    LookObjectiveFactories.registerFactories();
    verifyCompleteness();
  }

  private BuiltInObjectiveFactories() {}

  public static ObjectiveGoalFactory get(ObjectiveType objectiveType) {
    return objectiveType != null ? goalFactories.get(objectiveType) : null;
  }

  public static boolean isTargetObjective(ObjectiveType objectiveType) {
    return targetObjectiveTypes.contains(objectiveType);
  }

  public static Set<ObjectiveType> getTargetObjectiveTypes() {
    return Collections.unmodifiableSet(targetObjectiveTypes);
  }

  public static Set<ObjectiveType> getRegisteredTypes() {
    return Collections.unmodifiableSet(goalFactories.keySet());
  }

  static void register(ObjectiveType objectiveType, ObjectiveGoalFactory objectiveGoalFactory) {
    ObjectiveGoalFactory previousFactory =
        goalFactories.putIfAbsent(objectiveType, objectiveGoalFactory);
    if (previousFactory != null) {
      log.error("Objective {} is already registered by {}", objectiveType, previousFactory);
    }
  }

  static void register(ObjectiveType objectiveType, ObjectiveGoalCreator objectiveGoalCreator) {
    register(objectiveType, goalFactory(objectiveGoalCreator));
  }

  static void registerTarget(
      ObjectiveType objectiveType, ObjectiveGoalFactory objectiveGoalFactory) {
    register(objectiveType, objectiveGoalFactory);
    targetObjectiveTypes.add(objectiveType);
  }

  static void registerTarget(
      ObjectiveType objectiveType, ObjectiveGoalCreator objectiveGoalCreator) {
    registerTarget(objectiveType, targetFactory(objectiveGoalCreator));
  }

  static ObjectiveGoalFactory requiresPathfinderMob(ObjectiveGoalFactory objectiveGoalFactory) {
    return new PathfinderMobObjectiveFactory(objectiveGoalFactory);
  }

  static ObjectiveGoalFactory requiresPathfinderMob(ObjectiveGoalCreator objectiveGoalCreator) {
    return requiresPathfinderMob(goalFactory(objectiveGoalCreator));
  }

  static ObjectiveGoalFactory goalFactory(ObjectiveGoalCreator objectiveGoalCreator) {
    return new ObjectiveGoalFactory() {
      @Override
      public Goal createGoal(ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC) {
        return objectiveGoalCreator.create(objectiveDataEntry, easyNPC);
      }
    };
  }

  private static void verifyCompleteness() {
    Set<ObjectiveType> missingTypes = EnumSet.allOf(ObjectiveType.class);
    missingTypes.removeAll(UNSUPPORTED_TYPES);
    missingTypes.removeAll(goalFactories.keySet());
    if (!missingTypes.isEmpty()) {
      log.error("Objective types without a registered factory: {}", missingTypes);
      throw new IllegalStateException(
          "Objective types without a registered factory: " + missingTypes);
    }
  }

  static ObjectiveGoalFactory targetFactory(ObjectiveGoalCreator objectiveGoalCreator) {
    return new ObjectiveGoalFactory() {
      @Override
      public Goal createTarget(ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC) {
        return objectiveGoalCreator.create(objectiveDataEntry, easyNPC);
      }
    };
  }
}
