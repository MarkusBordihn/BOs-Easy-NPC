/*
 * Copyright 2023 Markus Bordihn
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

public enum ObjectiveType {
  // @formatter:off
  NONE,
  AVOID_ENTITY,
  ATTACK_ANIMAL,
  ATTACK_PLAYER,
  ATTACK_MONSTER,
  ATTACK_ENTITY_BY_UUID,
  ATTACK_MOB_WITHOUT_CREEPER,
  ATTACK_MOB,
  ATTACK_VILLAGER,
  CUSTOM,
  FOLLOW_ENTITY_BY_UUID,
  FOLLOW_OWNER,
  FOLLOW_PLAYER,
  MELEE_ATTACK,
  MOVE_BACK_TO_VILLAGE,
  MOVE_THROUGH_VILLAGE,
  NEAREST_ATTACKABLE_TARGET,
  RANDOM_STROLL,
  RANDOM_STROLL_IN_VILLAGE,
  RANDOM_SWIMMING,
  FLOAT,
  LOOK_AT_PLAYER,
  LOOK_AT_MOB,
  RESET_LOOK_AT,
  OPEN_DOOR,
  CLOSE_DOOR,
  ZOMBIE_ATTACK,
  WATER_AVOIDING_RANDOM_STROLL;

  // @formatter:on

  public static ObjectiveType get(String objectiveType) {
    if (objectiveType == null || objectiveType.isEmpty()) {
      return ObjectiveType.NONE;
    }
    try {
      return ObjectiveType.valueOf(objectiveType);
    } catch (IllegalArgumentException e) {
      return ObjectiveType.NONE;
    }
  }

  public String getObjectiveName() {
    return this.name().toLowerCase();
  }
}
