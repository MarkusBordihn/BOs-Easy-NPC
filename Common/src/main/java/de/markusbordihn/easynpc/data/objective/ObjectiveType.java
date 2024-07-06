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
  ATTACK_ANIMAL(2),
  ATTACK_MOB(2),
  ATTACK_MOB_WITHOUT_CREEPER(2),
  ATTACK_MONSTER(2),
  ATTACK_PLAYER(2),
  ATTACK_PLAYER_WITHOUT_OWNER(2),
  ATTACK_VILLAGER(2),
  AVOID_SUN(2),
  BOW_ATTACK(4),
  CLOSE_DOOR(8),
  CROSSBOW_ATTACK(4),
  FLEE_SUN(3),
  FLOAT(0),
  FOLLOW_ENTITY_BY_UUID(7),
  FOLLOW_ITEM(7),
  FOLLOW_OWNER(6),
  FOLLOW_PLAYER(7),
  GUN_ATTACK(4),
  LOOK_AT_ANIMAL(10, false),
  LOOK_AT_ENTITY_BY_UUID(9, false),
  LOOK_AT_ITEM(9, false),
  LOOK_AT_MOB(10, false),
  LOOK_AT_OWNER(9, false),
  LOOK_AT_PLAYER(9, false),
  LOOK_AT_RESET(9, false),
  LOOK_RANDOM_AROUND(10, false),
  MELEE_ATTACK(2),
  MOVE_BACK_TO_HOME(3),
  MOVE_BACK_TO_VILLAGE(3),
  MOVE_THROUGH_VILLAGE(5),
  NONE(false),
  OPEN_DOOR(8),
  PANIC(1),
  RANDOM_STROLL(5),
  RANDOM_STROLL_AROUND_HOME(2),
  RANDOM_STROLL_IN_VILLAGE(2),
  RANDOM_SWIMMING(4),
  WATER_AVOIDING_RANDOM_STROLL(5),
  ZOMBIE_ATTACK(2);

  // @formatter:on

  private final boolean hasTravelObjective;
  private final int defaultPriority;

  ObjectiveType(boolean hasTravelObjective) {
    this(5, hasTravelObjective);
  }

  ObjectiveType(int defaultPriority) {
    this(defaultPriority, true);
  }

  ObjectiveType(int defaultPriority, boolean hasTravelObjective) {
    this.defaultPriority = defaultPriority;
    this.hasTravelObjective = hasTravelObjective;
  }

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

  public boolean hasTravelObjective() {
    return this.hasTravelObjective;
  }

  public int getDefaultPriority() {
    return this.defaultPriority;
  }

  public String getObjectiveName() {
    return this.name().toLowerCase();
  }
}
