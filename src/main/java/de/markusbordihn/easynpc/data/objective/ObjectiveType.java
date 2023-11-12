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
  STANDARD,
  AVOID_ENTITY,
  RANDOM_STROLL,
  WATER_AVOIDING_RANDOM_STROLL,
  NEAREST_ATTACKABLE_TARGET,
  MOVE_THROUGH_VILLAGE,
  FOLLOW_OWNER,
  FOLLOW_PLAYER,
  FOLLOW_ENTITY_BY_UUID,
  CUSTOM,
  NONE;
  // @formatter:on

  private boolean isGoalSelector = false;
  private boolean isTargetSelector = false;

  ObjectiveType() {
    this.isGoalSelector = true;
    this.isTargetSelector = false;
  }

  ObjectiveType(boolean isTargetSelector) {
    this.isGoalSelector = false;
    this.isTargetSelector = isTargetSelector;
  }

  ObjectiveType(boolean isGoalSelector, boolean isTargetSelector) {
    this.isGoalSelector = isGoalSelector;
    this.isTargetSelector = isTargetSelector;
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

  public boolean isGoalSelector() {
    return this.isGoalSelector;
  }

  public boolean isTargetSelector() {
    return this.isTargetSelector;
  }
}
