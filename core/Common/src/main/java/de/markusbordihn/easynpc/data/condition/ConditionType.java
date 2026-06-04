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

package de.markusbordihn.easynpc.data.condition;

public enum ConditionType {
  NONE(false, false, false),
  SCOREBOARD(true, true, true),
  EXECUTION_LIMIT(false, true, false),
  HAS_ITEM_IN_INVENTORY(true, false, false),
  HAS_ITEM_IN_MAIN_HAND(true, false, false),
  HAS_ITEM_IN_OFFHAND(true, false, false),
  ADVANCEMENT(true, false, false),
  EXPERIENCE_LEVEL(false, true, true),
  PLAYER_HEALTH(false, true, true),
  PLAYER_TAG(true, false, false),
  TEAM(true, false, false),
  GAMEMODE(true, false, false),
  FALLBACK(false, false, false),
  ;

  private final boolean requiresName;
  private final boolean requiresValue;
  private final boolean requiresOperation;

  ConditionType(boolean requiresName, boolean requiresValue, boolean requiresOperation) {
    this.requiresName = requiresName;
    this.requiresValue = requiresValue;
    this.requiresOperation = requiresOperation;
  }

  public static ConditionType get(String conditionType) {
    if (conditionType == null || conditionType.isEmpty()) {
      return ConditionType.NONE;
    }
    try {
      return ConditionType.valueOf(conditionType);
    } catch (IllegalArgumentException e) {
      return ConditionType.NONE;
    }
  }

  public boolean requiresName() {
    return requiresName;
  }

  public boolean requiresValue() {
    return requiresValue;
  }

  public boolean requiresOperation() {
    return requiresOperation;
  }
}
