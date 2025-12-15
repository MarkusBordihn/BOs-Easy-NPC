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

public enum ConditionOperationType {
  NONE,
  EQUALS,
  NOT_EQUALS,
  GREATER_THAN,
  GREATER_THAN_OR_EQUALS,
  LESS_THAN,
  LESS_THAN_OR_EQUALS;

  public static ConditionOperationType get(String operationType) {
    if (operationType == null || operationType.isEmpty()) {
      return ConditionOperationType.NONE;
    }
    try {
      return ConditionOperationType.valueOf(operationType);
    } catch (IllegalArgumentException e) {
      return ConditionOperationType.NONE;
    }
  }

  public String getSymbol() {
    return switch (this) {
      case EQUALS -> "==";
      case NOT_EQUALS -> "!=";
      case GREATER_THAN -> ">";
      case GREATER_THAN_OR_EQUALS -> ">=";
      case LESS_THAN -> "<";
      case LESS_THAN_OR_EQUALS -> "<=";
      default -> "";
    };
  }

  public boolean evaluate(int value1, int value2) {
    return switch (this) {
      case EQUALS -> value1 == value2;
      case NOT_EQUALS -> value1 != value2;
      case GREATER_THAN -> value1 > value2;
      case GREATER_THAN_OR_EQUALS -> value1 >= value2;
      case LESS_THAN -> value1 < value2;
      case LESS_THAN_OR_EQUALS -> value1 <= value2;
      default -> false;
    };
  }
}
