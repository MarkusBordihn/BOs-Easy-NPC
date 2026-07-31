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

package de.markusbordihn.easynpc.data.state;

import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionSubTypeEntry;
import de.markusbordihn.easynpc.utils.EnumUtils;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;

public enum StateValueType implements ConditionSubTypeEntry {
  NUMBER,
  FLAG,
  TEXT;

  public static StateValueType get(String stateValueType) {
    return EnumUtils.get(StateValueType.class, stateValueType, NUMBER);
  }

  public static StateValueType find(String stateValueType) {
    return EnumUtils.getIgnoreCase(StateValueType.class, stateValueType, null);
  }

  public static StateValueType of(StateEntry stateEntry) {
    return stateEntry != null && stateEntry.isText() ? TEXT : NUMBER;
  }

  public static StateValueType detect(String value) {
    try {
      Integer.parseInt(value != null ? value.trim() : "");
      return NUMBER;
    } catch (NumberFormatException ignored) {
      return TEXT;
    }
  }

  private static int parseNumber(String value) {
    try {
      return Integer.parseInt(value.trim());
    } catch (NumberFormatException ignored) {
      return 0;
    }
  }

  private static boolean parseFlag(String value) {
    String trimmedValue = value != null ? value.trim() : "";
    return trimmedValue.equalsIgnoreCase("true") || trimmedValue.equals("1");
  }

  public String getCommandName() {
    return this.name().toLowerCase(Locale.ROOT);
  }

  public StateEntry parse(String value) {
    return switch (this) {
      case NUMBER -> StateEntry.of(parseNumber(value));
      case FLAG -> StateEntry.of(parseFlag(value));
      case TEXT -> StateEntry.of(value);
    };
  }

  public String format(StateEntry stateEntry) {
    if (stateEntry == null) {
      return this == TEXT ? "" : this.format(StateEntry.EMPTY);
    }

    return switch (this) {
      case NUMBER -> String.valueOf(stateEntry.asNumber());
      case FLAG -> String.valueOf(stateEntry.asFlag());
      case TEXT -> stateEntry.asText();
    };
  }

  public boolean supports(ConditionOperationType operationType) {
    return this == NUMBER || operationType.isEqualityOperation();
  }

  public Set<ConditionOperationType> supportedOperationTypes() {
    return Arrays.stream(ConditionOperationType.values())
        .filter(operationType -> operationType != ConditionOperationType.NONE)
        .filter(this::supports)
        .collect(Collectors.toCollection(LinkedHashSet::new));
  }
}
