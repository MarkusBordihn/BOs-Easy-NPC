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

package de.markusbordihn.easynpc.data.type;

import java.util.Locale;

public enum ValueType {
  BOOLEAN,
  DOUBLE,
  INTEGER,
  STRING;

  public static ValueType get(String valueType) {
    if (valueType == null || valueType.isEmpty()) {
      return ValueType.STRING;
    }
    try {
      return ValueType.valueOf(valueType.toUpperCase(Locale.ROOT));
    } catch (IllegalArgumentException e) {
      return ValueType.STRING;
    }
  }

  public String getTypeName() {
    return this.name().toLowerCase(Locale.ROOT);
  }

  public boolean isValidValue(String value) {
    if (value == null) {
      return false;
    }

    return switch (this) {
      case BOOLEAN -> "true".equalsIgnoreCase(value) || "false".equalsIgnoreCase(value);
      case INTEGER -> {
        try {
          Integer.parseInt(value);
          yield true;
        } catch (NumberFormatException e) {
          yield false;
        }
      }
      case DOUBLE -> {
        try {
          Double.parseDouble(value);
          yield true;
        } catch (NumberFormatException e) {
          yield false;
        }
      }
      case STRING -> true;
    };
  }

  public Object parseValue(String value) {
    if (!isValidValue(value)) {
      throw new IllegalArgumentException("Invalid value '" + value + "' for type " + this);
    }

    return switch (this) {
      case BOOLEAN -> Boolean.parseBoolean(value);
      case INTEGER -> Integer.parseInt(value);
      case DOUBLE -> Double.parseDouble(value);
      case STRING -> value;
    };
  }
}
