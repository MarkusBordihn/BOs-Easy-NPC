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

package de.markusbordihn.easynpc.data.display;

import de.markusbordihn.easynpc.data.type.ValueType;
import java.util.Locale;

public enum DisplayAttributeType {
  NONE(ValueType.STRING),
  VISIBLE(ValueType.BOOLEAN),
  VISIBLE_AT_DAY(ValueType.BOOLEAN),
  VISIBLE_AT_NIGHT(ValueType.BOOLEAN),
  VISIBLE_IN_CREATIVE(ValueType.BOOLEAN),
  VISIBLE_IN_SPECTATOR(ValueType.BOOLEAN),
  VISIBLE_IN_STANDARD(ValueType.BOOLEAN),
  VISIBLE_TO_OWNER(ValueType.BOOLEAN),
  VISIBLE_TO_TEAM(ValueType.BOOLEAN),
  INTERACTION_WHEN_INVISIBLE(ValueType.BOOLEAN),
  LIGHT_LEVEL(ValueType.INTEGER),
  NAME_VISIBILITY(ValueType.STRING);

  private final ValueType valueType;

  DisplayAttributeType(ValueType valueType) {
    this.valueType = valueType;
  }

  public static DisplayAttributeType get(String displayAttributeType) {
    if (displayAttributeType == null || displayAttributeType.isEmpty()) {
      return DisplayAttributeType.NONE;
    }
    try {
      return DisplayAttributeType.valueOf(displayAttributeType);
    } catch (IllegalArgumentException e) {
      return DisplayAttributeType.NONE;
    }
  }

  public String getAttributeName() {
    return this.name().toLowerCase(Locale.ROOT);
  }

  public ValueType getValueType() {
    return this.valueType;
  }

  public boolean isValidValue(String value) {
    return this.valueType.isValidValue(value);
  }

  public Object parseValue(String value) {
    return this.valueType.parseValue(value);
  }
}
