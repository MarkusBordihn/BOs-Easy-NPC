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

package de.markusbordihn.easynpc.data.type;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ValueTypeTest {

  @Test
  void testGet_validNames_caseInsensitive() {
    assertEquals(ValueType.BOOLEAN, ValueType.get("BOOLEAN"));
    assertEquals(ValueType.BOOLEAN, ValueType.get("boolean"));
    assertEquals(ValueType.INTEGER, ValueType.get("INTEGER"));
    assertEquals(ValueType.INTEGER, ValueType.get("integer"));
    assertEquals(ValueType.DOUBLE, ValueType.get("DOUBLE"));
    assertEquals(ValueType.DOUBLE, ValueType.get("double"));
    assertEquals(ValueType.STRING, ValueType.get("STRING"));
    assertEquals(ValueType.STRING, ValueType.get("string"));
  }

  @Test
  void testGet_null_returnsString() {
    assertEquals(ValueType.STRING, ValueType.get(null));
  }

  @Test
  void testGet_empty_returnsString() {
    assertEquals(ValueType.STRING, ValueType.get(""));
  }

  @ParameterizedTest
  @ValueSource(strings = {"INVALID", "INT", "BOOL", "FLOAT", "NUMBER"})
  void testGet_unknownType_returnsString(String input) {
    assertEquals(ValueType.STRING, ValueType.get(input));
  }

  @Test
  void testIsValidValue_boolean() {
    assertTrue(ValueType.BOOLEAN.isValidValue("true"));
    assertTrue(ValueType.BOOLEAN.isValidValue("false"));
    assertTrue(ValueType.BOOLEAN.isValidValue("TRUE"));
    assertTrue(ValueType.BOOLEAN.isValidValue("FALSE"));
    assertFalse(ValueType.BOOLEAN.isValidValue("yes"));
    assertFalse(ValueType.BOOLEAN.isValidValue("1"));
    assertFalse(ValueType.BOOLEAN.isValidValue(""));
    assertFalse(ValueType.BOOLEAN.isValidValue(null));
  }

  @Test
  void testIsValidValue_integer() {
    assertTrue(ValueType.INTEGER.isValidValue("0"));
    assertTrue(ValueType.INTEGER.isValidValue("42"));
    assertTrue(ValueType.INTEGER.isValidValue("-10"));
    assertTrue(ValueType.INTEGER.isValidValue("2147483647"));
    assertFalse(ValueType.INTEGER.isValidValue("3.14"));
    assertFalse(ValueType.INTEGER.isValidValue("abc"));
    assertFalse(ValueType.INTEGER.isValidValue(""));
    assertFalse(ValueType.INTEGER.isValidValue(null));
  }

  @Test
  void testIsValidValue_double() {
    assertTrue(ValueType.DOUBLE.isValidValue("3.14"));
    assertTrue(ValueType.DOUBLE.isValidValue("0.0"));
    assertTrue(ValueType.DOUBLE.isValidValue("-1.5"));
    assertTrue(ValueType.DOUBLE.isValidValue("42"));
    assertFalse(ValueType.DOUBLE.isValidValue("abc"));
    assertFalse(ValueType.DOUBLE.isValidValue(""));
    assertFalse(ValueType.DOUBLE.isValidValue(null));
  }

  @Test
  void testIsValidValue_string_alwaysValid() {
    assertTrue(ValueType.STRING.isValidValue("anything"));
    assertTrue(ValueType.STRING.isValidValue(""));
    assertTrue(ValueType.STRING.isValidValue("123"));
    assertFalse(ValueType.STRING.isValidValue(null));
  }

  @Test
  void testParseValue_boolean() {
    assertEquals(Boolean.TRUE, ValueType.BOOLEAN.parseValue("true"));
    assertEquals(Boolean.FALSE, ValueType.BOOLEAN.parseValue("false"));
  }

  @Test
  void testParseValue_integer() {
    assertEquals(42, ValueType.INTEGER.parseValue("42"));
    assertEquals(-10, ValueType.INTEGER.parseValue("-10"));
    assertEquals(0, ValueType.INTEGER.parseValue("0"));
  }

  @Test
  void testParseValue_double() {
    assertEquals(3.14, (Double) ValueType.DOUBLE.parseValue("3.14"), 0.0001);
    assertEquals(0.0, (Double) ValueType.DOUBLE.parseValue("0.0"), 0.0001);
  }

  @Test
  void testParseValue_string() {
    assertEquals("hello", ValueType.STRING.parseValue("hello"));
    assertEquals("", ValueType.STRING.parseValue(""));
  }

  @Test
  void testParseValue_invalidInput_throwsException() {
    assertThrows(IllegalArgumentException.class, () -> ValueType.BOOLEAN.parseValue("maybe"));
    assertThrows(IllegalArgumentException.class, () -> ValueType.INTEGER.parseValue("3.14"));
    assertThrows(IllegalArgumentException.class, () -> ValueType.DOUBLE.parseValue("not-a-number"));
    assertThrows(IllegalArgumentException.class, () -> ValueType.STRING.parseValue(null));
  }

  @Test
  void testGetTypeName_lowercase() {
    assertEquals("boolean", ValueType.BOOLEAN.getTypeName());
    assertEquals("integer", ValueType.INTEGER.getTypeName());
    assertEquals("double", ValueType.DOUBLE.getTypeName());
    assertEquals("string", ValueType.STRING.getTypeName());
  }
}
