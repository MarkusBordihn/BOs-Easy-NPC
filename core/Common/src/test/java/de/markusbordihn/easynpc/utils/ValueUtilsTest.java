/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.utils;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

@DisplayName("ValueUtils Tests")
class ValueUtilsTest {

  @ParameterizedTest
  @ValueSource(strings = {"0", "1.5", "100", "0.0", "999.999"})
  void testIsFloatValue_valid(String value) {
    assertTrue(ValueUtils.isFloatValue(value));
  }

  @ParameterizedTest
  @ValueSource(strings = {"-1", "-0.5", "abc", "1.2.3", "NaN"})
  void testIsFloatValue_invalid(String value) {
    assertFalse(ValueUtils.isFloatValue(value));
  }

  @Test
  void testIsFloatValue_emptyString() {
    assertTrue(ValueUtils.isFloatValue(""));
  }

  @ParameterizedTest
  @ValueSource(strings = {"0", "1.5", "100.123", "0.0", "999.999"})
  void testIsDoubleValue_valid(String value) {
    assertTrue(ValueUtils.isDoubleValue(value));
  }

  @ParameterizedTest
  @ValueSource(strings = {"-1", "-0.5", "abc", "1.2.3", "NaN"})
  void testIsDoubleValue_invalid(String value) {
    assertFalse(ValueUtils.isDoubleValue(value));
  }

  @ParameterizedTest
  @CsvSource({"0.5, 0.0, 1.0", "50, 0, 100", "10.5, 10, 11"})
  void testIsDoubleValue_inRange(String value, double min, double max) {
    assertTrue(ValueUtils.isDoubleValue(value, min, max));
  }

  @ParameterizedTest
  @CsvSource({"1.5, 2.0, 3.0", "50, 60, 100", "-1, 0, 10"})
  void testIsDoubleValue_outOfRange(String value, double min, double max) {
    assertFalse(ValueUtils.isDoubleValue(value, min, max));
  }

  @ParameterizedTest
  @DisplayName("Should validate valid degree values (-180 to 180)")
  @ValueSource(strings = {"0", "90", "-90", "180", "-180", "45.5", "-45.5"})
  void testIsDegreeValue_valid(String value) {
    assertTrue(ValueUtils.isDegreeValue(value));
  }

  @ParameterizedTest
  @ValueSource(strings = {"181", "-181", "360", "-360", "abc", ""})
  void testIsDegreeValue_invalid(String value) {
    assertFalse(ValueUtils.isDegreeValue(value));
  }

  @ParameterizedTest
  @ValueSource(strings = {"0", "100", "-100", "1000.5", "-1000.5", "30000000"})
  void testIsPositionValue_valid(String value) {
    assertTrue(ValueUtils.isPositionValue(value));
  }

  @ParameterizedTest
  @ValueSource(strings = {"32000001", "-32000001", "abc", ""})
  void testIsPositionValue_invalid(String value) {
    assertFalse(ValueUtils.isPositionValue(value));
  }

  @ParameterizedTest
  @DisplayName("Should validate valid scale values (0.0 to 10.0)")
  @ValueSource(strings = {"0", "1", "5.5", "10", "0.01", "9.99"})
  void testIsScaleValue_valid(String value) {
    assertTrue(ValueUtils.isScaleValue(value));
  }

  @ParameterizedTest
  @ValueSource(strings = {"-1", "10.01", "11", "100", "abc", "", "1.234"})
  void testIsScaleValue_invalid(String value) {
    assertFalse(ValueUtils.isScaleValue(value));
  }

  @Test
  @DisplayName("Should validate scale with exactly 2 decimal places")
  void testIsScaleValue_decimalPrecision() {
    assertTrue(ValueUtils.isScaleValue("1.23"));
    assertFalse(ValueUtils.isScaleValue("1.234"));
  }

  @ParameterizedTest
  @ValueSource(strings = {"0", "1", "100", "999"})
  void testIsPositiveNumericValueOrZero_valid(String value) {
    assertTrue(ValueUtils.isPositiveNumericValueOrZero(value));
  }

  @ParameterizedTest
  @DisplayName("Should reject negative or non-numeric values")
  @ValueSource(strings = {"-1", "-100", "abc", "1.5", ""})
  void testIsPositiveNumericValueOrZero_invalid(String value) {
    assertFalse(ValueUtils.isPositiveNumericValueOrZero(value));
  }

  @ParameterizedTest
  @CsvSource({"5, 0, 10", "0, 0, 10", "10, 0, 10", "50, 0, 100"})
  void testIsNumericValue_inRange(String value, int min, int max) {
    assertTrue(ValueUtils.isNumericValue(value, min, max));
  }

  @ParameterizedTest
  @CsvSource({"11, 0, 10", "-1, 0, 10", "101, 0, 100"})
  void testIsNumericValue_outOfRange(String value, int min, int max) {
    assertFalse(ValueUtils.isNumericValue(value, min, max));
  }

  @ParameterizedTest
  @ValueSource(strings = {"0", "1", "-1", "100", "-100", ""})
  void testIsNumericValue_withNegatives(String value) {
    assertTrue(ValueUtils.isNumericValue(value));
  }

  @ParameterizedTest
  @ValueSource(strings = {"abc", "1.5", "1a", "a1"})
  void testIsNumericValue_nonNumeric(String value) {
    assertFalse(ValueUtils.isNumericValue(value));
  }

  @ParameterizedTest
  @NullAndEmptySource
  void testValidationMethods_nullHandling(String value) {
    assertFalse(ValueUtils.isDegreeValue(value));
    assertFalse(ValueUtils.isPositionValue(value));
    assertFalse(ValueUtils.isScaleValue(value));
    assertFalse(ValueUtils.isPositiveNumericValueOrZero(value));
  }

  @ParameterizedTest
  @CsvSource({"100.5, 0, 200", "-100, -200, 0", "0, -100, 100"})
  void testIsPositionValueInRange(String value, double min, double max) {
    assertTrue(ValueUtils.isPositionValueInRange(value, min, max));
  }

  @ParameterizedTest
  @CsvSource({"5.5, 1, 10", "0.5, 0, 1", "9.99, 0, 10"})
  void testIsScaleValueInRange(String value, double min, double max) {
    assertTrue(ValueUtils.isScaleValueInRange(value, min, max));
  }
}
