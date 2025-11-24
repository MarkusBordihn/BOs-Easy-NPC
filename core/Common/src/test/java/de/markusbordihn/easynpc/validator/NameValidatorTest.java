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

package de.markusbordihn.easynpc.validator;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

@DisplayName("NameValidator Tests")
class NameValidatorTest {

  @ParameterizedTest
  @DisplayName("Should validate correct player names")
  @ValueSource(
      strings = {
        "Player123",
        "Test_User",
        "Gamer99",
        "MinecraftPro",
        "User_Name_123",
        "abc",
        "ABCDEFGHIJ123456"
      })
  void testIsValidPlayerName_valid(String name) {
    assertTrue(NameValidator.isValidPlayerName(name));
  }

  @ParameterizedTest
  @DisplayName("Should reject player names that are too short")
  @ValueSource(strings = {"ab", "a", "12"})
  void testIsValidPlayerName_tooShort(String name) {
    assertFalse(NameValidator.isValidPlayerName(name));
  }

  @ParameterizedTest
  @DisplayName("Should reject player names that are too long")
  @ValueSource(strings = {"ThisNameIsTooLong1", "ABCDEFGHIJ1234567", "VeryLongUsername123"})
  void testIsValidPlayerName_tooLong(String name) {
    assertFalse(NameValidator.isValidPlayerName(name));
  }

  @ParameterizedTest
  @DisplayName("Should reject player names with invalid characters")
  @ValueSource(
      strings = {
        "Player-123",
        "User Name",
        "Player@123",
        "Test.User",
        "User#Name",
        "Player!",
        "Test$User"
      })
  void testIsValidPlayerName_invalidCharacters(String name) {
    assertFalse(NameValidator.isValidPlayerName(name));
  }

  @ParameterizedTest
  @DisplayName("Should reject player names starting with 'http'")
  @ValueSource(strings = {"http", "https", "httpUser", "httpsPlayer"})
  void testIsValidPlayerName_startsWithHttp(String name) {
    assertFalse(NameValidator.isValidPlayerName(name));
  }

  @ParameterizedTest
  @DisplayName("Should reject player name 'htt'")
  @ValueSource(strings = {"htt"})
  void testIsValidPlayerName_htt(String name) {
    assertFalse(NameValidator.isValidPlayerName(name));
  }

  @ParameterizedTest
  @NullAndEmptySource
  @DisplayName("Should reject null or empty player names")
  void testIsValidPlayerName_nullOrEmpty(String name) {
    assertFalse(NameValidator.isValidPlayerName(name));
  }

  @ParameterizedTest
  @DisplayName("Should validate player names at boundary lengths")
  @ValueSource(strings = {"abc", "ABCDEFGHIJ123456"})
  void testIsValidPlayerName_boundaryLengths(String name) {
    assertTrue(NameValidator.isValidPlayerName(name));
  }

  @ParameterizedTest
  @DisplayName("Should validate alphanumeric player names")
  @ValueSource(strings = {"Player1", "Test2", "User123", "Gamer999"})
  void testIsValidPlayerName_alphanumeric(String name) {
    assertTrue(NameValidator.isValidPlayerName(name));
  }

  @ParameterizedTest
  @DisplayName("Should validate player names with underscores")
  @ValueSource(strings = {"Player_1", "Test_User", "User_Name_123", "___test___"})
  void testIsValidPlayerName_withUnderscores(String name) {
    assertTrue(NameValidator.isValidPlayerName(name));
  }

  @ParameterizedTest
  @DisplayName("Should validate numeric player names")
  @ValueSource(strings = {"123", "999", "12345", "1234567890123456"})
  void testIsValidPlayerName_numeric(String name) {
    assertTrue(NameValidator.isValidPlayerName(name));
  }
}
