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

package de.markusbordihn.easynpc.utils;

import static org.junit.jupiter.api.Assertions.*;

import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

class UUIDUtilsTest {

  @Test
  void testTextToUUID_isDeterministic() {
    UUID first = UUIDUtils.textToUUID("test_npc_name");
    UUID second = UUIDUtils.textToUUID("test_npc_name");
    assertEquals(first, second);
  }

  @Test
  void testTextToUUID_differentInputs_differentUUIDs() {
    UUID uuid1 = UUIDUtils.textToUUID("npc_one");
    UUID uuid2 = UUIDUtils.textToUUID("npc_two");
    assertNotEquals(uuid1, uuid2);
  }

  @Test
  void testTextToUUID_knownValue_isStable() {
    UUID result = UUIDUtils.textToUUID("BOs-Easy-NPC");
    assertNotNull(result);
    assertEquals(result, UUIDUtils.textToUUID("BOs-Easy-NPC"));
  }

  @ParameterizedTest
  @NullAndEmptySource
  void testTextToUUID_nullOrEmpty_returnsNonNull(String input) {
    UUID result = UUIDUtils.textToUUID(input);
    assertNotNull(result);
  }

  @Test
  void testParseUUID_validString() {
    String validUUID = "550e8400-e29b-41d4-a716-446655440000";
    UUID result = UUIDUtils.parseUUID(validUUID);
    assertNotNull(result);
    assertEquals(UUID.fromString(validUUID), result);
  }

  @Test
  void testParseUUID_null_returnsNull() {
    assertNull(UUIDUtils.parseUUID(null));
  }

  @Test
  void testParseUUID_wrongLength_returnsNull() {
    assertNull(UUIDUtils.parseUUID("550e8400-e29b-41d4"));
    assertNull(UUIDUtils.parseUUID("550e8400-e29b-41d4-a716-446655440000-extra"));
    assertNull(UUIDUtils.parseUUID(""));
  }

  @ParameterizedTest
  @ValueSource(
      strings = {
        "not-a-uuid-at-all-here",
        "GGGGGGGG-GGGG-GGGG-GGGG-GGGGGGGGGGGG",
        "550e8400_e29b_41d4_a716_446655440000"
      })
  void testParseUUID_invalidPattern_returnsNull(String input) {
    assertNull(UUIDUtils.parseUUID(input));
  }

  @Test
  void testParseUUID_caseInsensitive() {
    UUID lower = UUIDUtils.parseUUID("550e8400-e29b-41d4-a716-446655440000");
    UUID upper = UUIDUtils.parseUUID("550E8400-E29B-41D4-A716-446655440000");
    assertNotNull(lower);
    assertNotNull(upper);
    assertEquals(lower, upper);
  }
}
