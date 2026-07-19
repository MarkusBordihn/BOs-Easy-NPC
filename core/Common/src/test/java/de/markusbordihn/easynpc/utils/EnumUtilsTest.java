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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class EnumUtilsTest {

  @Test
  void testGet_validName() {
    assertEquals(Sample.BETA, EnumUtils.get(Sample.class, "BETA", Sample.ALPHA));
  }

  @Test
  void testGet_null_returnsFallback() {
    assertEquals(Sample.ALPHA, EnumUtils.get(Sample.class, null, Sample.ALPHA));
  }

  @Test
  void testGet_empty_returnsFallback() {
    assertEquals(Sample.ALPHA, EnumUtils.get(Sample.class, "", Sample.ALPHA));
  }

  @Test
  void testGet_unknown_returnsFallback() {
    assertEquals(Sample.GAMMA, EnumUtils.get(Sample.class, "DELTA", Sample.GAMMA));
  }

  @Test
  void testGet_isCaseSensitive() {
    assertEquals(Sample.ALPHA, EnumUtils.get(Sample.class, "beta", Sample.ALPHA));
  }

  @Test
  void testGet_nullFallbackAllowed() {
    assertNull(EnumUtils.get(Sample.class, "DELTA", null));
  }

  @Test
  void testGetIgnoreCase_matchesRegardlessOfCase() {
    assertEquals(Sample.BETA, EnumUtils.getIgnoreCase(Sample.class, "beta", Sample.ALPHA));
    assertEquals(Sample.BETA, EnumUtils.getIgnoreCase(Sample.class, "BeTa", Sample.ALPHA));
    assertEquals(Sample.BETA, EnumUtils.getIgnoreCase(Sample.class, "BETA", Sample.ALPHA));
  }

  @Test
  void testGetIgnoreCase_unknown_returnsFallback() {
    assertEquals(Sample.ALPHA, EnumUtils.getIgnoreCase(Sample.class, "delta", Sample.ALPHA));
  }

  @Test
  void testGetIgnoreCase_null_returnsFallback() {
    assertEquals(Sample.ALPHA, EnumUtils.getIgnoreCase(Sample.class, null, Sample.ALPHA));
  }

  private enum Sample {
    ALPHA,
    BETA,
    GAMMA
  }
}
