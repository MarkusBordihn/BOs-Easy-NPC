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

package de.markusbordihn.easynpc.utils;

import java.util.UUID;

public class UUIDUtils {

  private UUIDUtils() {}

  public static short[] encodeUUIDToShort(UUID uuid) {
    long mostSignificantBits = uuid.getMostSignificantBits();
    long leastSignificantBits = uuid.getLeastSignificantBits();

    short[] encodedShorts = new short[8];
    for (int i = 0; i < 4; i++) {
      encodedShorts[i] = (short) (mostSignificantBits >>> (i * 16));
      encodedShorts[i + 4] = (short) (leastSignificantBits >>> (i * 16));
    }

    return encodedShorts;
  }

  public static UUID decodeShortToUUID(short[] encodedShorts) {
    long mostSignificantBits = 0;
    long leastSignificantBits = 0;

    for (int i = 0; i < 4; i++) {
      mostSignificantBits |= ((long) encodedShorts[i] & 0xFFFF) << (i * 16);
      leastSignificantBits |= ((long) encodedShorts[i + 4] & 0xFFFF) << (i * 16);
    }

    return new UUID(mostSignificantBits, leastSignificantBits);
  }
}
