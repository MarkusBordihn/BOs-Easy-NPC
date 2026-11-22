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

import java.nio.charset.StandardCharsets;
import java.util.UUID;

public class SkinFileNameUtils {

  public static final String PNG_FILE_EXTENSION = ".png";

  private SkinFileNameUtils() {}

  public static UUID getSkinUUID(String fileName) {
    if (fileName == null || !fileName.endsWith(PNG_FILE_EXTENSION)) {
      return null;
    }

    try {
      return UUID.fromString(fileName.substring(0, fileName.indexOf('.')));
    } catch (IllegalArgumentException e) {
      return UUID.nameUUIDFromBytes(fileName.getBytes(StandardCharsets.UTF_8));
    }
  }

  public static String withPngExtension(String fileName) {
    return fileName.endsWith(PNG_FILE_EXTENSION) ? fileName : fileName + PNG_FILE_EXTENSION;
  }
}
