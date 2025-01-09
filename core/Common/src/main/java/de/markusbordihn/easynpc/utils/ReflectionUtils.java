/*
 * Copyright 2024 Markus Bordihn
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

import java.lang.reflect.Field;
import java.util.UUID;

public class ReflectionUtils {

  private ReflectionUtils() {}

  public static boolean changeIntValueField(
      Object object, final String[] fieldNames, final int value) {
    for (String fieldName : fieldNames) {
      if (changeIntValueField(object, fieldName, value)) {
        return true;
      }
    }
    return false;
  }

  public static boolean changeIntValueField(
      Object object, final String fieldName, final int value) {
    try {
      Field field = object.getClass().getDeclaredField(fieldName);
      field.setAccessible(true);
      field.setInt(object, value);
      return true;
    } catch (NoSuchFieldException | IllegalAccessException e) {
      return false;
    }
  }

  public static UUID getUUIDValueField(Object object, final String fieldName) {
    try {
      Field field = object.getClass().getDeclaredField(fieldName);
      field.setAccessible(true);
      return (UUID) field.get(object);
    } catch (NoSuchFieldException | IllegalAccessException e) {
      return null;
    }
  }

  public static UUID getUUIDValueField(Object object, final String[] fieldNames) {
    for (String fieldName : fieldNames) {
      UUID uuid = getUUIDValueField(object, fieldName);
      if (uuid != null) {
        return uuid;
      }
    }
    return null;
  }
}
