/*
 * Copyright 2022 Markus Bordihn
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

import de.markusbordihn.easynpc.Constants;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ValueUtils {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ValueUtils() {}

  public static boolean isFloatValue(String text) {
    return text != null
        && (text.isEmpty()
            || (text.matches("^\\d+(\\.?\\d*)?$") && Float.parseFloat(text) >= 0.0F));
  }

  public static boolean isPositiveNumericValue(String text) {
    return text != null
        && (text.isEmpty() || (text.matches("^\\d+$") && Integer.parseInt(text) > 0));
  }

  public static boolean isPositiveNumericValueOrZero(String text) {
    return text != null
        && (text.isEmpty() || (text.matches("^\\d+$") && Integer.parseInt(text) >= 0));
  }

  public static boolean isNumericValue(String text) {
    return text != null && (text.isEmpty() || (text.matches("^-?\\d+$")));
  }

  public static boolean isBlockPosValue(String text) {
    return text != null && (text.isEmpty() || (text.matches("^~?-?\\d+$")));
  }

  public static Double getDoubleValue(String value) {
    if (value != null && !value.isEmpty()) {
      try {
        return Double.parseDouble(value);
      } catch (NumberFormatException e) {
        log.error("Failed to parse double value: {}", value);
      }
    }
    return null;
  }
}
