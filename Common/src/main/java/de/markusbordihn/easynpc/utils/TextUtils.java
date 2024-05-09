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

import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

public class TextUtils {

  private TextUtils() {}

  public static Component normalizeName(String name) {
    return Component.literal(normalizeString(name));
  }

  public static String normalizeString(String string) {
    String normalizedString = string.toLowerCase().replace("_", " ").replace("-", " ");
    normalizedString =
        normalizedString.substring(0, 1).toUpperCase() + normalizedString.substring(1);
    return normalizedString;
  }

  public static String normalizeString(String string, int maxSize) {
    return limitString(normalizeString(string), maxSize);
  }

  public static String limitString(String string, int maxSize) {
    if (string == null || string.isBlank()) {
      return string;
    }
    String trimmedString = string.trim();
    int stringLength = trimmedString.length();
    if (stringLength <= maxSize) {
      return trimmedString;
    }
    return trimmedString.substring(0, maxSize) + '…';
  }

  public static Component removeAction(Component component) {
    MutableComponent mutableComponent =
        component.plainCopy().setStyle(component.getStyle().withClickEvent(null));
    for (Component componentSibling : component.getSiblings()) {
      mutableComponent.append(removeAction(componentSibling));
    }
    return mutableComponent;
  }
}
