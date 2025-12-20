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

package de.markusbordihn.easynpc.data.dialog;

public final class DialogPriority {

  public static final int MANUAL_ONLY = -1;
  public static final int FALLBACK = 0;
  public static final int LOW = 1;
  public static final int NORMAL = 5;
  public static final int HIGH = 10;
  public static final int CRITICAL = 100;

  private DialogPriority() {}

  public static int calculateDefaultPriority(String label) {
    if (label == null || label.isEmpty()) {
      return FALLBACK;
    }

    return switch (label.toLowerCase().trim()) {
      case "default", "start", "welcome", "greeting", "intro", "introduction" -> HIGH;
      case "main", "question", "help", "info", "information", "talk", "conversation" -> NORMAL;
      case "bye", "goodbye", "farewell", "exit", "leave", "thanks", "thankyou", "idle", "random" ->
          LOW;
      default -> FALLBACK;
    };
  }

  public static String getNameForPriority(int priority) {
    return switch (priority) {
      case MANUAL_ONLY -> "Manual Only";
      case FALLBACK -> "Fallback";
      case LOW -> "Low";
      case NORMAL -> "Normal";
      case HIGH -> "High";
      case CRITICAL -> "Critical";
      default -> "Custom";
    };
  }

  public static String getDisplayName(int priority) {
    return getNameForPriority(priority) + " (" + priority + ")";
  }
}
