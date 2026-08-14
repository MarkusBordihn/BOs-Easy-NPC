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

package de.markusbordihn.easynpc.data.action;

import java.util.Locale;

public record WaitDuration(int ticks) {

  public static final int MIN_TICKS = 1;
  public static final int MAX_TICKS = 12000;
  public static final long INVALID_TICKS = -1L;
  public static final WaitDuration EMPTY = new WaitDuration(0);

  private static final int TICKS_PER_SECOND = 20;
  private static final int TICKS_PER_MINUTE = 60 * TICKS_PER_SECOND;

  public static WaitDuration parse(String command) {
    long ticks = parseUnclampedTicks(command);
    if (ticks == INVALID_TICKS) {
      return EMPTY;
    }

    return new WaitDuration((int) Math.min(MAX_TICKS, Math.max(MIN_TICKS, ticks)));
  }

  public static long parseUnclampedTicks(String command) {
    if (command == null) {
      return INVALID_TICKS;
    }

    String duration = command.trim().toLowerCase(Locale.ROOT);
    if (duration.isEmpty()) {
      return INVALID_TICKS;
    }

    char unit = duration.charAt(duration.length() - 1);
    String amount =
        Character.isDigit(unit) ? duration : duration.substring(0, duration.length() - 1).trim();

    double value;
    try {
      value = Double.parseDouble(amount);
    } catch (NumberFormatException e) {
      return INVALID_TICKS;
    }

    if (!Double.isFinite(value) || value <= 0.0D) {
      return INVALID_TICKS;
    }

    double ticks;
    if (Character.isDigit(unit) || unit == 's') {
      ticks = value * TICKS_PER_SECOND;
    } else if (unit == 't') {
      ticks = value;
    } else if (unit == 'm') {
      ticks = value * TICKS_PER_MINUTE;
    } else {
      return INVALID_TICKS;
    }

    return Math.max(1L, Math.round(Math.min(ticks, Long.MAX_VALUE / 2.0D)));
  }

  public boolean isValid() {
    return this.ticks >= MIN_TICKS;
  }
}
