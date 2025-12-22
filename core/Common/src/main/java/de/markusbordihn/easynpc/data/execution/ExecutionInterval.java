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

package de.markusbordihn.easynpc.data.execution;

public enum ExecutionInterval {
  PER_MINUTE(60_000L),
  PER_HOUR(3_600_000L),
  PER_DAY(86_400_000L),
  PER_WEEK(604_800_000L),
  PER_MONTH(2_592_000_000L),
  LIFETIME(Long.MAX_VALUE);

  private final long milliseconds;

  ExecutionInterval(long milliseconds) {
    this.milliseconds = milliseconds;
  }

  public static ExecutionInterval get(String name) {
    if (name == null || name.isEmpty()) {
      return PER_DAY;
    }
    try {
      return ExecutionInterval.valueOf(name);
    } catch (IllegalArgumentException e) {
      return PER_DAY;
    }
  }

  public boolean hasIntervalPassed(long lastExecution) {
    if (this == LIFETIME) {
      return false;
    }
    return System.currentTimeMillis() - lastExecution >= this.milliseconds;
  }

  public long getMilliseconds() {
    return this.milliseconds;
  }
}
