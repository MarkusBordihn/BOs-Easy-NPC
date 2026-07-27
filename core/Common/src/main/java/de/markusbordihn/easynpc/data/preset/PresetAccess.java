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

package de.markusbordihn.easynpc.data.preset;

import de.markusbordihn.easynpc.Constants;
import java.util.Locale;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public enum PresetAccess {
  PUBLIC,
  RESTRICTED,
  INTERNAL;

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static PresetAccess get(String presetAccess) {
    if (presetAccess == null || presetAccess.isEmpty()) {
      return PUBLIC;
    }

    try {
      return PresetAccess.valueOf(presetAccess.toUpperCase(Locale.ROOT));
    } catch (IllegalArgumentException e) {
      log.warn("Unknown preset access {}, using {} instead.", presetAccess, PUBLIC);
      return PUBLIC;
    }
  }

  public boolean isListedForPlayers() {
    return this == PUBLIC;
  }

  public boolean isUsableByCommand() {
    return this != INTERNAL;
  }
}
