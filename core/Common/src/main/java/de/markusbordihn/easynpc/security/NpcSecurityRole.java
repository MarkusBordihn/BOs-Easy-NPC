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

package de.markusbordihn.easynpc.security;

import java.util.Locale;

public enum NpcSecurityRole {
  NORMAL_PLAYER(0),
  CREATIVE_PLAYER(1),
  ADMIN(2),
  SERVER_TRUSTED(3);

  private final int rank;

  NpcSecurityRole(int rank) {
    this.rank = rank;
  }

  public static NpcSecurityRole parse(String value, NpcSecurityRole defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }

    try {
      return valueOf(value.trim().toUpperCase(Locale.ROOT));
    } catch (IllegalArgumentException exception) {
      return defaultValue;
    }
  }

  public boolean allows(NpcSecurityRole requiredRole) {
    return requiredRole != null && this.rank >= requiredRole.rank;
  }
}
