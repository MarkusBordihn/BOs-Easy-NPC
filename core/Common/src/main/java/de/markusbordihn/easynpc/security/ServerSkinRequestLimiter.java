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

import java.util.UUID;

public class ServerSkinRequestLimiter {

  static final int REQUESTS_PER_MINUTE = 30;
  static final int TRANSFER_UNIT_SIZE = 64 * 1024;
  static final int TRANSFER_UNITS_PER_MINUTE = 64;
  private static final PlayerRateLimiter REQUESTS = new PlayerRateLimiter();
  private static final PlayerRateLimiter TRANSFER_UNITS = new PlayerRateLimiter();

  private ServerSkinRequestLimiter() {}

  public static synchronized boolean checkAndRecord(UUID playerUUID, int byteCount) {
    int transferUnits = (byteCount + TRANSFER_UNIT_SIZE - 1) / TRANSFER_UNIT_SIZE;
    if (TRANSFER_UNITS.remaining(playerUUID, TRANSFER_UNITS_PER_MINUTE) < transferUnits
        || REQUESTS.checkAndRecord(playerUUID, REQUESTS_PER_MINUTE, 1) == 0) {
      return false;
    }

    TRANSFER_UNITS.checkAndRecord(playerUUID, TRANSFER_UNITS_PER_MINUTE, transferUnits);
    return true;
  }

  static synchronized void clearPlayer(UUID playerUUID) {
    REQUESTS.clearPlayer(playerUUID);
    TRANSFER_UNITS.clearPlayer(playerUUID);
  }
}
