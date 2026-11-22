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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.io.ServerSkinIndex;
import java.util.UUID;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ServerSkinRequestLimiterTest {

  private final UUID playerUUID = UUID.randomUUID();

  @AfterEach
  void clearPlayer() {
    ServerSkinRequestLimiter.clearPlayer(this.playerUUID);
  }

  @Test
  @DisplayName("Requests above the per-minute limit are dropped")
  void requestsAboveLimitAreDropped() {
    for (int i = 0; i < ServerSkinRequestLimiter.REQUESTS_PER_MINUTE; i++) {
      assertTrue(ServerSkinRequestLimiter.checkAndRecord(this.playerUUID, 0));
    }

    assertFalse(ServerSkinRequestLimiter.checkAndRecord(this.playerUUID, 0));
    assertTrue(ServerSkinRequestLimiter.checkAndRecord(UUID.randomUUID(), 0));
  }

  @Test
  @DisplayName("Transfer budget stops large skins but keeps small answers possible")
  void transferBudgetStopsLargeSkins() {
    int largestSkinSize = ServerSkinIndex.MAX_SKIN_FILE_SIZE;
    int largestSkinsPerMinute =
        ServerSkinRequestLimiter.TRANSFER_UNITS_PER_MINUTE
            / (largestSkinSize / ServerSkinRequestLimiter.TRANSFER_UNIT_SIZE);
    for (int i = 0; i < largestSkinsPerMinute; i++) {
      assertTrue(ServerSkinRequestLimiter.checkAndRecord(this.playerUUID, largestSkinSize));
    }

    assertFalse(ServerSkinRequestLimiter.checkAndRecord(this.playerUUID, largestSkinSize));
    assertTrue(ServerSkinRequestLimiter.checkAndRecord(this.playerUUID, 1024));
  }
}
