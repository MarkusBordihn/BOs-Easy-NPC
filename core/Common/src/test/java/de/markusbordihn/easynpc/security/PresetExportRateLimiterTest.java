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

import static org.junit.jupiter.api.Assertions.*;

import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class PresetExportRateLimiterTest {

  @Test
  @DisplayName("Should allow exports up to the limit")
  void testAllowsUpToLimit() {
    UUID uuid = UUID.randomUUID();

    for (int i = 0; i < 3; i++) {
      assertTrue(PresetExportRateLimiter.checkAndRecord(uuid, 3), "export " + (i + 1));
    }
  }

  @Test
  @DisplayName("Should block exports once the limit is reached")
  void testBlocksAtLimit() {
    UUID uuid = UUID.randomUUID();
    PresetExportRateLimiter.checkAndRecord(uuid, 2);
    PresetExportRateLimiter.checkAndRecord(uuid, 2);

    assertFalse(PresetExportRateLimiter.checkAndRecord(uuid, 2));
  }

  @Test
  @DisplayName("Should allow exports again after the player was cleared")
  void testClearResetsLimit() {
    UUID uuid = UUID.randomUUID();
    PresetExportRateLimiter.checkAndRecord(uuid, 1);
    assertFalse(PresetExportRateLimiter.checkAndRecord(uuid, 1));

    PresetExportRateLimiter.clearPlayer(uuid);
    assertTrue(PresetExportRateLimiter.checkAndRecord(uuid, 1));
  }

  @Test
  @DisplayName("Should keep the export budget separate from the spawn budget")
  void testIndependentFromSpawnBudget() {
    UUID uuid = UUID.randomUUID();
    PresetExportRateLimiter.checkAndRecord(uuid, 1);

    assertFalse(PresetExportRateLimiter.checkAndRecord(uuid, 1));
    assertTrue(SpawnRateLimiter.checkAndRecord(uuid, 1));
    assertFalse(SpawnRateLimiter.checkAndRecord(uuid, 1));
    SpawnRateLimiter.clearPlayer(uuid);
    assertFalse(PresetExportRateLimiter.checkAndRecord(uuid, 1));
  }
}
