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
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SpawnRateLimiterTest {

  @Test
  void testAllowsUpToLimit() {
    UUID uuid = UUID.randomUUID();
    int limit = 3;

    for (int i = 0; i < limit; i++) {
      assertTrue(
          SpawnRateLimiter.checkAndRecord(uuid, limit), "spawn " + (i + 1) + " should succeed");
    }
  }

  @Test
  void testBlocksAtLimit() {
    UUID uuid = UUID.randomUUID();
    int limit = 3;

    for (int i = 0; i < limit; i++) {
      SpawnRateLimiter.checkAndRecord(uuid, limit);
    }

    assertFalse(SpawnRateLimiter.checkAndRecord(uuid, limit));
  }

  @Test
  void testZeroLimitBlocksAll() {
    UUID uuid = UUID.randomUUID();
    assertFalse(SpawnRateLimiter.checkAndRecord(uuid, 0));
  }

  @Test
  void testPlayerIsolation() {
    UUID player1 = UUID.randomUUID();
    UUID player2 = UUID.randomUUID();
    int limit = 2;

    SpawnRateLimiter.checkAndRecord(player1, limit);
    SpawnRateLimiter.checkAndRecord(player1, limit);

    assertFalse(SpawnRateLimiter.checkAndRecord(player1, limit), "player1 should be rate-limited");
    assertTrue(SpawnRateLimiter.checkAndRecord(player2, limit), "player2 should be independent");
  }

  @Test
  void testClearResetsLimit() {
    UUID uuid = UUID.randomUUID();
    int limit = 2;

    SpawnRateLimiter.checkAndRecord(uuid, limit);
    SpawnRateLimiter.checkAndRecord(uuid, limit);
    assertFalse(SpawnRateLimiter.checkAndRecord(uuid, limit));

    SpawnRateLimiter.clearPlayer(uuid);
    assertTrue(SpawnRateLimiter.checkAndRecord(uuid, limit));
  }

  @Test
  @DisplayName("Should return full limit as remaining for an unknown player")
  void testRemainingForNewPlayer() {
    UUID uuid = UUID.randomUUID();
    assertEquals(5, SpawnRateLimiter.remainingSpawns(uuid, 5));
  }

  @Test
  @DisplayName("Should decrement remaining count with each recorded spawn")
  void testRemainingDecrementsCorrectly() {
    UUID uuid = UUID.randomUUID();
    int limit = 4;

    assertEquals(4, SpawnRateLimiter.remainingSpawns(uuid, limit));

    SpawnRateLimiter.checkAndRecord(uuid, limit);
    assertEquals(3, SpawnRateLimiter.remainingSpawns(uuid, limit));

    SpawnRateLimiter.checkAndRecord(uuid, limit);
    assertEquals(2, SpawnRateLimiter.remainingSpawns(uuid, limit));
  }

  @Test
  void testRemainingIsZeroAtLimit() {
    UUID uuid = UUID.randomUUID();
    int limit = 2;

    SpawnRateLimiter.checkAndRecord(uuid, limit);
    SpawnRateLimiter.checkAndRecord(uuid, limit);

    assertEquals(0, SpawnRateLimiter.remainingSpawns(uuid, limit));
  }

  @Test
  @DisplayName("Should return zero remaining for unknown player when limit is zero")
  void testRemainingZeroLimit() {
    UUID uuid = UUID.randomUUID();
    assertEquals(0, SpawnRateLimiter.remainingSpawns(uuid, 0));
  }

  @Test
  @DisplayName("Should grant a whole batch that fits into the limit")
  void testBatchWithinLimit() {
    UUID uuid = UUID.randomUUID();

    assertEquals(4, SpawnRateLimiter.checkAndRecord(uuid, 5, 4));
    assertEquals(1, SpawnRateLimiter.remainingSpawns(uuid, 5));
  }

  @Test
  @DisplayName("Should grant only the remaining spawns of an oversized batch")
  void testBatchGrantsRemainingOnly() {
    UUID uuid = UUID.randomUUID();
    SpawnRateLimiter.checkAndRecord(uuid, 5);
    SpawnRateLimiter.checkAndRecord(uuid, 5);

    assertEquals(3, SpawnRateLimiter.checkAndRecord(uuid, 5, 10));
    assertEquals(0, SpawnRateLimiter.remainingSpawns(uuid, 5));
    assertEquals(0, SpawnRateLimiter.checkAndRecord(uuid, 5, 1));
  }

  @Test
  @DisplayName("Should ignore batches without any requested spawn")
  void testBatchWithoutCount() {
    UUID uuid = UUID.randomUUID();

    assertEquals(0, SpawnRateLimiter.checkAndRecord(uuid, 5, 0));
    assertEquals(5, SpawnRateLimiter.remainingSpawns(uuid, 5));
  }

  @Test
  @DisplayName("Should not exceed the limit when batches run concurrently")
  void testConcurrentBatchesStayWithinLimit() throws InterruptedException {
    UUID uuid = UUID.randomUUID();
    int limit = 20;
    int threads = 8;
    AtomicInteger grantedSpawns = new AtomicInteger();
    CountDownLatch startSignal = new CountDownLatch(1);
    CountDownLatch doneSignal = new CountDownLatch(threads);

    for (int i = 0; i < threads; i++) {
      Thread worker =
          new Thread(
              () -> {
                try {
                  startSignal.await();
                  grantedSpawns.addAndGet(SpawnRateLimiter.checkAndRecord(uuid, limit, 5));
                } catch (InterruptedException e) {
                  Thread.currentThread().interrupt();
                } finally {
                  doneSignal.countDown();
                }
              });
      worker.start();
    }

    startSignal.countDown();
    assertTrue(doneSignal.await(10, TimeUnit.SECONDS), "all batches should finish");
    assertEquals(limit, grantedSpawns.get());
    assertEquals(0, SpawnRateLimiter.remainingSpawns(uuid, limit));
  }
}
