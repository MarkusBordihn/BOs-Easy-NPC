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

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

final class PlayerRateLimiter {

  static final long WINDOW_MS = 60_000L;
  private final Map<UUID, Deque<Long>> actionLog = new ConcurrentHashMap<>();
  private final AtomicLong lastSweep = new AtomicLong();

  int checkAndRecord(UUID uuid, int limit, int count) {
    if (count <= 0) {
      return 0;
    }

    long now = System.currentTimeMillis();
    this.sweepExpired(now);
    int[] grantedActions = new int[1];
    this.actionLog.compute(
        uuid,
        (playerUuid, actionTimestamps) -> {
          Deque<Long> activeTimestamps = pruneExpired(actionTimestamps, now);
          grantedActions[0] = Math.max(0, Math.min(count, limit - activeTimestamps.size()));
          for (int i = 0; i < grantedActions[0]; i++) {
            activeTimestamps.addLast(now);
          }

          return activeTimestamps.isEmpty() ? null : activeTimestamps;
        });

    return grantedActions[0];
  }

  int remaining(UUID uuid, int limit) {
    long now = System.currentTimeMillis();
    int[] availableActions = new int[1];
    this.actionLog.compute(
        uuid,
        (playerUuid, actionTimestamps) -> {
          Deque<Long> activeTimestamps = pruneExpired(actionTimestamps, now);
          availableActions[0] = Math.max(0, limit - activeTimestamps.size());

          return activeTimestamps.isEmpty() ? null : activeTimestamps;
        });

    return availableActions[0];
  }

  void clearPlayer(UUID playerUuid) {
    this.actionLog.remove(playerUuid);
  }

  private static Deque<Long> pruneExpired(Deque<Long> actionTimestamps, long now) {
    if (actionTimestamps == null) {
      return new ArrayDeque<>();
    }

    while (!actionTimestamps.isEmpty() && now - actionTimestamps.peekFirst() > WINDOW_MS) {
      actionTimestamps.pollFirst();
    }

    return actionTimestamps;
  }

  private void sweepExpired(long now) {
    long previousSweep = this.lastSweep.get();
    if (now - previousSweep < WINDOW_MS || !this.lastSweep.compareAndSet(previousSweep, now)) {
      return;
    }

    for (UUID playerUuid : this.actionLog.keySet()) {
      this.actionLog.computeIfPresent(
          playerUuid,
          (playerKey, actionTimestamps) ->
              pruneExpired(actionTimestamps, now).isEmpty() ? null : actionTimestamps);
    }
  }
}
