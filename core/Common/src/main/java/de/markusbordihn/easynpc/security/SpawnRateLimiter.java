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

import de.markusbordihn.easynpc.config.SecurityConfig;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import net.minecraft.server.level.ServerPlayer;

public class SpawnRateLimiter {

  static final long WINDOW_MS = 60_000L;
  private static final Map<UUID, Deque<Long>> spawnLog = new ConcurrentHashMap<>();
  private static final AtomicLong lastSweep = new AtomicLong();

  private SpawnRateLimiter() {}

  public static boolean checkAndRecord(ServerPlayer serverPlayer) {
    return checkAndRecord(serverPlayer.getUUID(), spawnLimit(serverPlayer));
  }

  public static int checkAndRecord(ServerPlayer serverPlayer, int count) {
    return checkAndRecord(serverPlayer.getUUID(), spawnLimit(serverPlayer), count);
  }

  public static int spawnLimit(ServerPlayer serverPlayer) {
    ActorSecurityContext actorContext = CommandSecurity.getActorContext(serverPlayer);

    return actorContext != null && actorContext.admin()
        ? SecurityConfig.NPC_SPAWN_RATE_LIMIT_ADMIN
        : SecurityConfig.NPC_SPAWN_RATE_LIMIT_CREATIVE;
  }

  static int checkAndRecord(UUID uuid, int limit, int count) {
    if (count <= 0) {
      return 0;
    }

    long now = System.currentTimeMillis();
    sweepExpired(now);
    int[] grantedSpawns = new int[1];
    spawnLog.compute(
        uuid,
        (playerUuid, spawnTimestamps) -> {
          Deque<Long> activeTimestamps = pruneExpired(spawnTimestamps, now);
          grantedSpawns[0] = Math.max(0, Math.min(count, limit - activeTimestamps.size()));
          for (int i = 0; i < grantedSpawns[0]; i++) {
            activeTimestamps.addLast(now);
          }

          return activeTimestamps.isEmpty() ? null : activeTimestamps;
        });

    return grantedSpawns[0];
  }

  static boolean checkAndRecord(UUID uuid, int limit) {
    return checkAndRecord(uuid, limit, 1) > 0;
  }

  public static int remainingSpawns(ServerPlayer serverPlayer) {
    return remainingSpawns(serverPlayer.getUUID(), spawnLimit(serverPlayer));
  }

  static int remainingSpawns(UUID uuid, int limit) {
    long now = System.currentTimeMillis();
    int[] availableSpawns = new int[1];
    spawnLog.compute(
        uuid,
        (playerUuid, spawnTimestamps) -> {
          Deque<Long> activeTimestamps = pruneExpired(spawnTimestamps, now);
          availableSpawns[0] = Math.max(0, limit - activeTimestamps.size());

          return activeTimestamps.isEmpty() ? null : activeTimestamps;
        });

    return availableSpawns[0];
  }

  public static void clearPlayer(UUID playerUuid) {
    spawnLog.remove(playerUuid);
  }

  private static Deque<Long> pruneExpired(Deque<Long> spawnTimestamps, long now) {
    if (spawnTimestamps == null) {
      return new ArrayDeque<>();
    }

    while (!spawnTimestamps.isEmpty() && now - spawnTimestamps.peekFirst() > WINDOW_MS) {
      spawnTimestamps.pollFirst();
    }

    return spawnTimestamps;
  }

  private static void sweepExpired(long now) {
    long previousSweep = lastSweep.get();
    if (now - previousSweep < WINDOW_MS || !lastSweep.compareAndSet(previousSweep, now)) {
      return;
    }

    for (UUID playerUuid : spawnLog.keySet()) {
      spawnLog.computeIfPresent(
          playerUuid,
          (playerKey, spawnTimestamps) ->
              pruneExpired(spawnTimestamps, now).isEmpty() ? null : spawnTimestamps);
    }
  }
}
