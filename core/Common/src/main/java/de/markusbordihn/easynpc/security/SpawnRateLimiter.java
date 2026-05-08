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
import net.minecraft.server.level.ServerPlayer;

public class SpawnRateLimiter {

  static final long WINDOW_MS = 60_000L;
  private static final Map<UUID, Deque<Long>> spawnLog = new ConcurrentHashMap<>();

  private SpawnRateLimiter() {}

  public static boolean checkAndRecord(ServerPlayer serverPlayer) {
    ActorSecurityContext actorContext = CommandSecurity.getActorContext(serverPlayer);

    return checkAndRecord(
        serverPlayer.getUUID(),
        actorContext != null && actorContext.admin()
            ? SecurityConfig.NPC_SPAWN_RATE_LIMIT_ADMIN
            : SecurityConfig.NPC_SPAWN_RATE_LIMIT_CREATIVE);
  }

  static boolean checkAndRecord(UUID uuid, int limit) {
    long now = System.currentTimeMillis();
    Deque<Long> log = spawnLog.computeIfAbsent(uuid, k -> new ArrayDeque<>());
    synchronized (log) {
      while (!log.isEmpty() && now - log.peekFirst() > WINDOW_MS) {
        log.pollFirst();
      }
      if (log.size() >= limit) {
        return false;
      }

      log.addLast(now);
    }

    return true;
  }

  public static int remainingSpawns(ServerPlayer serverPlayer) {
    ActorSecurityContext actorContext = CommandSecurity.getActorContext(serverPlayer);

    return remainingSpawns(
        serverPlayer.getUUID(),
        actorContext != null && actorContext.admin()
            ? SecurityConfig.NPC_SPAWN_RATE_LIMIT_ADMIN
            : SecurityConfig.NPC_SPAWN_RATE_LIMIT_CREATIVE);
  }

  static int remainingSpawns(UUID uuid, int limit) {
    Deque<Long> log = spawnLog.get(uuid);
    if (log == null) {
      return limit;
    }

    long now = System.currentTimeMillis();
    synchronized (log) {
      while (!log.isEmpty() && now - log.peekFirst() > WINDOW_MS) {
        log.pollFirst();
      }
      return Math.max(0, limit - log.size());
    }
  }

  public static void clearPlayer(UUID playerUuid) {
    spawnLog.remove(playerUuid);
  }
}
