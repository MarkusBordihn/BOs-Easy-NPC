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
import java.util.UUID;
import net.minecraft.server.level.ServerPlayer;

public class SpawnRateLimiter {

  private static final PlayerRateLimiter SPAWNS = new PlayerRateLimiter();

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
    return SPAWNS.checkAndRecord(uuid, limit, count);
  }

  static boolean checkAndRecord(UUID uuid, int limit) {
    return checkAndRecord(uuid, limit, 1) > 0;
  }

  public static int remainingSpawns(ServerPlayer serverPlayer) {
    return remainingSpawns(serverPlayer.getUUID(), spawnLimit(serverPlayer));
  }

  static int remainingSpawns(UUID uuid, int limit) {
    return SPAWNS.remaining(uuid, limit);
  }

  public static void clearPlayer(UUID playerUuid) {
    SPAWNS.clearPlayer(playerUuid);
  }
}
