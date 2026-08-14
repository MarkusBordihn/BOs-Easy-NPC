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

package de.markusbordihn.easynpc.handler;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

public class PlayerIdleTracker {

  private static final int CHECK_INTERVAL_TICKS = 10;
  private static final double MOVEMENT_THRESHOLD_SQUARED = 0.01D;
  private static final Map<UUID, IdleState> idleStates = new ConcurrentHashMap<>();

  private static long currentTick = 0L;

  private PlayerIdleTracker() {}

  public static void handleServerTick(MinecraftServer minecraftServer) {
    if (minecraftServer == null) {
      return;
    }

    currentTick = minecraftServer.getTickCount();
    if (currentTick % CHECK_INTERVAL_TICKS != 0) {
      return;
    }

    Set<UUID> onlinePlayerUUIDs = new HashSet<>();
    for (ServerPlayer serverPlayer : minecraftServer.getPlayerList().getPlayers()) {
      onlinePlayerUUIDs.add(serverPlayer.getUUID());
      update(serverPlayer);
    }

    if (idleStates.size() != onlinePlayerUUIDs.size()) {
      idleStates.keySet().retainAll(onlinePlayerUUIDs);
    }
  }

  public static long getIdleTicks(UUID playerUUID) {
    IdleState idleState = playerUUID != null ? idleStates.get(playerUUID) : null;
    if (idleState == null) {
      return 0L;
    }

    return Math.max(0L, currentTick - idleState.lastMovementTick());
  }

  public static boolean isIdle(UUID playerUUID, int seconds) {
    return getIdleTicks(playerUUID) >= seconds * 20L;
  }

  public static void reset() {
    idleStates.clear();
    currentTick = 0L;
  }

  private static void update(ServerPlayer serverPlayer) {
    IdleState idleState = idleStates.get(serverPlayer.getUUID());
    if (idleState == null
        || idleState.position().distanceToSqr(serverPlayer.position())
            > MOVEMENT_THRESHOLD_SQUARED) {
      idleStates.put(serverPlayer.getUUID(), new IdleState(serverPlayer.position(), currentTick));
    }
  }

  private record IdleState(Vec3 position, long lastMovementTick) {}
}
