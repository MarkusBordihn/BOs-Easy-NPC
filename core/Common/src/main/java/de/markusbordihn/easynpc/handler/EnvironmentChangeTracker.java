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

import de.markusbordihn.easynpc.entity.easynpc.handlers.BaseTickHandler;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;

/**
 * Detects day and weather transitions once per level. Transitions remain available for one base
 * tick period because NPC base ticks are staggered.
 */
public class EnvironmentChangeTracker {

  private static final long DAY_LENGTH = 24000L;
  private static final long DAY_TIME_END = 12000L;
  private static final long NEVER = -1L;
  private static final long CHANGE_WINDOW_TICKS = BaseTickHandler.BASE_TICK + 1L;
  private static final Map<ResourceKey<Level>, EnvironmentState> environmentStates =
      new ConcurrentHashMap<>();

  private EnvironmentChangeTracker() {}

  public static void handleServerTick(MinecraftServer minecraftServer) {
    if (minecraftServer == null) {
      return;
    }

    for (ServerLevel serverLevel : minecraftServer.getAllLevels()) {
      update(serverLevel);
    }
  }

  public static boolean hasDayTimeChanged(Level level) {
    EnvironmentState environmentState = getState(level);
    return environmentState != null
        && isWithinChangeWindow(level, environmentState.dayTimeChangedAt());
  }

  public static boolean hasWeatherChanged(Level level) {
    EnvironmentState environmentState = getState(level);
    return environmentState != null
        && isWithinChangeWindow(level, environmentState.weatherChangedAt());
  }

  public static void reset() {
    environmentStates.clear();
  }

  private static EnvironmentState getState(Level level) {
    return level != null ? environmentStates.get(level.dimension()) : null;
  }

  private static boolean isWithinChangeWindow(Level level, long changedAt) {
    return level != null && isWithinChangeWindow(level.getGameTime(), changedAt);
  }

  static boolean isWithinChangeWindow(long currentTick, long changedAt) {
    if (changedAt == NEVER) {
      return false;
    }

    long elapsedTicks = currentTick - changedAt;
    return elapsedTicks >= 0 && elapsedTicks < CHANGE_WINDOW_TICKS;
  }

  private static void update(ServerLevel serverLevel) {
    boolean isDayTime = serverLevel.getDayTime() % DAY_LENGTH < DAY_TIME_END;
    boolean isRaining = serverLevel.isRaining();
    boolean isThundering = serverLevel.isThundering();
    long gameTime = serverLevel.getGameTime();

    environmentStates.compute(
        serverLevel.dimension(),
        (dimension, previousState) -> {
          if (previousState == null) {
            return new EnvironmentState(isDayTime, isRaining, isThundering, NEVER, NEVER);
          }

          return new EnvironmentState(
              isDayTime,
              isRaining,
              isThundering,
              previousState.isDayTime() != isDayTime ? gameTime : previousState.dayTimeChangedAt(),
              previousState.isRaining() != isRaining || previousState.isThundering() != isThundering
                  ? gameTime
                  : previousState.weatherChangedAt());
        });
  }

  private record EnvironmentState(
      boolean isDayTime,
      boolean isRaining,
      boolean isThundering,
      long dayTimeChangedAt,
      long weatherChangedAt) {}
}
