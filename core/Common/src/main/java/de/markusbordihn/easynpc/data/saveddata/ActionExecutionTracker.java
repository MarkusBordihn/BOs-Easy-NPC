/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.data.saveddata;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.execution.ExecutionData;
import de.markusbordihn.easynpc.data.execution.ExecutionId;
import de.markusbordihn.easynpc.data.execution.ExecutionInterval;
import de.markusbordihn.easynpc.data.execution.ExecutionTrackerData;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.saveddata.SavedData;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ActionExecutionTracker extends SavedData {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String DATA_NAME = "easy_npc_action_executions";

  private final Map<UUID, Map<ExecutionId, ExecutionData>> trackingData;

  public ActionExecutionTracker() {
    this.trackingData = new HashMap<>();
  }

  public ActionExecutionTracker(CompoundTag compoundTag) {
    this.trackingData = new ExecutionTrackerData(compoundTag).trackingData();
  }

  public static ActionExecutionTracker get(ServerLevel serverLevel) {
    return serverLevel
        .getDataStorage()
        .computeIfAbsent(ActionExecutionTracker::new, ActionExecutionTracker::new, DATA_NAME);
  }

  public boolean canExecute(
      UUID playerUUID, ExecutionId executionId, int limit, ExecutionInterval interval) {
    if (executionId == null) {
      return false;
    }

    if (limit == 0) {
      return true;
    }

    ExecutionData data =
        this.trackingData.getOrDefault(playerUUID, new HashMap<>()).get(executionId);
    if (data == null) {
      return true;
    }

    if (interval == ExecutionInterval.LIFETIME) {
      return data.executionCount() < limit;
    }

    if (interval.hasIntervalPassed(data.windowStartTime())) {
      return true;
    }

    return data.executionCount() < limit;
  }

  public void recordExecution(
      UUID playerUUID, ExecutionId executionId, ExecutionInterval interval) {
    if (executionId == null) {
      return;
    }

    Map<ExecutionId, ExecutionData> playerData =
        this.trackingData.computeIfAbsent(playerUUID, k -> new HashMap<>());
    ExecutionData currentData = playerData.get(executionId);
    long now = System.currentTimeMillis();

    if (currentData == null || interval.hasIntervalPassed(currentData.windowStartTime())) {
      playerData.put(executionId, new ExecutionData(1, now, now));
      log.debug(
          "Started new execution window for player {} execution {} with interval {}",
          playerUUID,
          executionId,
          interval);
    } else {
      playerData.put(
          executionId,
          new ExecutionData(currentData.executionCount() + 1, currentData.windowStartTime(), now));
      log.debug(
          "Recorded execution {} for player {} execution {}",
          currentData.executionCount() + 1,
          playerUUID,
          executionId);
    }

    setDirty();
  }

  public void resetExecution(UUID playerUUID, ExecutionId executionId) {
    if (executionId == null) {
      return;
    }

    Map<ExecutionId, ExecutionData> playerData = this.trackingData.get(playerUUID);
    if (playerData != null && playerData.remove(executionId) != null) {
      log.debug("Reset execution for player {} execution {}", playerUUID, executionId);
      setDirty();
    }
  }

  public void resetExecutionForAllPlayers(ExecutionId executionId) {
    if (executionId == null) {
      return;
    }

    int resetCount = 0;
    for (Map<ExecutionId, ExecutionData> playerData : this.trackingData.values()) {
      if (playerData.remove(executionId) != null) {
        resetCount++;
      }
    }
    if (resetCount > 0) {
      log.debug("Reset execution for {} players for execution {}", resetCount, executionId);
      setDirty();
    }
  }

  @Override
  public CompoundTag save(CompoundTag compoundTag) {
    return new ExecutionTrackerData(this.trackingData).save(compoundTag);
  }
}
