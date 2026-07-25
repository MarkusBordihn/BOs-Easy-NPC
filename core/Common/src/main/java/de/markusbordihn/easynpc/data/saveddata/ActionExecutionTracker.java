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

import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import com.mojang.serialization.Dynamic;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.execution.ExecutionData;
import de.markusbordihn.easynpc.data.execution.ExecutionId;
import de.markusbordihn.easynpc.data.execution.ExecutionInterval;
import de.markusbordihn.easynpc.data.execution.ExecutionTrackerData;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.datafix.DataFixTypes;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ActionExecutionTracker extends SavedData {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Identifier DATA_NAME =
      Identifier.fromNamespaceAndPath("easy_npc", "action_executions");
  private static final Codec<ActionExecutionTracker> CODEC =
      Codec.PASSTHROUGH.comapFlatMap(
          dynamic -> {
            try {
              return DataResult.success(loadFromNbt(dynamic));
            } catch (Exception e) {
              return DataResult.error(
                  () -> "Failed to load ActionExecutionTracker: " + e.getMessage());
            }
          },
          tracker -> new Dynamic<>(NbtOps.INSTANCE, saveToNbt(tracker)));

  public static final SavedDataType<ActionExecutionTracker> TYPE =
      new SavedDataType<>(
          DATA_NAME,
          ActionExecutionTracker::new,
          CODEC,
          DataFixTypes.SAVED_DATA_STRUCTURE_FEATURE_INDICES);
  private final Map<UUID, Map<ExecutionId, ExecutionData>> trackingData;

  ActionExecutionTracker() {
    this(new HashMap<>());
  }

  private ActionExecutionTracker(Map<UUID, Map<ExecutionId, ExecutionData>> trackingData) {
    this.trackingData = new HashMap<>(trackingData);
  }

  static ActionExecutionTracker loadFromNbt(Dynamic<?> dynamic) {
    Tag tag = dynamic.convert(NbtOps.INSTANCE).getValue();
    if (!(tag instanceof CompoundTag compoundTag)) {
      return new ActionExecutionTracker(new HashMap<>());
    }
    return new ActionExecutionTracker(new ExecutionTrackerData(compoundTag).trackingData());
  }

  static CompoundTag saveToNbt(ActionExecutionTracker tracker) {
    return new ExecutionTrackerData(tracker.trackingData).save();
  }

  public static ActionExecutionTracker get(ServerLevel serverLevel) {
    return serverLevel.getDataStorage().computeIfAbsent(TYPE);
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
        this.trackingData.computeIfAbsent(playerUUID, trackedPlayerUUID -> new HashMap<>());
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

    this.setDirty();
  }

  public void resetExecution(UUID playerUUID, ExecutionId executionId) {
    if (executionId == null) {
      return;
    }

    Map<ExecutionId, ExecutionData> playerData = this.trackingData.get(playerUUID);
    if (playerData != null && playerData.remove(executionId) != null) {
      log.debug("Reset execution for player {} execution {}", playerUUID, executionId);
      this.setDirty();
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
      this.setDirty();
    }
  }

  public void cleanupExpiredRecords() {
    long cutoffTime = System.currentTimeMillis() - ExecutionInterval.PER_MONTH.getMilliseconds();
    int removedCount = 0;
    for (Map<ExecutionId, ExecutionData> playerData : this.trackingData.values()) {
      int beforeSize = playerData.size();
      playerData.entrySet().removeIf(entry -> entry.getValue().lastExecutionTime() < cutoffTime);
      removedCount += beforeSize - playerData.size();
    }

    this.trackingData.entrySet().removeIf(entry -> entry.getValue().isEmpty());

    if (removedCount > 0) {
      log.debug("Cleaned up {} expired execution records", removedCount);
      this.setDirty();
    }
  }
}
