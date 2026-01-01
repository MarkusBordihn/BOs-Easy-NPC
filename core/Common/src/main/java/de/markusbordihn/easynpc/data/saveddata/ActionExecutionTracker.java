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
import de.markusbordihn.easynpc.data.execution.ExecutionInterval;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.datafix.DataFixTypes;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ActionExecutionTracker extends SavedData {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String DATA_NAME = "easy_npc_action_executions";
  private static final String DATA_PLAYERS_TAG = "Players";
  private static final String DATA_PLAYER_UUID_TAG = "PlayerUUID";
  private static final String DATA_ACTIONS_TAG = "Actions";
  private static final String DATA_ACTION_UUID_TAG = "ActionUUID";
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
  private final Map<UUID, Map<UUID, ExecutionData>> trackingData;

  private ActionExecutionTracker() {
    this(new HashMap<>());
  }

  private ActionExecutionTracker(Map<UUID, Map<UUID, ExecutionData>> trackingData) {
    this.trackingData = new HashMap<>(trackingData);
  }

  private static ActionExecutionTracker loadFromNbt(Dynamic<?> dynamic) {
    ActionExecutionTracker tracker = new ActionExecutionTracker(new HashMap<>());
    Tag tag = dynamic.convert(NbtOps.INSTANCE).getValue();

    if (!(tag instanceof CompoundTag compoundTag)) {
      return tracker;
    }

    ListTag playersTag = compoundTag.getListOrEmpty(DATA_PLAYERS_TAG);
    for (int i = 0; i < playersTag.size(); i++) {
      CompoundTag playerTag = playersTag.getCompoundOrEmpty(i);
      UUID playerUUID = CompoundTagUtils.readUUID(playerTag, DATA_PLAYER_UUID_TAG);
      if (playerUUID == null) {
        continue;
      }

      Map<UUID, ExecutionData> playerData = new HashMap<>();
      ListTag actionsTag = playerTag.getListOrEmpty(DATA_ACTIONS_TAG);
      for (int j = 0; j < actionsTag.size(); j++) {
        CompoundTag actionTag = actionsTag.getCompoundOrEmpty(j);
        UUID actionUUID = CompoundTagUtils.readUUID(actionTag, DATA_ACTION_UUID_TAG);
        if (actionUUID != null) {
          playerData.put(actionUUID, new ExecutionData(actionTag));
        }
      }

      tracker.trackingData.put(playerUUID, playerData);
    }

    return tracker;
  }

  private static CompoundTag saveToNbt(ActionExecutionTracker tracker) {
    CompoundTag compoundTag = new CompoundTag();
    ListTag playersTag = new ListTag();

    for (Map.Entry<UUID, Map<UUID, ExecutionData>> playerEntry : tracker.trackingData.entrySet()) {
      CompoundTag playerTag = new CompoundTag();
      CompoundTagUtils.writeUUID(playerTag, DATA_PLAYER_UUID_TAG, playerEntry.getKey());

      ListTag actionsTag = new ListTag();
      for (Map.Entry<UUID, ExecutionData> actionEntry : playerEntry.getValue().entrySet()) {
        CompoundTag actionTag = new CompoundTag();
        CompoundTagUtils.writeUUID(actionTag, DATA_ACTION_UUID_TAG, actionEntry.getKey());
        actionEntry.getValue().save(actionTag);
        actionsTag.add(actionTag);
      }

      playerTag.put(DATA_ACTIONS_TAG, actionsTag);
      playersTag.add(playerTag);
    }

    compoundTag.put(DATA_PLAYERS_TAG, playersTag);
    return compoundTag;
  }

  public static ActionExecutionTracker get(ServerLevel serverLevel) {
    return serverLevel.getDataStorage().computeIfAbsent(TYPE);
  }

  public boolean canExecute(
      UUID playerUUID, UUID actionUUID, int limit, ExecutionInterval interval) {
    if (limit == 0) {
      return true;
    }

    ExecutionData data =
        this.trackingData.getOrDefault(playerUUID, new HashMap<>()).get(actionUUID);
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

  public void recordExecution(UUID playerUUID, UUID actionUUID, ExecutionInterval interval) {
    Map<UUID, ExecutionData> playerData =
        this.trackingData.computeIfAbsent(playerUUID, k -> new HashMap<>());
    ExecutionData currentData = playerData.get(actionUUID);
    long now = System.currentTimeMillis();

    if (currentData == null || interval.hasIntervalPassed(currentData.windowStartTime())) {
      playerData.put(actionUUID, new ExecutionData(1, now, now));
      log.debug(
          "Started new execution window for player {} action {} with interval {}",
          playerUUID,
          actionUUID,
          interval);
    } else {
      playerData.put(
          actionUUID,
          new ExecutionData(currentData.executionCount() + 1, currentData.windowStartTime(), now));
      log.debug(
          "Recorded execution {} for player {} action {}",
          currentData.executionCount() + 1,
          playerUUID,
          actionUUID);
    }

    setDirty();
  }

  public void resetExecution(UUID playerUUID, UUID actionUUID) {
    Map<UUID, ExecutionData> playerData = this.trackingData.get(playerUUID);
    if (playerData != null && playerData.remove(actionUUID) != null) {
      log.debug("Reset execution for player {} action {}", playerUUID, actionUUID);
      setDirty();
    }
  }

  public void resetExecutionForAllPlayers(UUID actionUUID) {
    int resetCount = 0;
    for (Map<UUID, ExecutionData> playerData : this.trackingData.values()) {
      if (playerData.remove(actionUUID) != null) {
        resetCount++;
      }
    }
    if (resetCount > 0) {
      log.debug("Reset execution for {} players for action {}", resetCount, actionUUID);
      setDirty();
    }
  }

  public void cleanupExpiredRecords() {
    long cutoffTime = System.currentTimeMillis() - ExecutionInterval.PER_MONTH.getMilliseconds();
    int removedCount = 0;
    for (Map<UUID, ExecutionData> playerData : this.trackingData.values()) {
      int beforeSize = playerData.size();
      playerData.entrySet().removeIf(entry -> entry.getValue().lastExecutionTime() < cutoffTime);
      removedCount += beforeSize - playerData.size();
    }

    this.trackingData.entrySet().removeIf(entry -> entry.getValue().isEmpty());

    if (removedCount > 0) {
      log.debug("Cleaned up {} expired execution records", removedCount);
      setDirty();
    }
  }
}
