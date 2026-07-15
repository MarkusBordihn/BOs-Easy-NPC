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

package de.markusbordihn.easynpc.data.execution;

import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;

public record ExecutionTrackerData(Map<UUID, Map<ExecutionId, ExecutionData>> trackingData) {

  public static final String DATA_PLAYERS_TAG = "Players";
  public static final String DATA_PLAYER_UUID_TAG = "PlayerUUID";
  public static final String DATA_EXECUTIONS_TAG = "Executions";
  public static final String DATA_EXECUTION_ID_TAG = "Id";
  public static final String DATA_EXECUTION_TYPE_TAG = "Type";

  public ExecutionTrackerData() {
    this(new HashMap<>());
  }

  public ExecutionTrackerData(CompoundTag tag) {
    this(loadFromTag(tag));
  }

  private static Map<UUID, Map<ExecutionId, ExecutionData>> loadFromTag(CompoundTag compoundTag) {
    Map<UUID, Map<ExecutionId, ExecutionData>> data = new HashMap<>();
    ListTag playersTag = compoundTag.getListOrEmpty(DATA_PLAYERS_TAG);

    for (int i = 0; i < playersTag.size(); i++) {
      CompoundTag playerTag = playersTag.getCompoundOrEmpty(i);
      UUID playerUUID = CompoundTagUtils.readUUID(playerTag, DATA_PLAYER_UUID_TAG);
      if (playerUUID == null) {
        continue;
      }

      Map<ExecutionId, ExecutionData> playerData = new HashMap<>();
      ListTag executionsTag = playerTag.getListOrEmpty(DATA_EXECUTIONS_TAG);
      for (int j = 0; j < executionsTag.size(); j++) {
        CompoundTag executionTag = executionsTag.getCompoundOrEmpty(j);
        ExecutionType type =
            ExecutionType.get(executionTag.getString(DATA_EXECUTION_TYPE_TAG).orElse(""));
        UUID executionUUID = CompoundTagUtils.readUUID(executionTag, DATA_EXECUTION_ID_TAG);
        if (type == null || executionUUID == null) {
          continue;
        }
        playerData.put(new ExecutionId(type, executionUUID), new ExecutionData(executionTag));
      }

      if (!playerData.isEmpty()) {
        data.put(playerUUID, playerData);
      }
    }

    return data;
  }

  public CompoundTag save(CompoundTag compoundTag) {
    ListTag playersTag = new ListTag();

    for (Map.Entry<UUID, Map<ExecutionId, ExecutionData>> playerEntry :
        this.trackingData.entrySet()) {
      CompoundTag playerTag = new CompoundTag();
      CompoundTagUtils.writeUUID(playerTag, DATA_PLAYER_UUID_TAG, playerEntry.getKey());

      ListTag executionsTag = new ListTag();
      for (Map.Entry<ExecutionId, ExecutionData> executionEntry :
          playerEntry.getValue().entrySet()) {
        CompoundTag executionTag = new CompoundTag();
        executionTag.putString(DATA_EXECUTION_TYPE_TAG, executionEntry.getKey().type().name());
        CompoundTagUtils.writeUUID(
            executionTag, DATA_EXECUTION_ID_TAG, executionEntry.getKey().value());
        executionEntry.getValue().save(executionTag);
        executionsTag.add(executionTag);
      }

      playerTag.put(DATA_EXECUTIONS_TAG, executionsTag);
      playersTag.add(playerTag);
    }

    compoundTag.put(DATA_PLAYERS_TAG, playersTag);
    return compoundTag;
  }

  public CompoundTag save() {
    return save(new CompoundTag());
  }
}
