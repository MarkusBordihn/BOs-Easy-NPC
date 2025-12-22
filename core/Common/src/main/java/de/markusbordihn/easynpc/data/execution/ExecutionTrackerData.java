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

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;

public record ExecutionTrackerData(Map<UUID, Map<UUID, ExecutionData>> trackingData) {

  public static final String DATA_PLAYERS_TAG = "Players";
  public static final String DATA_PLAYER_UUID_TAG = "PlayerUUID";
  public static final String DATA_TARGETS_TAG = "Targets";
  public static final String DATA_TARGET_UUID_TAG = "TargetUUID";

  public ExecutionTrackerData() {
    this(new HashMap<>());
  }

  public ExecutionTrackerData(CompoundTag tag) {
    this(loadFromTag(tag));
  }

  private static Map<UUID, Map<UUID, ExecutionData>> loadFromTag(CompoundTag compoundTag) {
    Map<UUID, Map<UUID, ExecutionData>> data = new HashMap<>();
    ListTag playersTag = compoundTag.getList(DATA_PLAYERS_TAG, Tag.TAG_COMPOUND);

    for (int i = 0; i < playersTag.size(); i++) {
      CompoundTag playerTag = playersTag.getCompound(i);
      UUID playerUUID = playerTag.getUUID(DATA_PLAYER_UUID_TAG);
      Map<UUID, ExecutionData> playerData = new HashMap<>();

      ListTag targetsTag = playerTag.getList(DATA_TARGETS_TAG, Tag.TAG_COMPOUND);
      for (int j = 0; j < targetsTag.size(); j++) {
        CompoundTag targetTag = targetsTag.getCompound(j);
        playerData.put(targetTag.getUUID(DATA_TARGET_UUID_TAG), new ExecutionData(targetTag));
      }

      data.put(playerUUID, playerData);
    }

    return data;
  }

  public CompoundTag save(CompoundTag compoundTag) {
    ListTag playersTag = new ListTag();

    for (Map.Entry<UUID, Map<UUID, ExecutionData>> playerEntry : this.trackingData.entrySet()) {
      CompoundTag playerTag = new CompoundTag();
      playerTag.putUUID(DATA_PLAYER_UUID_TAG, playerEntry.getKey());

      ListTag targetsTag = new ListTag();
      for (Map.Entry<UUID, ExecutionData> targetEntry : playerEntry.getValue().entrySet()) {
        CompoundTag targetTag = new CompoundTag();
        targetTag.putUUID(DATA_TARGET_UUID_TAG, targetEntry.getKey());
        targetEntry.getValue().save(targetTag);
        targetsTag.add(targetTag);
      }

      playerTag.put(DATA_TARGETS_TAG, targetsTag);
      playersTag.add(playerTag);
    }

    compoundTag.put(DATA_PLAYERS_TAG, playersTag);
    return compoundTag;
  }

  public CompoundTag save() {
    return save(new CompoundTag());
  }
}
