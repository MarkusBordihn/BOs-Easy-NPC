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

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.Map;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import org.junit.jupiter.api.Test;

class ExecutionTrackerDataTest {

  @Test
  void testEmptyConstructor() {
    ExecutionTrackerData data = new ExecutionTrackerData();
    assertNotNull(data.trackingData());
    assertTrue(data.trackingData().isEmpty());
  }

  @Test
  void testSaveAndLoad() {
    ExecutionTrackerData original = new ExecutionTrackerData();
    UUID playerUUID = UUID.randomUUID();
    ExecutionId executionId = ExecutionId.dialog(UUID.randomUUID(), UUID.randomUUID());
    original
        .trackingData()
        .put(playerUUID, Map.of(executionId, new ExecutionData(5, 1000L, 2000L)));
    CompoundTag tag = original.save();
    ExecutionTrackerData loaded = new ExecutionTrackerData(tag);

    assertEquals(1, loaded.trackingData().size());
    assertTrue(loaded.trackingData().containsKey(playerUUID));
    assertTrue(loaded.trackingData().get(playerUUID).containsKey(executionId));

    ExecutionData loadedData = loaded.trackingData().get(playerUUID).get(executionId);
    assertEquals(5, loadedData.executionCount());
    assertEquals(1000L, loadedData.windowStartTime());
    assertEquals(2000L, loadedData.lastExecutionTime());
  }

  @Test
  void testSaveAndLoadPreservesType() {
    ExecutionTrackerData original = new ExecutionTrackerData();
    UUID playerUUID = UUID.randomUUID();
    UUID npcId = UUID.randomUUID();
    UUID sharedId = UUID.randomUUID();
    ExecutionId actionId = ExecutionId.action(npcId, sharedId);
    ExecutionId dialogId = ExecutionId.dialog(npcId, sharedId);
    original
        .trackingData()
        .put(
            playerUUID,
            Map.of(
                actionId, new ExecutionData(1, 100L, 200L),
                dialogId, new ExecutionData(2, 300L, 400L)));
    ExecutionTrackerData loaded = new ExecutionTrackerData(original.save());

    Map<ExecutionId, ExecutionData> playerData = loaded.trackingData().get(playerUUID);
    assertEquals(2, playerData.size());
    assertEquals(1, playerData.get(actionId).executionCount());
    assertEquals(2, playerData.get(dialogId).executionCount());
  }

  @Test
  void testSaveMultiplePlayers() {
    ExecutionTrackerData data = new ExecutionTrackerData();
    UUID player1 = UUID.randomUUID();
    UUID player2 = UUID.randomUUID();
    ExecutionId target1 = ExecutionId.action(UUID.randomUUID(), UUID.randomUUID());
    ExecutionId target2 =
        ExecutionId.dialogButton(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID());
    data.trackingData().put(player1, Map.of(target1, new ExecutionData(1, 100L, 200L)));
    data.trackingData().put(player2, Map.of(target2, new ExecutionData(2, 300L, 400L)));
    CompoundTag tag = data.save();
    ExecutionTrackerData loaded = new ExecutionTrackerData(tag);

    assertEquals(2, loaded.trackingData().size());
    assertTrue(loaded.trackingData().containsKey(player1));
    assertTrue(loaded.trackingData().containsKey(player2));
  }

  @Test
  void testLoadFromEmptyTag() {
    CompoundTag emptyTag = new CompoundTag();
    ExecutionTrackerData data = new ExecutionTrackerData(emptyTag);

    assertNotNull(data.trackingData());
    assertTrue(data.trackingData().isEmpty());
  }

  @Test
  void testLoadSkipsEntriesWithoutType() {
    CompoundTag playerTag = new CompoundTag();
    CompoundTagUtils.writeUUID(
        playerTag, ExecutionTrackerData.DATA_PLAYER_UUID_TAG, UUID.randomUUID());

    CompoundTag legacyExecutionTag = new CompoundTag();
    CompoundTagUtils.writeUUID(
        legacyExecutionTag, ExecutionTrackerData.DATA_EXECUTION_ID_TAG, UUID.randomUUID());
    new ExecutionData(1, 100L, 200L).save(legacyExecutionTag);
    ListTag executionsTag = new ListTag();
    executionsTag.add(legacyExecutionTag);
    playerTag.put(ExecutionTrackerData.DATA_EXECUTIONS_TAG, executionsTag);

    ListTag playersTag = new ListTag();
    playersTag.add(playerTag);
    CompoundTag tag = new CompoundTag();
    tag.put(ExecutionTrackerData.DATA_PLAYERS_TAG, playersTag);

    ExecutionTrackerData loaded = new ExecutionTrackerData(tag);
    assertTrue(loaded.trackingData().isEmpty());
  }

  @Test
  void testSaveToExistingTag() {
    ExecutionTrackerData data = new ExecutionTrackerData();
    UUID playerUUID = UUID.randomUUID();
    ExecutionId executionId = ExecutionId.action(UUID.randomUUID(), UUID.randomUUID());
    data.trackingData().put(playerUUID, Map.of(executionId, new ExecutionData(3, 500L, 600L)));
    CompoundTag tag = new CompoundTag();
    tag.putString("ExtraField", "test");
    data.save(tag);

    assertTrue(tag.contains(ExecutionTrackerData.DATA_PLAYERS_TAG));
    assertEquals("test", tag.getString("ExtraField").orElse(""));
  }
}
