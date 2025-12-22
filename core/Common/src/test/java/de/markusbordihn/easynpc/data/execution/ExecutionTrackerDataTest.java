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

import java.util.Map;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
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
    UUID targetUUID = UUID.randomUUID();
    original.trackingData().put(playerUUID, Map.of(targetUUID, new ExecutionData(5, 1000L, 2000L)));
    CompoundTag tag = original.save();
    ExecutionTrackerData loaded = new ExecutionTrackerData(tag);

    assertEquals(1, loaded.trackingData().size());
    assertTrue(loaded.trackingData().containsKey(playerUUID));
    assertTrue(loaded.trackingData().get(playerUUID).containsKey(targetUUID));

    ExecutionData loadedData = loaded.trackingData().get(playerUUID).get(targetUUID);
    assertEquals(5, loadedData.executionCount());
    assertEquals(1000L, loadedData.windowStartTime());
    assertEquals(2000L, loadedData.lastExecutionTime());
  }

  @Test
  void testSaveMultiplePlayers() {
    ExecutionTrackerData data = new ExecutionTrackerData();
    UUID player1 = UUID.randomUUID();
    UUID player2 = UUID.randomUUID();
    UUID target1 = UUID.randomUUID();
    UUID target2 = UUID.randomUUID();
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
  void testSaveToExistingTag() {
    ExecutionTrackerData data = new ExecutionTrackerData();
    UUID playerUUID = UUID.randomUUID();
    UUID targetUUID = UUID.randomUUID();
    data.trackingData().put(playerUUID, Map.of(targetUUID, new ExecutionData(3, 500L, 600L)));
    CompoundTag tag = new CompoundTag();
    tag.putString("ExtraField", "test");
    data.save(tag);

    assertTrue(tag.contains(ExecutionTrackerData.DATA_PLAYERS_TAG));
    assertEquals("test", tag.getString("ExtraField"));
  }
}
