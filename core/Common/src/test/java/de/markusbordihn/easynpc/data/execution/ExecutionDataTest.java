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

import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.Test;

class ExecutionDataTest {

  @Test
  void testConstructorWithValues() {
    ExecutionData data = new ExecutionData(5, 1000L, 2000L);

    assertEquals(5, data.executionCount());
    assertEquals(1000L, data.windowStartTime());
    assertEquals(2000L, data.lastExecutionTime());
  }

  @Test
  void testSaveAndLoad() {
    ExecutionData original = new ExecutionData(3, 5000L, 6000L);
    CompoundTag tag = original.save();
    ExecutionData loaded = new ExecutionData(tag);

    assertEquals(original.executionCount(), loaded.executionCount());
    assertEquals(original.windowStartTime(), loaded.windowStartTime());
    assertEquals(original.lastExecutionTime(), loaded.lastExecutionTime());
  }

  @Test
  void testSaveToExistingTag() {
    ExecutionData data = new ExecutionData(10, 100L, 200L);
    CompoundTag tag = new CompoundTag();
    tag.putString("ExtraData", "test");
    data.save(tag);

    assertEquals(10, tag.getInt(ExecutionData.DATA_EXECUTION_COUNT_TAG));
    assertEquals(100L, tag.getLong(ExecutionData.DATA_WINDOW_START_TAG));
    assertEquals(200L, tag.getLong(ExecutionData.DATA_LAST_EXECUTION_TAG));
    assertEquals("test", tag.getString("ExtraData"));
  }

  @Test
  void testLoadFromEmptyTag() {
    CompoundTag emptyTag = new CompoundTag();
    ExecutionData data = new ExecutionData(emptyTag);

    assertEquals(0, data.executionCount());
    assertEquals(0L, data.windowStartTime());
    assertEquals(0L, data.lastExecutionTime());
  }
}
