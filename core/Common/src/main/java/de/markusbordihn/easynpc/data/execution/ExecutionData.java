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

import net.minecraft.nbt.CompoundTag;

public record ExecutionData(int executionCount, long windowStartTime, long lastExecutionTime) {

  public static final String DATA_EXECUTION_COUNT_TAG = "Count";
  public static final String DATA_WINDOW_START_TAG = "WindowStart";
  public static final String DATA_LAST_EXECUTION_TAG = "LastExec";

  public ExecutionData(CompoundTag compoundTag) {
    this(
        compoundTag.getInt(DATA_EXECUTION_COUNT_TAG),
        compoundTag.getLong(DATA_WINDOW_START_TAG),
        compoundTag.getLong(DATA_LAST_EXECUTION_TAG));
  }

  public CompoundTag save(CompoundTag compoundTag) {
    compoundTag.putInt(DATA_EXECUTION_COUNT_TAG, this.executionCount);
    compoundTag.putLong(DATA_WINDOW_START_TAG, this.windowStartTime);
    compoundTag.putLong(DATA_LAST_EXECUTION_TAG, this.lastExecutionTime);
    return compoundTag;
  }

  public CompoundTag save() {
    return save(new CompoundTag());
  }
}
