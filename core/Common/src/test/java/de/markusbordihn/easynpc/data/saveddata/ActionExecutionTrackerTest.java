/*
 * Copyright 2025 Markus Bordihn
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

import static org.junit.jupiter.api.Assertions.*;

import com.mojang.serialization.Dynamic;
import de.markusbordihn.easynpc.data.execution.ExecutionInterval;
import java.util.UUID;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import org.junit.jupiter.api.Test;

class ActionExecutionTrackerTest {

  @Test
  void testPersistedActionIdUsesSameTrackerKey() {
    ActionExecutionTracker tracker = new ActionExecutionTracker();
    UUID playerId = UUID.randomUUID();
    UUID persistedActionId = UUID.randomUUID();
    UUID newActionId = UUID.randomUUID();

    assertTrue(tracker.canExecute(playerId, persistedActionId, 1, ExecutionInterval.LIFETIME));

    tracker.recordExecution(playerId, persistedActionId, ExecutionInterval.LIFETIME);

    assertFalse(tracker.canExecute(playerId, persistedActionId, 1, ExecutionInterval.LIFETIME));
    assertTrue(tracker.canExecute(playerId, newActionId, 1, ExecutionInterval.LIFETIME));

    Tag savedTag = ActionExecutionTracker.saveToNbt(tracker);
    Dynamic<Tag> dynamic = new Dynamic<>(NbtOps.INSTANCE, savedTag);
    ActionExecutionTracker reloadedTracker = ActionExecutionTracker.loadFromNbt(dynamic);

    assertFalse(
        reloadedTracker.canExecute(playerId, persistedActionId, 1, ExecutionInterval.LIFETIME));
    assertTrue(reloadedTracker.canExecute(playerId, newActionId, 1, ExecutionInterval.LIFETIME));
  }

  @Test
  void testResetExecutionUsesExactTrackerKey() {
    ActionExecutionTracker tracker = new ActionExecutionTracker();
    UUID playerId = UUID.randomUUID();
    UUID actionId = UUID.randomUUID();
    UUID dialogId = UUID.randomUUID();

    tracker.recordExecution(playerId, actionId, ExecutionInterval.LIFETIME);
    tracker.recordExecution(playerId, dialogId, ExecutionInterval.LIFETIME);

    tracker.resetExecution(playerId, actionId);

    assertTrue(tracker.canExecute(playerId, actionId, 1, ExecutionInterval.LIFETIME));
    assertFalse(tracker.canExecute(playerId, dialogId, 1, ExecutionInterval.LIFETIME));
  }
}
