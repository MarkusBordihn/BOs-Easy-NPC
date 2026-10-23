/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.entity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.config.StorageConfig;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class NPCSaveSchedulerTest {

  private static final long START_TIME = 1_000_000L;
  private static final float IDLE_TICK_MILLISECONDS = 5.0f;

  private int previousIntervalSeconds;

  private record ChangedNPC(String name, long lastUpdateTime) {}

  @BeforeEach
  void setUp() {
    this.previousIntervalSeconds = StorageConfig.NPC_SAVE_INTERVAL_SECONDS;
    StorageConfig.NPC_SAVE_INTERVAL_SECONDS = 30;
    NPCSaveScheduler.reset();
  }

  @AfterEach
  void tearDown() {
    StorageConfig.NPC_SAVE_INTERVAL_SECONDS = this.previousIntervalSeconds;
    NPCSaveScheduler.reset();
  }

  @Test
  @DisplayName("First tick only schedules the run, the run starts after the interval")
  void testRunStartsAfterInterval() {
    assertFalse(NPCSaveScheduler.tryStartRun(START_TIME, IDLE_TICK_MILLISECONDS));
    assertFalse(NPCSaveScheduler.tryStartRun(START_TIME + 29_999, IDLE_TICK_MILLISECONDS));
    assertTrue(NPCSaveScheduler.tryStartRun(START_TIME + 30_000, IDLE_TICK_MILLISECONDS));
    assertFalse(NPCSaveScheduler.tryStartRun(START_TIME + 30_050, IDLE_TICK_MILLISECONDS));
    assertTrue(NPCSaveScheduler.tryStartRun(START_TIME + 60_000, IDLE_TICK_MILLISECONDS));
  }

  @Test
  @DisplayName("Interval 0 never starts a run")
  void testIntervalZeroDisablesRuns() {
    StorageConfig.NPC_SAVE_INTERVAL_SECONDS = 0;

    assertFalse(NPCSaveScheduler.tryStartRun(START_TIME, IDLE_TICK_MILLISECONDS));
    assertFalse(NPCSaveScheduler.tryStartRun(START_TIME + 3_600_000, IDLE_TICK_MILLISECONDS));
  }

  @Test
  @DisplayName("High tick time postpones a due run until the load drops")
  void testHighTickTimePostponesRun() {
    float busyTickMilliseconds = NPCSaveScheduler.MAXIMUM_AVERAGE_TICK_MILLISECONDS;
    NPCSaveScheduler.tryStartRun(START_TIME, IDLE_TICK_MILLISECONDS);

    assertFalse(NPCSaveScheduler.tryStartRun(START_TIME + 30_000, busyTickMilliseconds));
    assertFalse(NPCSaveScheduler.tryStartRun(START_TIME + 30_050, busyTickMilliseconds));
    assertTrue(NPCSaveScheduler.tryStartRun(START_TIME + 30_100, IDLE_TICK_MILLISECONDS));
  }

  @Test
  @DisplayName("Selection skips changes younger than the minimum age")
  void testSelectionSkipsRecentChanges() {
    ChangedNPC oldChange = new ChangedNPC("old", START_TIME);
    ChangedNPC recentChange = new ChangedNPC("recent", START_TIME + 20_000);

    List<ChangedNPC> selected =
        NPCSaveScheduler.selectOldestChanges(
            Stream.of(recentChange, oldChange), ChangedNPC::lastUpdateTime, START_TIME + 10_000, 4);

    assertEquals(List.of(oldChange), selected);
  }

  @Test
  @DisplayName("Selection takes the oldest changes first and stops at the batch size")
  void testSelectionOrdersByAgeAndLimitsBatch() {
    List<ChangedNPC> changedNPCs =
        List.of(
            new ChangedNPC("third", START_TIME + 3),
            new ChangedNPC("first", START_TIME + 1),
            new ChangedNPC("fifth", START_TIME + 5),
            new ChangedNPC("second", START_TIME + 2),
            new ChangedNPC("fourth", START_TIME + 4));

    List<ChangedNPC> selected =
        NPCSaveScheduler.selectOldestChanges(
            changedNPCs.stream(), ChangedNPC::lastUpdateTime, START_TIME + 10, 3);

    assertEquals(
        List.of("first", "second", "third"),
        selected.stream().map(ChangedNPC::name).toList());
  }

  @Test
  @DisplayName("Advancing the time makes a recent change eligible")
  void testAdvancedTimeMakesChangeEligible() {
    ChangedNPC change = new ChangedNPC("change", START_TIME);
    long minimumAgeMilliseconds = 30_000;

    assertTrue(
        NPCSaveScheduler.selectOldestChanges(
                Stream.of(change),
                ChangedNPC::lastUpdateTime,
                START_TIME + 1_000 - minimumAgeMilliseconds,
                4)
            .isEmpty());
    assertEquals(
        List.of(change),
        NPCSaveScheduler.selectOldestChanges(
            Stream.of(change),
            ChangedNPC::lastUpdateTime,
            START_TIME + 30_000 - minimumAgeMilliseconds,
            4));
  }
}
