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

package de.markusbordihn.easynpc.gametest;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import de.markusbordihn.easynpc.Constants;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.gametest.framework.GameTestInfo;
import net.minecraft.gametest.framework.GameTestListener;
import net.minecraft.gametest.framework.TestFunction;
import net.minecraft.world.level.block.Rotation;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class GameTestHelpersTest {

  private final List<GameTestListener> listeners = new ArrayList<>();

  private final GameTestInfo testInfo =
      new GameTestInfo(
          new TestFunction(
              "cleanup",
              "cleanup.test",
              Constants.MOD_ID + ":gametest.3x3x3",
              100,
              0L,
              true,
              helper -> {}),
          Rotation.NONE,
          null) {
        @Override
        public void addListener(GameTestListener listener) {
          GameTestHelpersTest.this.listeners.add(listener);
        }
      };

  private final GameTestHelper helper = new GameTestHelper(this.testInfo);

  @Test
  @DisplayName("Runs the cleanup once the test has passed")
  void runsCleanupAfterPassedTest() {
    AtomicInteger cleanupRuns = new AtomicInteger();
    GameTestHelpers.addCleanup(this.helper, cleanupRuns::incrementAndGet);

    this.listeners.forEach(listener -> listener.testPassed(this.testInfo));

    assertEquals(1, cleanupRuns.get());
  }

  @Test
  @DisplayName("Runs the cleanup once the test has failed")
  void runsCleanupAfterFailedTest() {
    AtomicInteger cleanupRuns = new AtomicInteger();
    GameTestHelpers.addCleanup(this.helper, cleanupRuns::incrementAndGet);

    this.listeners.forEach(listener -> listener.testFailed(this.testInfo));

    assertEquals(1, cleanupRuns.get());
  }

  @Test
  @DisplayName("Runs the remaining cleanups when one of them fails")
  void continuesAfterFailingCleanup() {
    AtomicInteger cleanupRuns = new AtomicInteger();
    GameTestHelpers.addCleanup(
        this.helper,
        () -> {
          throw new IllegalStateException("cleanup failed");
        });
    GameTestHelpers.addCleanup(this.helper, cleanupRuns::incrementAndGet);

    assertDoesNotThrow(
        () -> this.listeners.forEach(listener -> listener.testPassed(this.testInfo)));
    assertEquals(1, cleanupRuns.get());
  }
}
