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
import net.minecraft.SharedConstants;
import net.minecraft.core.Holder;
import net.minecraft.core.registries.Registries;
import net.minecraft.gametest.framework.FunctionGameTestInstance;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.gametest.framework.GameTestInfo;
import net.minecraft.gametest.framework.GameTestInstance;
import net.minecraft.gametest.framework.GameTestListener;
import net.minecraft.gametest.framework.RetryOptions;
import net.minecraft.gametest.framework.TestData;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.level.block.Rotation;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class GameTestHelpersTest {

  private static final Identifier CLEANUP_TEST_ID =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "cleanup_test");

  private final List<GameTestListener> listeners = new ArrayList<>();

  private final GameTestInfo testInfo =
      new GameTestInfo(createCleanupTest(), Rotation.NONE, null, RetryOptions.noRetries()) {
        @Override
        public void addListener(GameTestListener listener) {
          GameTestHelpersTest.this.listeners.add(listener);
        }
      };

  private final GameTestHelper helper = new GameTestHelper(this.testInfo);

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static Holder.Reference<GameTestInstance> createCleanupTest() {
    return new StandAloneTestReference(
        new FunctionGameTestInstance(
            ResourceKey.create(Registries.TEST_FUNCTION, CLEANUP_TEST_ID),
            new TestData<>(
                null,
                Identifier.fromNamespaceAndPath(Constants.MOD_ID, "gametest.3x3x3"),
                100,
                0,
                true)));
  }

  @Test
  @DisplayName("Runs the cleanup once the test has passed")
  void runsCleanupAfterPassedTest() {
    AtomicInteger cleanupRuns = new AtomicInteger();
    GameTestHelpers.addCleanup(this.helper, cleanupRuns::incrementAndGet);

    this.listeners.forEach(listener -> listener.testPassed(this.testInfo, null));

    assertEquals(1, cleanupRuns.get());
  }

  @Test
  @DisplayName("Runs the cleanup once the test has failed")
  void runsCleanupAfterFailedTest() {
    AtomicInteger cleanupRuns = new AtomicInteger();
    GameTestHelpers.addCleanup(this.helper, cleanupRuns::incrementAndGet);

    this.listeners.forEach(listener -> listener.testFailed(this.testInfo, null));

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
        () -> this.listeners.forEach(listener -> listener.testPassed(this.testInfo, null)));
    assertEquals(1, cleanupRuns.get());
  }

  // Holder.Reference binds a value only through a registry or its protected constructor.
  private static final class StandAloneTestReference extends Holder.Reference<GameTestInstance> {

    private StandAloneTestReference(GameTestInstance testInstance) {
      super(
          Type.STAND_ALONE,
          null,
          ResourceKey.create(Registries.TEST_INSTANCE, CLEANUP_TEST_ID),
          testInstance);
    }
  }
}
