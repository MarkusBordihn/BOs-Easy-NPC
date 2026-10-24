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

import static de.markusbordihn.easynpc.gametest.GameTestEntityTypes.humanoid;

import net.minecraft.gametest.framework.GameTestHelper;

public final class NPCSaveSchedulerTest {

  private NPCSaveSchedulerTest() {}

  public static void testChangeIsSavedAfterMinimumAge(GameTestHelper helper) {
    NPCSaveSchedulerTestHelper.assertChangeIsSavedAfterMinimumAge(helper, humanoid());
    helper.succeed();
  }

  public static void testUnchangedNPCIsNotSaved(GameTestHelper helper) {
    NPCSaveSchedulerTestHelper.assertUnchangedNPCIsNotSaved(helper, humanoid());
    helper.succeed();
  }

  public static void testBatchSizeLimitsSavesPerRun(GameTestHelper helper) {
    NPCSaveSchedulerTestHelper.assertBatchSizeLimitsSavesPerRun(helper, humanoid());
    helper.succeed();
  }

  public static void testUnloadDuringPendingWriteKeepsNewestData(GameTestHelper helper) {
    NPCSaveSchedulerTestHelper.assertUnloadDuringPendingWriteKeepsNewestData(helper, humanoid());
    helper.succeed();
  }

  public static void testAutosaveWritesChangedNPCFile(GameTestHelper helper) {
    NPCSaveSchedulerTestHelper.assertAutosaveWritesChangedNPCFile(helper, humanoid());
    helper.succeed();
  }

  public static void testDisabledIntervalKeepsChangeUntilUnload(GameTestHelper helper) {
    NPCSaveSchedulerTestHelper.assertDisabledIntervalKeepsChangeUntilUnload(helper, humanoid());
    helper.succeed();
  }

  public static void testMissingIndexHashIsAdoptedWithoutWrite(GameTestHelper helper) {
    NPCSaveSchedulerTestHelper.assertMissingIndexHashIsAdoptedWithoutWrite(helper, humanoid());
    helper.succeed();
  }

  public static void testFiredSpawnActionIsSaved(GameTestHelper helper) {
    NPCSaveSchedulerTestHelper.assertFiredSpawnActionIsSaved(helper, humanoid());
    helper.succeed();
  }
}
