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

package de.markusbordihn.easynpc.io;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Duration;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class BackgroundFileWriterTest {

  private static final Duration WRITER_TIMEOUT = Duration.ofSeconds(10);

  @Test
  @DisplayName("Tasks run in the order they were submitted")
  void testTasksRunInOrder() {
    List<Integer> executionOrder = new CopyOnWriteArrayList<>();
    for (int i = 0; i < 100; i++) {
      int taskNumber = i;
      BackgroundFileWriter.submit(() -> executionOrder.add(taskNumber));
    }

    assertTrue(BackgroundFileWriter.awaitPendingWrites(WRITER_TIMEOUT));
    assertEquals(100, executionOrder.size());
    for (int i = 0; i < 100; i++) {
      assertEquals(i, executionOrder.get(i));
    }
  }

  @Test
  @DisplayName("Waiting returns after all pending tasks have run")
  void testAwaitPendingWrites() {
    CountDownLatch writerBlock = new CountDownLatch(1);
    AtomicBoolean taskDone = new AtomicBoolean();
    BackgroundFileWriter.submit(() -> awaitQuietly(writerBlock));
    BackgroundFileWriter.submit(() -> taskDone.set(true));
    assertTrue(BackgroundFileWriter.getPendingWriteCount() >= 2);

    writerBlock.countDown();
    assertTrue(BackgroundFileWriter.awaitPendingWrites(WRITER_TIMEOUT));
    assertTrue(taskDone.get());
    assertEquals(0, BackgroundFileWriter.getPendingWriteCount());
  }

  @Test
  @DisplayName("A failing task does not stop the writer")
  void testFailingTaskKeepsWriterRunning() {
    AtomicBoolean taskDone = new AtomicBoolean();
    BackgroundFileWriter.submit(
        () -> {
          throw new IllegalStateException("Expected test failure");
        });
    BackgroundFileWriter.submit(() -> taskDone.set(true));

    assertTrue(BackgroundFileWriter.awaitPendingWrites(WRITER_TIMEOUT));
    assertTrue(taskDone.get());
  }

  @Test
  @DisplayName("Waiting times out while a task is still running")
  void testAwaitPendingWritesTimeout() {
    CountDownLatch writerBlock = new CountDownLatch(1);
    BackgroundFileWriter.submit(() -> awaitQuietly(writerBlock));
    try {
      assertFalse(BackgroundFileWriter.awaitPendingWrites(Duration.ofMillis(50)));
    } finally {
      writerBlock.countDown();
    }

    assertTrue(BackgroundFileWriter.awaitPendingWrites(WRITER_TIMEOUT));
  }

  @Test
  @DisplayName("Waiting from the writer thread returns instead of blocking it")
  void testAwaitPendingWritesFromWriterThread() {
    AtomicBoolean awaitResult = new AtomicBoolean(true);
    BackgroundFileWriter.submit(
        () -> awaitResult.set(BackgroundFileWriter.awaitPendingWrites(WRITER_TIMEOUT)));

    assertTrue(BackgroundFileWriter.awaitPendingWrites(WRITER_TIMEOUT));
    assertFalse(awaitResult.get());
  }

  private static void awaitQuietly(CountDownLatch latch) {
    try {
      latch.await(WRITER_TIMEOUT.toMillis(), TimeUnit.MILLISECONDS);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
    }
  }
}
