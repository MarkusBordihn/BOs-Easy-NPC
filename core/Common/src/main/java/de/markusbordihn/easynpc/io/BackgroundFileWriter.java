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

import de.markusbordihn.easynpc.Constants;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class BackgroundFileWriter {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String THREAD_NAME = "EasyNPC-File-Writer";
  private static final ExecutorService singleThreadExecutor =
      Executors.newSingleThreadExecutor(
          runnable -> {
            Thread thread = new Thread(runnable, THREAD_NAME);
            thread.setDaemon(true);
            return thread;
          });
  private static final AtomicInteger pendingWrites = new AtomicInteger();

  private BackgroundFileWriter() {}

  public static void submit(Runnable writeTask) {
    pendingWrites.incrementAndGet();
    singleThreadExecutor.execute(
        () -> {
          try {
            writeTask.run();
          } catch (RuntimeException e) {
            log.error("Background file write failed:", e);
          } finally {
            pendingWrites.decrementAndGet();
          }
        });
  }

  public static boolean awaitPendingWrites(Duration timeout) {
    if (isWriterThread()) {
      return false;
    }

    CompletableFuture<Void> barrier = new CompletableFuture<>();
    singleThreadExecutor.execute(() -> barrier.complete(null));
    try {
      barrier.get(timeout.toMillis(), TimeUnit.MILLISECONDS);
      return true;
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      return false;
    } catch (ExecutionException | TimeoutException e) {
      return false;
    }
  }

  public static int getPendingWriteCount() {
    return pendingWrites.get();
  }

  private static boolean isWriterThread() {
    return THREAD_NAME.equals(Thread.currentThread().getName());
  }
}
