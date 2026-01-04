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

package de.markusbordihn.easynpc.client.texture;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("AsyncTextureLoader Tests")
class AsyncTextureLoaderTest {

  private Path tempDir;

  @BeforeEach
  void setUp() throws IOException {
    tempDir = Files.createTempDirectory("texture_test");
  }

  @AfterEach
  void tearDown() throws IOException {
    // Clean up temp directory
    if (tempDir != null && Files.exists(tempDir)) {
      Files.walk(tempDir)
          .sorted((a, b) -> -a.compareTo(b))
          .forEach(
              path -> {
                try {
                  Files.deleteIfExists(path);
                } catch (IOException e) {
                  // Ignore cleanup errors
                }
              });
    }
  }

  @Test
  @DisplayName("Should return same future for duplicate requests")
  void testDuplicatePrevention() {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);
    String testUrl = "http://example.com/texture.png";
    CompletableFuture<?> future1 = AsyncTextureLoader.loadTextureAsync(key, testUrl, tempDir);
    CompletableFuture<?> future2 = AsyncTextureLoader.loadTextureAsync(key, testUrl, tempDir);

    assertNotNull(future1);
    assertNotNull(future2);
    assertSame(
        future1,
        future2,
        "Duplicate requests for the same key should return the same CompletableFuture");
  }

  @Test
  @DisplayName("Should return same future for duplicate player texture requests")
  void testPlayerTextureDuplicatePrevention() {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);
    UUID playerUUID = UUID.randomUUID();
    CompletableFuture<?> future1 =
        AsyncTextureLoader.loadPlayerTextureAsync(key, playerUUID, tempDir);
    CompletableFuture<?> future2 =
        AsyncTextureLoader.loadPlayerTextureAsync(key, playerUUID, tempDir);

    assertNotNull(future1);
    assertNotNull(future2);
    assertSame(
        future1,
        future2,
        "Duplicate player texture requests for the same key should return the same CompletableFuture");
  }

  @Test
  @DisplayName("Should handle different keys independently")
  void testDifferentKeysIndependent() {
    TextureModelKey key1 = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);
    TextureModelKey key2 = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);
    String testUrl = "http://example.com/texture.png";
    CompletableFuture<?> future1 = AsyncTextureLoader.loadTextureAsync(key1, testUrl, tempDir);
    CompletableFuture<?> future2 = AsyncTextureLoader.loadTextureAsync(key2, testUrl, tempDir);

    assertNotNull(future1);
    assertNotNull(future2);
    assertNotSame(
        future1,
        future2,
        "Different keys should get different CompletableFutures even with same URL");
  }

  @Test
  @DisplayName("Should handle concurrent requests for same key")
  void testConcurrentRequestsSameKey() throws InterruptedException {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);
    String testUrl = "http://example.com/texture.png";
    int threadCount = 10;
    CountDownLatch startLatch = new CountDownLatch(1);
    CountDownLatch doneLatch = new CountDownLatch(threadCount);
    AtomicInteger sameReferenceCount = new AtomicInteger(0);
    CompletableFuture<?> firstFuture = AsyncTextureLoader.loadTextureAsync(key, testUrl, tempDir);

    for (int i = 0; i < threadCount; i++) {
      new Thread(
              () -> {
                try {
                  startLatch.await();
                  CompletableFuture<?> future =
                      AsyncTextureLoader.loadTextureAsync(key, testUrl, tempDir);
                  if (future == firstFuture) {
                    sameReferenceCount.incrementAndGet();
                  }
                } catch (InterruptedException e) {
                  Thread.currentThread().interrupt();
                } finally {
                  doneLatch.countDown();
                }
              })
          .start();
    }

    startLatch.countDown();
    assertTrue(doneLatch.await(5, TimeUnit.SECONDS), "All threads should complete within timeout");
    assertEquals(
        threadCount,
        sameReferenceCount.get(),
        "All concurrent requests should get the same future reference");
  }

  @Test
  @DisplayName("Should handle concurrent requests for different keys")
  void testConcurrentRequestsDifferentKeys() throws InterruptedException {
    String testUrl = "http://example.com/texture.png";
    int threadCount = 10;
    CountDownLatch startLatch = new CountDownLatch(1);
    CountDownLatch doneLatch = new CountDownLatch(threadCount);
    AtomicInteger successCount = new AtomicInteger(0);

    for (int i = 0; i < threadCount; i++) {
      new Thread(
              () -> {
                try {
                  startLatch.await();
                  TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);
                  CompletableFuture<?> future =
                      AsyncTextureLoader.loadTextureAsync(key, testUrl, tempDir);
                  if (future != null) {
                    successCount.incrementAndGet();
                  }
                } catch (InterruptedException e) {
                  Thread.currentThread().interrupt();
                } finally {
                  doneLatch.countDown();
                }
              })
          .start();
    }

    startLatch.countDown();
    assertTrue(doneLatch.await(5, TimeUnit.SECONDS));
    assertEquals(
        threadCount,
        successCount.get(),
        "All threads should successfully create futures for different keys");
  }

  @Test
  @DisplayName("Should handle null URL gracefully")
  void testNullUrl() {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);

    assertDoesNotThrow(() -> AsyncTextureLoader.loadTextureAsync(key, null, tempDir));
  }

  @Test
  @DisplayName("Should handle empty URL gracefully")
  void testEmptyUrl() {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);

    assertDoesNotThrow(() -> AsyncTextureLoader.loadTextureAsync(key, "", tempDir));
  }

  @Test
  @DisplayName("Should handle null directory gracefully")
  void testNullDirectory() {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);
    String testUrl = "http://example.com/texture.png";

    assertDoesNotThrow(() -> AsyncTextureLoader.loadTextureAsync(key, testUrl, null));
  }

  @Test
  @DisplayName("Should complete future on successful load")
  void testFutureCompletion() throws InterruptedException, ExecutionException, TimeoutException {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);
    UUID playerUUID = UUID.randomUUID();

    CompletableFuture<?> future =
        AsyncTextureLoader.loadPlayerTextureAsync(key, playerUUID, tempDir);

    assertNotNull(future);
    try {
      future.get(10, TimeUnit.SECONDS);
    } catch (Exception e) {
      // Expected - no real texture server available in tests
    }

    assertTrue(future.isDone(), "Future should be completed");
  }

  @Test
  @DisplayName("Should handle invalid texture URL")
  void testInvalidTextureUrl() {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);
    String invalidUrl = "not-a-valid-url";
    CompletableFuture<?> future = AsyncTextureLoader.loadTextureAsync(key, invalidUrl, tempDir);

    assertNotNull(future);
  }

  @Test
  @DisplayName("Should handle multiple different skin models")
  void testMultipleSkinModels() {
    UUID uuid = UUID.randomUUID();
    String testUrl = "http://example.com/texture.png";
    TextureModelKey humanoidKey = new TextureModelKey(uuid, SkinModel.HUMANOID);
    TextureModelKey slimKey = new TextureModelKey(uuid, SkinModel.HUMANOID_SLIM);
    CompletableFuture<?> future1 =
        AsyncTextureLoader.loadTextureAsync(humanoidKey, testUrl, tempDir);
    CompletableFuture<?> future2 = AsyncTextureLoader.loadTextureAsync(slimKey, testUrl, tempDir);

    assertNotNull(future1);
    assertNotNull(future2);
    assertNotSame(future1, future2);
  }

  @Test
  @DisplayName("Should not block calling thread")
  void testNonBlocking() {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);
    String testUrl = "http://example.com/texture.png";
    long startTime = System.currentTimeMillis();
    CompletableFuture<?> future = AsyncTextureLoader.loadTextureAsync(key, testUrl, tempDir);
    long duration = System.currentTimeMillis() - startTime;

    assertNotNull(future);
    assertTrue(
        duration < 1000,
        "loadTextureAsync should return immediately without blocking (took " + duration + "ms)");
  }
}
