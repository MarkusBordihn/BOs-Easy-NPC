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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.utils.PlayersUtils;
import java.nio.file.Path;
import java.util.Map;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class AsyncTextureLoader {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Async Texture Loader]";
  private static final ExecutorService textureLoadExecutor =
      Executors.newFixedThreadPool(
          2,
          runnable -> {
            Thread thread = new Thread(runnable);
            thread.setName("EasyNPC-Texture-Loader");
            thread.setPriority(Thread.MIN_PRIORITY);
            thread.setDaemon(true);
            return thread;
          });
  private static final ScheduledExecutorService rateLimitScheduler =
      Executors.newSingleThreadScheduledExecutor(
          runnable -> {
            Thread thread = new Thread(runnable);
            thread.setName("EasyNPC-Texture-RateLimiter");
            thread.setPriority(Thread.MIN_PRIORITY);
            thread.setDaemon(true);
            return thread;
          });
  private static final Queue<TextureLoadRequest> downloadQueue = new ConcurrentLinkedQueue<>();
  private static final Map<TextureModelKey, CompletableFuture<ResourceLocation>> pendingLoads =
      new ConcurrentHashMap<>();
  private static volatile long lastDownloadTime = 0;

  static {
    rateLimitScheduler.scheduleAtFixedRate(
        AsyncTextureLoader::processQueue, 0, 100, TimeUnit.MILLISECONDS);

    // Register shutdown hook to clean up executor services
    Runtime.getRuntime()
        .addShutdownHook(
            new Thread(
                () -> {
                  log.debug("{} Shutting down texture loader executor services...", LOG_PREFIX);
                  shutdown();
                },
                "EasyNPC-Texture-Loader-Shutdown"));
  }

  private AsyncTextureLoader() {}

  public static CompletableFuture<ResourceLocation> loadTextureAsync(
      TextureModelKey key, String url, Path targetDirectory) {
    CompletableFuture<ResourceLocation> existing = pendingLoads.get(key);
    if (existing != null) {
      return existing;
    }

    // Create a new future and register it
    CompletableFuture<ResourceLocation> future = new CompletableFuture<>();
    pendingLoads.put(key, future);
    downloadQueue.offer(new TextureLoadRequest(key, url, targetDirectory, future));
    return future;
  }

  public static CompletableFuture<ResourceLocation> loadPlayerTextureAsync(
      TextureModelKey key, UUID playerUUID, Path targetDirectory) {
    CompletableFuture<ResourceLocation> existing = pendingLoads.get(key);
    if (existing != null) {
      return existing;
    }

    // Create a new future and register it
    CompletableFuture<ResourceLocation> future = new CompletableFuture<>();
    pendingLoads.put(key, future);

    // Asynchronously get the player texture URL
    CompletableFuture.supplyAsync(
            () -> PlayersUtils.getUserTexture(playerUUID), textureLoadExecutor)
        .thenAccept(
            url -> {
              if (url == null || url.isEmpty()) {
                log.error("{} Unable to get player skin URL for UUID: {}", LOG_PREFIX, playerUUID);
                pendingLoads.remove(key);
                future.complete(null);
              } else {
                log.debug("{} Got player skin URL for {}: {}", LOG_PREFIX, playerUUID, url);
                downloadQueue.offer(new TextureLoadRequest(key, url, targetDirectory, future));
              }
            })
        .exceptionally(
            throwable -> {
              log.error(
                  "{} Failed to get player texture URL for {}: {}",
                  LOG_PREFIX,
                  playerUUID,
                  throwable.getMessage());
              pendingLoads.remove(key);
              future.completeExceptionally(throwable);
              return null;
            });

    return future;
  }

  private static void processQueue() {
    try {
      long now = System.currentTimeMillis();
      if (now - lastDownloadTime < 500) {
        return;
      }

      // Poll the next request from the queue
      TextureLoadRequest request = downloadQueue.poll();
      if (request == null) {
        return;
      }

      // Update last download time and start the download task
      lastDownloadTime = now;
      CompletableFuture.supplyAsync(
              () ->
                  RemoteTextureLoader.loadRemoteTexture(
                      request.key, request.url, request.targetDirectory),
              textureLoadExecutor)
          .whenComplete(
              (result, throwable) -> {
                pendingLoads.remove(request.key);
                if (throwable != null) {
                  log.error(
                      "{} Failed to load texture {}: {}",
                      LOG_PREFIX,
                      request.key,
                      throwable.getMessage());
                  request.future.completeExceptionally(throwable);
                } else {
                  request.future.complete(result);
                }
              });
    } catch (Exception e) {
      log.error("{} Error processing queue: {}", LOG_PREFIX, e.getMessage());
    }
  }

  public static void shutdown() {
    rateLimitScheduler.shutdown();
    textureLoadExecutor.shutdown();
  }

  private record TextureLoadRequest(
      TextureModelKey key,
      String url,
      Path targetDirectory,
      CompletableFuture<ResourceLocation> future) {}
}
