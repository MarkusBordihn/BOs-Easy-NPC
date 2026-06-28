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

import com.mojang.blaze3d.platform.NativeImage;
import de.markusbordihn.easynpc.Constants;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executor;
import java.util.function.BooleanSupplier;
import net.minecraft.client.Minecraft;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TextureRegistrationQueue {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Texture Registration Queue]";
  private static final TextureRegistrationQueue INSTANCE = new TextureRegistrationQueue();
  private static final int DEFAULT_REGISTRATIONS_PER_TICK = 3;

  private final Map<TextureModelKey, CompletableFuture<ResourceLocation>> pendingRegistrations =
      new ConcurrentHashMap<>();
  private final Map<TextureModelKey, TextureRegistrationStatus> registrationStatus =
      new ConcurrentHashMap<>();
  private final Queue<PendingTextureRegistration> registrationQueue = new ConcurrentLinkedQueue<>();
  private final BooleanSupplier renderThreadCheck;
  private final Executor renderThreadExecutor;
  private final TextureRegistrar textureRegistrar;

  public TextureRegistrationQueue() {
    this(
        () -> Minecraft.getInstance().isSameThread(),
        runnable -> Minecraft.getInstance().execute(runnable),
        TextureRegistrationHelper::registerTextureOnCurrentThread);
  }

  TextureRegistrationQueue(
      BooleanSupplier renderThreadCheck,
      Executor renderThreadExecutor,
      TextureRegistrar textureRegistrar) {
    this.renderThreadCheck = renderThreadCheck;
    this.renderThreadExecutor = renderThreadExecutor;
    this.textureRegistrar = textureRegistrar;
  }

  public static TextureRegistrationQueue getInstance() {
    return INSTANCE;
  }

  private static ResourceLocation getResourceLocation(TextureModelKey textureModelKey) {
    return new ResourceLocation(TextureNameHelper.getResourceName(textureModelKey));
  }

  private static void closeNativeImage(NativeImage nativeImage) {
    if (nativeImage != null) {
      nativeImage.close();
    }
  }

  public ResourceLocation register(TextureModelKey textureModelKey, NativeImage nativeImage) {
    CompletableFuture<ResourceLocation> registrationFuture =
        registerAsync(textureModelKey, nativeImage);
    return registrationFuture.isDone()
        ? registrationFuture.getNow(null)
        : getResourceLocation(textureModelKey);
  }

  public CompletableFuture<ResourceLocation> registerAsync(
      TextureModelKey textureModelKey, NativeImage nativeImage) {
    if (textureModelKey == null || nativeImage == null) {
      closeNativeImage(nativeImage);
      return CompletableFuture.completedFuture(null);
    }

    ResourceLocation resourceLocation = getResourceLocation(textureModelKey);
    if (getStatus(textureModelKey) == TextureRegistrationStatus.REGISTERED) {
      closeNativeImage(nativeImage);
      return CompletableFuture.completedFuture(resourceLocation);
    }
    if (getStatus(textureModelKey) == TextureRegistrationStatus.PENDING) {
      closeNativeImage(nativeImage);
      log.debug("{} Skipped duplicate pending registration for {}", LOG_PREFIX, textureModelKey);
      CompletableFuture<ResourceLocation> existingFuture =
          pendingRegistrations.get(textureModelKey);
      return existingFuture != null
          ? existingFuture
          : CompletableFuture.completedFuture(resourceLocation);
    }

    if (renderThreadCheck.getAsBoolean()) {
      return CompletableFuture.completedFuture(registerNow(textureModelKey, nativeImage));
    }

    CompletableFuture<ResourceLocation> registrationFuture = new CompletableFuture<>();
    CompletableFuture<ResourceLocation> existingFuture =
        pendingRegistrations.putIfAbsent(textureModelKey, registrationFuture);
    if (existingFuture != null) {
      closeNativeImage(nativeImage);
      log.debug("{} Skipped duplicate pending registration for {}", LOG_PREFIX, textureModelKey);
      return existingFuture;
    }

    registrationStatus.put(textureModelKey, TextureRegistrationStatus.PENDING);
    registrationQueue.offer(
        new PendingTextureRegistration(textureModelKey, nativeImage, registrationFuture));
    log.debug("{} Queued texture registration for {}", LOG_PREFIX, textureModelKey);
    return registrationFuture;
  }

  public TextureRegistrationStatus getStatus(TextureModelKey textureModelKey) {
    return registrationStatus.getOrDefault(textureModelKey, TextureRegistrationStatus.UNKNOWN);
  }

  public boolean hasPendingRegistration(TextureModelKey textureModelKey) {
    return getStatus(textureModelKey) == TextureRegistrationStatus.PENDING;
  }

  public void processPendingRegistrations() {
    processPendingRegistrations(DEFAULT_REGISTRATIONS_PER_TICK);
  }

  public void processPendingRegistrations(int maxRegistrations) {
    if (maxRegistrations <= 0 || registrationQueue.isEmpty()) {
      return;
    }

    if (!renderThreadCheck.getAsBoolean()) {
      try {
        renderThreadExecutor.execute(() -> processPendingRegistrations(maxRegistrations));
      } catch (RuntimeException exception) {
        log.error("{} Unable to schedule pending texture registrations:", LOG_PREFIX, exception);
      }
      return;
    }

    for (int index = 0; index < maxRegistrations; index++) {
      PendingTextureRegistration registration = registrationQueue.poll();
      if (registration == null) {
        return;
      }

      CompletableFuture<ResourceLocation> currentFuture =
          pendingRegistrations.get(registration.textureModelKey());
      if (currentFuture != registration.registrationFuture()) {
        closeNativeImage(registration.nativeImage());
        continue;
      }

      completeRegistration(
          registration.textureModelKey(),
          registration.nativeImage(),
          registration.registrationFuture());
    }
  }

  public void clear() {
    for (PendingTextureRegistration registration : registrationQueue) {
      closeNativeImage(registration.nativeImage());
    }
    pendingRegistrations.values().forEach(future -> future.complete(null));
    registrationQueue.clear();
    pendingRegistrations.clear();
    registrationStatus.clear();
  }

  public void clear(Set<TextureModelKey> textureModelKeys) {
    registrationQueue.removeIf(
        registration -> {
          if (textureModelKeys.contains(registration.textureModelKey())) {
            closeNativeImage(registration.nativeImage());
            registration.registrationFuture().complete(null);
            return true;
          }
          return false;
        });
    for (TextureModelKey textureModelKey : textureModelKeys) {
      CompletableFuture<ResourceLocation> registrationFuture =
          pendingRegistrations.remove(textureModelKey);
      if (registrationFuture != null) {
        registrationFuture.complete(null);
      }
      registrationStatus.remove(textureModelKey);
    }
  }

  private ResourceLocation registerNow(TextureModelKey textureModelKey, NativeImage nativeImage) {
    registrationStatus.put(textureModelKey, TextureRegistrationStatus.PENDING);
    try {
      ResourceLocation resourceLocation = textureRegistrar.register(textureModelKey, nativeImage);
      registrationStatus.put(
          textureModelKey,
          resourceLocation != null
              ? TextureRegistrationStatus.REGISTERED
              : TextureRegistrationStatus.FAILED);
      return resourceLocation;
    } catch (RuntimeException exception) {
      registrationStatus.put(textureModelKey, TextureRegistrationStatus.FAILED);
      closeNativeImage(nativeImage);
      log.error("{} Unable to register texture for {}:", LOG_PREFIX, textureModelKey, exception);
      return null;
    }
  }

  private void completeRegistration(
      TextureModelKey textureModelKey,
      NativeImage nativeImage,
      CompletableFuture<ResourceLocation> registrationFuture) {
    try {
      ResourceLocation resourceLocation = textureRegistrar.register(textureModelKey, nativeImage);
      if (resourceLocation != null) {
        registrationStatus.put(textureModelKey, TextureRegistrationStatus.REGISTERED);
        log.debug(
            "{} Registered queued texture {} with {}",
            LOG_PREFIX,
            textureModelKey,
            resourceLocation);
      } else {
        registrationStatus.put(textureModelKey, TextureRegistrationStatus.FAILED);
      }
      registrationFuture.complete(resourceLocation);
    } catch (RuntimeException exception) {
      registrationStatus.put(textureModelKey, TextureRegistrationStatus.FAILED);
      closeNativeImage(nativeImage);
      registrationFuture.completeExceptionally(exception);
      log.error(
          "{} Unable to register queued texture for {}:", LOG_PREFIX, textureModelKey, exception);
    } finally {
      pendingRegistrations.remove(textureModelKey, registrationFuture);
    }
  }

  @FunctionalInterface
  interface TextureRegistrar {
    ResourceLocation register(TextureModelKey textureModelKey, NativeImage nativeImage);
  }

  private record PendingTextureRegistration(
      TextureModelKey textureModelKey,
      NativeImage nativeImage,
      CompletableFuture<ResourceLocation> registrationFuture) {}
}
