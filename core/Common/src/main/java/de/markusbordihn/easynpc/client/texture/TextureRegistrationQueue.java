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
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.function.BooleanSupplier;
import net.minecraft.client.Minecraft;
import net.minecraft.resources.Identifier;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TextureRegistrationQueue {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Texture Registration Queue]";
  private static final TextureRegistrationQueue INSTANCE = new TextureRegistrationQueue();

  private final Map<TextureModelKey, CompletableFuture<Identifier>> pendingRegistrations =
      new ConcurrentHashMap<>();
  private final Map<TextureModelKey, TextureRegistrationStatus> registrationStatus =
      new ConcurrentHashMap<>();
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

  private static Identifier getResourceLocation(TextureModelKey textureModelKey) {
    return Identifier.parse(TextureNameHelper.getResourceName(textureModelKey));
  }

  private static void closeNativeImage(NativeImage nativeImage) {
    if (nativeImage != null) {
      nativeImage.close();
    }
  }

  public Identifier register(TextureModelKey textureModelKey, NativeImage nativeImage) {
    if (textureModelKey == null || nativeImage == null) {
      closeNativeImage(nativeImage);
      return null;
    }

    Identifier resourceLocation = getResourceLocation(textureModelKey);
    if (getStatus(textureModelKey) == TextureRegistrationStatus.REGISTERED) {
      closeNativeImage(nativeImage);
      return resourceLocation;
    }
    if (getStatus(textureModelKey) == TextureRegistrationStatus.PENDING) {
      closeNativeImage(nativeImage);
      log.debug("{} Skipped duplicate pending registration for {}", LOG_PREFIX, textureModelKey);
      return resourceLocation;
    }

    if (renderThreadCheck.getAsBoolean()) {
      return registerNow(textureModelKey, nativeImage);
    }

    CompletableFuture<Identifier> registrationFuture = new CompletableFuture<>();
    CompletableFuture<Identifier> existingFuture =
        pendingRegistrations.putIfAbsent(textureModelKey, registrationFuture);
    if (existingFuture != null) {
      closeNativeImage(nativeImage);
      log.debug("{} Skipped duplicate pending registration for {}", LOG_PREFIX, textureModelKey);
      return resourceLocation;
    }

    registrationStatus.put(textureModelKey, TextureRegistrationStatus.PENDING);
    log.debug("{} Queued texture registration for {}", LOG_PREFIX, textureModelKey);
    try {
      renderThreadExecutor.execute(
          () -> completeRegistration(textureModelKey, nativeImage, registrationFuture));
    } catch (RuntimeException exception) {
      pendingRegistrations.remove(textureModelKey);
      registrationStatus.put(textureModelKey, TextureRegistrationStatus.FAILED);
      closeNativeImage(nativeImage);
      registrationFuture.completeExceptionally(exception);
      log.error(
          "{} Unable to queue texture registration for {}:",
          LOG_PREFIX,
          textureModelKey,
          exception);
      return null;
    }
    return resourceLocation;
  }

  public TextureRegistrationStatus getStatus(TextureModelKey textureModelKey) {
    return registrationStatus.getOrDefault(textureModelKey, TextureRegistrationStatus.UNKNOWN);
  }

  public boolean hasPendingRegistration(TextureModelKey textureModelKey) {
    return getStatus(textureModelKey) == TextureRegistrationStatus.PENDING;
  }

  public void clear() {
    pendingRegistrations.clear();
    registrationStatus.clear();
  }

  public void clear(Set<TextureModelKey> textureModelKeys) {
    for (TextureModelKey textureModelKey : textureModelKeys) {
      pendingRegistrations.remove(textureModelKey);
      registrationStatus.remove(textureModelKey);
    }
  }

  private Identifier registerNow(TextureModelKey textureModelKey, NativeImage nativeImage) {
    registrationStatus.put(textureModelKey, TextureRegistrationStatus.PENDING);
    try {
      Identifier resourceLocation = textureRegistrar.register(textureModelKey, nativeImage);
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
      CompletableFuture<Identifier> registrationFuture) {
    try {
      Identifier resourceLocation = textureRegistrar.register(textureModelKey, nativeImage);
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
      pendingRegistrations.remove(textureModelKey);
    }
  }

  @FunctionalInterface
  interface TextureRegistrar {
    Identifier register(TextureModelKey textureModelKey, NativeImage nativeImage);
  }
}
