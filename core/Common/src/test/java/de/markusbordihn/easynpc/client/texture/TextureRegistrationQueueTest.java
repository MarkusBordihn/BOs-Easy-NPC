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

import com.mojang.blaze3d.platform.NativeImage;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executor;
import java.util.concurrent.atomic.AtomicInteger;
import net.minecraft.resources.Identifier;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class TextureRegistrationQueueTest {

  private static TextureModelKey newTextureModelKey() {
    return new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);
  }

  private static Identifier expectedIdentifier(TextureModelKey textureModelKey) {
    return Identifier.parse(TextureNameHelper.getResourceName(textureModelKey));
  }

  @Test
  @DisplayName("Should register immediately on render thread")
  void testRegisterImmediatelyOnRenderThread() {
    TextureModelKey textureModelKey = newTextureModelKey();
    AtomicInteger registrationCount = new AtomicInteger();
    TextureRegistrationQueue queue =
        new TextureRegistrationQueue(
            () -> true,
            Runnable::run,
            (key, image) -> {
              registrationCount.incrementAndGet();
              image.close();
              return expectedIdentifier(key);
            });

    Identifier identifier = queue.register(textureModelKey, new NativeImage(64, 64, false));

    assertEquals(expectedIdentifier(textureModelKey), identifier);
    assertEquals(TextureRegistrationStatus.REGISTERED, queue.getStatus(textureModelKey));
    assertEquals(1, registrationCount.get());
  }

  @Test
  @DisplayName("Should queue registration off render thread")
  void testQueueRegistrationOffRenderThread() {
    TextureModelKey textureModelKey = newTextureModelKey();
    ControlledExecutor controlledExecutor = new ControlledExecutor();
    AtomicInteger registrationCount = new AtomicInteger();
    TextureRegistrationQueue queue =
        new TextureRegistrationQueue(
            () -> false,
            controlledExecutor,
            (key, image) -> {
              registrationCount.incrementAndGet();
              image.close();
              return expectedIdentifier(key);
            });

    Identifier identifier = queue.register(textureModelKey, new NativeImage(64, 64, false));

    assertEquals(expectedIdentifier(textureModelKey), identifier);
    assertTrue(queue.hasPendingRegistration(textureModelKey));
    assertEquals(0, registrationCount.get());

    controlledExecutor.runNext();

    assertEquals(TextureRegistrationStatus.REGISTERED, queue.getStatus(textureModelKey));
    assertEquals(1, registrationCount.get());
  }

  @Test
  @DisplayName("Should skip duplicate pending registration")
  void testSkipDuplicatePendingRegistration() {
    TextureModelKey textureModelKey = newTextureModelKey();
    ControlledExecutor controlledExecutor = new ControlledExecutor();
    AtomicInteger registrationCount = new AtomicInteger();
    TextureRegistrationQueue queue =
        new TextureRegistrationQueue(
            () -> false,
            controlledExecutor,
            (key, image) -> {
              registrationCount.incrementAndGet();
              image.close();
              return expectedIdentifier(key);
            });

    Identifier firstIdentifier = queue.register(textureModelKey, new NativeImage(64, 64, false));
    Identifier duplicateIdentifier =
        queue.register(textureModelKey, new NativeImage(64, 64, false));

    assertEquals(firstIdentifier, duplicateIdentifier);
    assertTrue(queue.hasPendingRegistration(textureModelKey));
    assertEquals(1, controlledExecutor.size());

    controlledExecutor.runNext();

    assertEquals(TextureRegistrationStatus.REGISTERED, queue.getStatus(textureModelKey));
    assertEquals(1, registrationCount.get());
  }

  @Test
  @DisplayName("Should mark failed registration")
  void testFailedRegistration() {
    TextureModelKey textureModelKey = newTextureModelKey();
    TextureRegistrationQueue queue =
        new TextureRegistrationQueue(
            () -> true,
            Runnable::run,
            (key, image) -> {
              image.close();
              return null;
            });

    Identifier identifier = queue.register(textureModelKey, new NativeImage(64, 64, false));

    assertNull(identifier);
    assertEquals(TextureRegistrationStatus.FAILED, queue.getStatus(textureModelKey));
  }

  @Test
  @DisplayName("Should clear only requested texture status")
  void testClearSelectedTextureStatus() {
    TextureModelKey firstKey = newTextureModelKey();
    TextureModelKey secondKey = newTextureModelKey();
    TextureRegistrationQueue queue =
        new TextureRegistrationQueue(
            () -> true,
            Runnable::run,
            (key, image) -> {
              image.close();
              return expectedIdentifier(key);
            });

    queue.register(firstKey, new NativeImage(64, 64, false));
    queue.register(secondKey, new NativeImage(64, 64, false));

    queue.clear(Set.of(firstKey));

    assertEquals(TextureRegistrationStatus.UNKNOWN, queue.getStatus(firstKey));
    assertEquals(TextureRegistrationStatus.REGISTERED, queue.getStatus(secondKey));
  }

  @Test
  @DisplayName("Should clear all texture status including failed entries")
  void testClearAllStatus() {
    TextureModelKey registeredKey = newTextureModelKey();
    TextureModelKey failedKey = newTextureModelKey();
    TextureRegistrationQueue queue =
        new TextureRegistrationQueue(
            () -> true,
            Runnable::run,
            (key, image) -> {
              image.close();
              return key.equals(failedKey) ? null : expectedIdentifier(key);
            });

    queue.register(registeredKey, new NativeImage(64, 64, false));
    queue.register(failedKey, new NativeImage(64, 64, false));

    assertEquals(TextureRegistrationStatus.REGISTERED, queue.getStatus(registeredKey));
    assertEquals(TextureRegistrationStatus.FAILED, queue.getStatus(failedKey));

    queue.clear();

    assertEquals(TextureRegistrationStatus.UNKNOWN, queue.getStatus(registeredKey));
    assertEquals(TextureRegistrationStatus.UNKNOWN, queue.getStatus(failedKey));
  }

  private static class ControlledExecutor implements Executor {

    private final ConcurrentLinkedQueue<Runnable> tasks = new ConcurrentLinkedQueue<>();

    @Override
    public void execute(Runnable command) {
      tasks.offer(command);
    }

    void runNext() {
      Runnable task = tasks.poll();
      assertNotNull(task);
      task.run();
    }

    int size() {
      return tasks.size();
    }
  }
}
