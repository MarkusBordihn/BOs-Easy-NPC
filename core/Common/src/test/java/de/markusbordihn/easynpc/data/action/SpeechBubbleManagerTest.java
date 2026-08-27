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

package de.markusbordihn.easynpc.data.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;
import net.minecraft.network.chat.Component;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SpeechBubbleManagerTest {

  @Test
  @DisplayName("Short speech bubbles keep the five second minimum")
  void testShortTextDuration() {
    assertEquals(
        SpeechBubbleManager.DEFAULT_DURATION_TICKS,
        SpeechBubbleManager.calculateReadingDurationTicks(Component.literal("Hello")));
  }

  @Test
  @DisplayName("Long speech bubbles receive additional reading time")
  void testLongTextDuration() {
    assertEquals(
        190, SpeechBubbleManager.calculateReadingDurationTicks(Component.literal("A".repeat(100))));
  }

  @Test
  @DisplayName("Speech bubble reading time is capped")
  void testMaximumDuration() {
    assertEquals(
        SpeechBubbleManager.MAX_READING_DURATION_TICKS,
        SpeechBubbleManager.calculateReadingDurationTicks(Component.literal("A".repeat(1000))));
  }

  @Test
  @DisplayName("Showing a bubble applies its calculated reading time")
  void testShowUsesReadingDuration() {
    UUID uuid = UUID.randomUUID();
    SpeechBubbleManager.show(uuid, Component.literal("A".repeat(100)), 20);

    assertEquals(190, SpeechBubbleManager.get(uuid).durationTicks());
    SpeechBubbleManager.remove(uuid);
  }

  @Test
  @DisplayName("Speech bubbles are only reported as present while one is shown")
  void testIsEmpty() {
    SpeechBubbleManager.clear();
    assertTrue(SpeechBubbleManager.isEmpty());

    UUID uuid = UUID.randomUUID();
    SpeechBubbleManager.show(uuid, Component.literal("Hello"), 20);
    assertFalse(SpeechBubbleManager.isEmpty());

    SpeechBubbleManager.remove(uuid);
    assertTrue(SpeechBubbleManager.isEmpty());
  }
}
