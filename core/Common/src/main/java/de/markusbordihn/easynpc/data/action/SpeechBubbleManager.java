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

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import net.minecraft.network.chat.Component;

public class SpeechBubbleManager {

  public static final int DEFAULT_DURATION_TICKS = 100;
  public static final int FADE_OUT_TICKS = 10;
  public static final int MAX_READING_DURATION_TICKS = 360;
  private static final int READING_PADDING_TICKS = 40;
  private static final AtomicInteger clientTick = new AtomicInteger();
  private static final Map<UUID, SpeechBubbleEntry> speechBubbles = new ConcurrentHashMap<>();

  private SpeechBubbleManager() {}

  public static void show(UUID uuid, Component text, int durationTicks) {
    if (uuid == null || text == null || durationTicks <= 0) {
      return;
    }

    int readableDurationTicks = Math.max(durationTicks, calculateReadingDurationTicks(text));
    speechBubbles.put(uuid, new SpeechBubbleEntry(text, clientTick.get(), readableDurationTicks));
  }

  public static void tick() {
    int currentTick = clientTick.incrementAndGet();
    speechBubbles.values().removeIf(speechBubbleEntry -> speechBubbleEntry.isExpired(currentTick));
  }

  public static int currentTick() {
    return clientTick.get();
  }

  public static int calculateReadingDurationTicks(Component text) {
    if (text == null) {
      return DEFAULT_DURATION_TICKS;
    }

    String displayedText = text.getString();
    int characterCount = displayedText.codePointCount(0, displayedText.length());
    int readingDurationTicks = READING_PADDING_TICKS + (characterCount * 3 + 1) / 2;
    return Math.max(
        DEFAULT_DURATION_TICKS, Math.min(readingDurationTicks, MAX_READING_DURATION_TICKS));
  }

  public static SpeechBubbleEntry get(UUID uuid) {
    if (uuid == null) {
      return null;
    }

    SpeechBubbleEntry speechBubbleEntry = speechBubbles.get(uuid);
    if (speechBubbleEntry == null) {
      return null;
    }

    if (speechBubbleEntry.isExpired(clientTick.get())) {
      speechBubbles.remove(uuid);
      return null;
    }

    return speechBubbleEntry;
  }

  public static boolean isEmpty() {
    return speechBubbles.isEmpty();
  }

  public static void remove(UUID uuid) {
    if (uuid != null) {
      speechBubbles.remove(uuid);
    }
  }

  public static void clear() {
    speechBubbles.clear();
  }

  public record SpeechBubbleEntry(Component text, int startTick, int durationTicks) {

    public boolean isExpired(int currentTick) {
      return currentTick - this.startTick >= this.durationTicks;
    }

    public float getOpacity(float currentTick) {
      float remainingTicks = this.startTick + (float) this.durationTicks - currentTick;
      if (remainingTicks >= FADE_OUT_TICKS) {
        return 1.0F;
      }

      return Math.max(0.0F, remainingTicks / FADE_OUT_TICKS);
    }
  }
}
