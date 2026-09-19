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

package de.markusbordihn.easynpc.data.highlight;

import de.markusbordihn.easynpc.config.ClientHighlightConfig;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

public class NPCHighlightManager {

  private static final AtomicInteger clientTick = new AtomicInteger();
  private static final Map<UUID, Integer> highlightedNPCs = new ConcurrentHashMap<>();

  private NPCHighlightManager() {}

  public static void highlight(UUID uuid, int durationTicks) {
    if (uuid == null || durationTicks <= 0) {
      return;
    }

    highlightedNPCs.put(uuid, clientTick.get() + durationTicks);
  }

  public static void tick() {
    int currentTick = clientTick.incrementAndGet();
    highlightedNPCs.values().removeIf(expirationTick -> expirationTick <= currentTick);
  }

  public static boolean isHighlighted(UUID uuid) {
    if (uuid == null || !ClientHighlightConfig.HIGHLIGHT_ENABLED || highlightedNPCs.isEmpty()) {
      return false;
    }

    Integer expirationTick = highlightedNPCs.get(uuid);
    if (expirationTick == null) {
      return false;
    }

    if (expirationTick <= clientTick.get()) {
      highlightedNPCs.remove(uuid);
      return false;
    }

    return true;
  }

  public static void clear() {
    highlightedNPCs.clear();
  }
}
