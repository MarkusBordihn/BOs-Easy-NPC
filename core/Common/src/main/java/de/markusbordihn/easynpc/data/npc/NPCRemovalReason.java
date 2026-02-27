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

package de.markusbordihn.easynpc.data.npc;

import net.minecraft.world.entity.Entity;

public enum NPCRemovalReason {
  NONE,
  KILLED,
  DESPAWNED,
  UNLOADED_TO_CHUNK,
  UNLOADED_WITH_PLAYER,
  UNLOADED_BY_PLAYER,
  UNLOADED_BY_SERVER,
  UNLOADED_BY_ACTION,
  CHANGED_DIMENSION;

  public static NPCRemovalReason fromRemovalReason(Entity.RemovalReason removalReason) {
    if (removalReason == null) {
      return NONE;
    }
    return switch (removalReason) {
      case KILLED -> KILLED;
      case DISCARDED -> DESPAWNED;
      case UNLOADED_TO_CHUNK -> UNLOADED_TO_CHUNK;
      case UNLOADED_WITH_PLAYER -> UNLOADED_WITH_PLAYER;
      case CHANGED_DIMENSION -> CHANGED_DIMENSION;
    };
  }

  public static NPCRemovalReason fromString(String value) {
    if (value == null || value.isEmpty()) {
      return NONE;
    }
    try {
      return valueOf(value);
    } catch (IllegalArgumentException e) {
      return NONE;
    }
  }
}
