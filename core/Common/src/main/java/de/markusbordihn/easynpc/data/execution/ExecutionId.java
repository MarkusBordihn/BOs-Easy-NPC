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

package de.markusbordihn.easynpc.data.execution;

import java.nio.charset.StandardCharsets;
import java.util.UUID;
import net.minecraft.world.entity.Entity;

public record ExecutionId(ExecutionType type, UUID value) {

  private static final UUID EMPTY_UUID = new UUID(0L, 0L);

  public ExecutionId {
    if (type == null) {
      throw new IllegalArgumentException("Execution id type must not be null");
    }
    if (value == null) {
      throw new IllegalArgumentException("Execution id value must not be null");
    }
  }

  public static ExecutionId dialog(UUID npcId, UUID dialogId) {
    return dialogId != null ? create(ExecutionType.DIALOG, npcId, dialogId) : null;
  }

  public static ExecutionId dialog(Entity npc, UUID dialogId) {
    return dialog(npc != null ? npc.getUUID() : null, dialogId);
  }

  public static ExecutionId dialogButton(UUID npcId, UUID dialogId, UUID dialogButtonId) {
    return dialogId != null && dialogButtonId != null
        ? create(ExecutionType.DIALOG_BUTTON, npcId, dialogId, dialogButtonId)
        : null;
  }

  public static ExecutionId dialogButton(Entity npc, UUID dialogId, UUID dialogButtonId) {
    return dialogButton(npc != null ? npc.getUUID() : null, dialogId, dialogButtonId);
  }

  public static ExecutionId action(UUID npcId, UUID actionId) {
    return actionId != null ? create(ExecutionType.ACTION, npcId, actionId) : null;
  }

  public static ExecutionId action(Entity npc, UUID actionId) {
    return action(npc != null ? npc.getUUID() : null, actionId);
  }

  private static ExecutionId create(ExecutionType type, UUID... components) {
    StringBuilder key = new StringBuilder(type.name());
    for (UUID component : components) {
      key.append(':').append(component != null ? component : EMPTY_UUID);
    }
    return new ExecutionId(
        type, UUID.nameUUIDFromBytes(key.toString().getBytes(StandardCharsets.UTF_8)));
  }
}
