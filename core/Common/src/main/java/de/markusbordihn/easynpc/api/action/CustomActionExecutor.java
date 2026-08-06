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

package de.markusbordihn.easynpc.api.action;

import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.List;
import net.minecraft.server.level.ServerPlayer;

/** The server player is null for an event without an initiator. */
@FunctionalInterface
public interface CustomActionExecutor {

  /**
   * @deprecated Implement {@link #execute(ActionDataEntry, EasyNPC, List, ActionContext)}, which
   *     also carries every player affected by the event.
   */
  @Deprecated(since = "7.6.0")
  void execute(
      ActionDataEntry actionDataEntry,
      EasyNPC<?> easyNPC,
      ServerPlayer serverPlayer,
      List<String> arguments);

  /** The context contains the initiator and every player affected by the event. */
  default void execute(
      ActionDataEntry actionDataEntry,
      EasyNPC<?> easyNPC,
      List<String> arguments,
      ActionContext actionContext) {
    this.execute(actionDataEntry, easyNPC, actionContext.initiator(), arguments);
  }
}
