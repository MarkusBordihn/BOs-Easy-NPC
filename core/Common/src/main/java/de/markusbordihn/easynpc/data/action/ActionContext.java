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

import java.util.Collection;
import java.util.List;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

/** Never persisted and never sent over the network. */
public record ActionContext(
    ActionEventType eventType,
    ServerPlayer initiator,
    List<ServerPlayer> audience,
    ResourceLocation sourceId) {

  public static final ActionContext EMPTY =
      new ActionContext(ActionEventType.NONE, null, List.of(), null);

  public ActionContext {
    audience = audience == null ? List.of() : List.copyOf(audience);
  }

  public static ActionContext of(ServerPlayer initiator) {
    return of(ActionEventType.NONE, initiator);
  }

  public static ActionContext of(ActionEventType eventType, ServerPlayer initiator) {
    return new ActionContext(eventType, initiator, toAudience(initiator), null);
  }

  public static ActionContext of(
      ActionEventType eventType, ServerPlayer initiator, Collection<ServerPlayer> audience) {
    return new ActionContext(eventType, initiator, toList(audience), null);
  }

  private static List<ServerPlayer> toAudience(ServerPlayer initiator) {
    if (initiator == null) {
      return List.of();
    }

    return List.of(initiator);
  }

  private static List<ServerPlayer> toList(Collection<ServerPlayer> audience) {
    if (audience == null) {
      return List.of();
    }

    return List.copyOf(audience);
  }

  public ActionContext withEventType(ActionEventType eventType) {
    return new ActionContext(eventType, this.initiator, this.audience, this.sourceId);
  }

  public ActionContext withInitiator(ServerPlayer initiator) {
    return new ActionContext(this.eventType, initiator, this.audience, this.sourceId);
  }

  public ActionContext withAudience(Collection<ServerPlayer> audience) {
    return new ActionContext(this.eventType, this.initiator, toList(audience), this.sourceId);
  }

  public ActionContext withSourceId(ResourceLocation sourceId) {
    return new ActionContext(this.eventType, this.initiator, this.audience, sourceId);
  }

  public boolean hasInitiator() {
    return this.initiator != null;
  }

  public boolean hasAudience() {
    return !this.audience.isEmpty();
  }

  public List<ServerPlayer> audienceOrInitiator() {
    if (!this.audience.isEmpty()) {
      return this.audience;
    }

    return toAudience(this.initiator);
  }
}
