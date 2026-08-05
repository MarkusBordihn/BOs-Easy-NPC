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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.resources.ResourceLocation;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ActionContextTest {

  @Test
  @DisplayName("The empty context has neither an initiator nor an audience")
  void emptyContext() {
    assertFalse(ActionContext.EMPTY.hasInitiator());
    assertFalse(ActionContext.EMPTY.hasAudience());
    assertTrue(ActionContext.EMPTY.audience().isEmpty());
    assertTrue(ActionContext.EMPTY.audienceOrInitiator().isEmpty());
    assertEquals(ActionEventType.NONE, ActionContext.EMPTY.eventType());
    assertNull(ActionContext.EMPTY.sourceId());
  }

  @Test
  @DisplayName("A context without an initiator keeps an empty audience")
  void contextWithoutInitiator() {
    ActionContext actionContext = ActionContext.of(null);

    assertFalse(actionContext.hasInitiator());
    assertFalse(actionContext.hasAudience());
    assertEquals(ActionEventType.NONE, actionContext.eventType());
  }

  @Test
  @DisplayName("A null audience becomes an empty list instead of null")
  void nullAudienceIsNormalized() {
    ActionContext actionContext = new ActionContext(ActionEventType.ON_SPAWN, null, null, null);

    assertTrue(actionContext.audience().isEmpty());
    assertEquals(ActionEventType.ON_SPAWN, actionContext.eventType());
  }

  @Test
  @DisplayName("The audience cannot be changed from the outside")
  void audienceIsImmutable() {
    assertThrows(
        UnsupportedOperationException.class, () -> ActionContext.EMPTY.audience().add(null));
  }

  @Test
  @DisplayName("The copy methods keep every other value")
  void copyMethodsKeepTheRest() {
    ResourceLocation sourceId = ResourceLocation.fromNamespaceAndPath("my_mod", "quest_stage");
    ActionContext actionContext =
        ActionContext.EMPTY.withEventType(ActionEventType.ON_STATE_CHANGE).withSourceId(sourceId);

    assertEquals(ActionEventType.ON_STATE_CHANGE, actionContext.eventType());
    assertEquals(sourceId, actionContext.sourceId());
    assertEquals(sourceId, actionContext.withEventType(ActionEventType.ON_SPAWN).sourceId());
    assertEquals(ActionEventType.ON_STATE_CHANGE, actionContext.withInitiator(null).eventType());
  }
}
