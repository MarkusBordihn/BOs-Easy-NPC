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

import static org.junit.jupiter.api.Assertions.*;

import java.util.UUID;
import org.junit.jupiter.api.Test;

class ExecutionIdTest {

  @Test
  void testSameInputsProduceSameId() {
    UUID npcId = UUID.randomUUID();
    UUID dialogId = UUID.randomUUID();
    UUID buttonId = UUID.randomUUID();

    assertEquals(ExecutionId.dialog(npcId, dialogId), ExecutionId.dialog(npcId, dialogId));
    assertEquals(ExecutionId.action(npcId, dialogId), ExecutionId.action(npcId, dialogId));
    assertEquals(
        ExecutionId.dialogButton(npcId, dialogId, buttonId),
        ExecutionId.dialogButton(npcId, dialogId, buttonId));
  }

  @Test
  void testTypeSeparatesIds() {
    UUID npcId = UUID.randomUUID();
    UUID sharedId = UUID.randomUUID();

    ExecutionId dialogId = ExecutionId.dialog(npcId, sharedId);
    ExecutionId actionId = ExecutionId.action(npcId, sharedId);
    ExecutionId buttonId = ExecutionId.dialogButton(npcId, sharedId, sharedId);

    assertNotEquals(dialogId, actionId);
    assertNotEquals(dialogId, buttonId);
    assertNotEquals(actionId, buttonId);
  }

  @Test
  void testNpcSeparatesIds() {
    UUID dialogId = UUID.randomUUID();

    assertNotEquals(
        ExecutionId.dialog(UUID.randomUUID(), dialogId),
        ExecutionId.dialog(UUID.randomUUID(), dialogId));
  }

  @Test
  void testNullNpcIsAllowedButScopedSeparately() {
    UUID npcId = null;
    UUID dialogId = UUID.randomUUID();

    ExecutionId withoutNpc = ExecutionId.dialog(npcId, dialogId);
    assertNotNull(withoutNpc);
    assertNotEquals(withoutNpc, ExecutionId.dialog(UUID.randomUUID(), dialogId));
  }

  @Test
  void testMissingTargetReturnsNull() {
    UUID npcId = UUID.randomUUID();
    UUID dialogId = UUID.randomUUID();

    assertNull(ExecutionId.dialog(npcId, null));
    assertNull(ExecutionId.action(npcId, null));
    assertNull(ExecutionId.dialogButton(npcId, null, UUID.randomUUID()));
    assertNull(ExecutionId.dialogButton(npcId, dialogId, null));
  }

  @Test
  void testFactoriesExposeTheirType() {
    UUID npcId = UUID.randomUUID();
    UUID targetId = UUID.randomUUID();

    assertEquals(ExecutionType.DIALOG, ExecutionId.dialog(npcId, targetId).type());
    assertEquals(ExecutionType.ACTION, ExecutionId.action(npcId, targetId).type());
    assertEquals(
        ExecutionType.DIALOG_BUTTON, ExecutionId.dialogButton(npcId, targetId, targetId).type());
  }

  @Test
  void testConstructorRejectsMissingComponents() {
    UUID value = UUID.randomUUID();

    assertThrows(IllegalArgumentException.class, () -> new ExecutionId(null, value));
    assertThrows(IllegalArgumentException.class, () -> new ExecutionId(ExecutionType.DIALOG, null));
  }
}
