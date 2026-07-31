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

package de.markusbordihn.easynpc.data.state;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.resources.ResourceLocation;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class StateIdentifierTest {

  @Test
  @DisplayName("A state without a namespace belongs to easy_npc and not to minecraft")
  void testDefaultNamespace() {
    ResourceLocation stateId = StateIdentifier.parse("quest_progress");

    assertEquals(StateIdentifier.DEFAULT_NAMESPACE, stateId.getNamespace());
    assertEquals("quest_progress", stateId.getPath());
  }

  @Test
  void testExplicitNamespaceIsKept() {
    ResourceLocation stateId = StateIdentifier.parse("my_mod:stage");

    assertEquals("my_mod", stateId.getNamespace());
    assertEquals("stage", stateId.getPath());
  }

  @Test
  void testSurroundingWhitespaceIsIgnored() {
    assertEquals(StateIdentifier.parse("quest"), StateIdentifier.parse("  quest  "));
  }

  @Test
  @DisplayName("An upper case or spaced name is rejected instead of silently doing nothing")
  void testInvalidNames() {
    assertNull(StateIdentifier.parse("questProgress"));
    assertNull(StateIdentifier.parse("quest progress"));
    assertNull(StateIdentifier.parse("my_mod:Stage"));
    assertNull(StateIdentifier.parse(""));
    assertNull(StateIdentifier.parse("  "));
    assertNull(StateIdentifier.parse(null));
  }

  @Test
  void testLengthLimit() {
    assertTrue(StateIdentifier.isValid("a".repeat(StateIdentifier.MAX_LENGTH)));
    assertFalse(StateIdentifier.isValid("a".repeat(StateIdentifier.MAX_LENGTH + 1)));
  }

  @Test
  @DisplayName("A partial input stays typeable while an invalid character is refused")
  void testInputFilter() {
    assertTrue(StateIdentifier.isValidInput(""));
    assertTrue(StateIdentifier.isValidInput("my_mod:"));
    assertTrue(StateIdentifier.isValidInput("quest.step-1/a"));
    assertFalse(StateIdentifier.isValidInput("Quest"));
    assertFalse(StateIdentifier.isValidInput("quest progress"));
    assertFalse(StateIdentifier.isValidInput("a".repeat(StateIdentifier.MAX_LENGTH + 1)));
    assertFalse(StateIdentifier.isValidInput(null));
  }

  @Test
  @DisplayName("A name that is typeable but not usable is refused when it is saved")
  void testTypeableButUnusableName() {
    assertTrue(StateIdentifier.isValidInput("my_mod:stage:extra"));
    assertFalse(StateIdentifier.isValid("my_mod:stage:extra"));
    assertTrue(StateIdentifier.isValidInput("my_mod:"));
    assertFalse(StateIdentifier.isValid("my_mod:"));
  }
}
