/*
 * Copyright 2025 Markus Bordihn
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

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class ActionUtilsTest {

  @Test
  void testParseAction_nullCommand() {
    String result = ActionUtils.parseAction(null, null, null);
    assertEquals("", result);
  }

  @Test
  void testParseAction_emptyCommand() {
    String result = ActionUtils.parseAction("", null, null);
    assertEquals("", result);
  }

  @Test
  void testParseAction_simpleCommand() {
    String result = ActionUtils.parseAction("say hello", null, null);
    // Returns original output which doesn't have the slash added
    assertEquals("say hello", result);
  }

  @ParameterizedTest
  @CsvSource(
      value = {
        "/error_message Test Error|/title @initiator title {\"text\":\"Test Error\",\"color\":\"dark_red\"}",
        "/warn_message Warning|/title @initiator title {\"text\":\"Warning\",\"color\":\"yellow\"}",
        "/info_message Information|/title @initiator title {\"text\":\"Information\",\"color\":\"aqua\"}",
        "/success_message Success|/title @initiator title {\"text\":\"Success\",\"color\":\"green\"}"
      },
      delimiter = '|')
  void testParseAction_messageShortcuts(String input, String expected) {
    String result = ActionUtils.parseAction(input, null, null);
    assertEquals(expected, result);
  }

  @Test
  void testParseAction_messageShortcutWithQuotes() {
    String result = ActionUtils.parseAction("/error_message \"Test\" Error", null, null);
    assertTrue(result.contains("Test"));
    assertTrue(result.contains("\\\""));
  }

  @Test
  void testParseAction_regularCommand() {
    String command = "/give @p diamond 1";
    String result = ActionUtils.parseAction(command, null, null);
    assertEquals(command, result);
  }

  @Test
  void testParseAction_initiatorMacroWithoutPlayer() {
    String command = "/say Hello @initiator";
    String result = ActionUtils.parseAction(command, null, null);
    assertEquals(command, result);
  }

  @Test
  void testParseAction_npcMacroWithoutEntity() {
    String command = "/say Hello from @npc";
    String result = ActionUtils.parseAction(command, null, null);
    assertEquals(command, result);
  }

  @Test
  void testMacroConstants() {
    assertEquals("@initiator", ActionUtils.MACRO_INITIATOR);
    assertEquals("@initiator-uuid", ActionUtils.MACRO_INITIATOR_UUID);
    assertEquals("@npc", ActionUtils.MACRO_NPC);
    assertEquals("@npc-uuid", ActionUtils.MACRO_NPC_UUID);
    assertEquals("/error_message", ActionUtils.MACRO_ERROR_MESSAGE);
    assertEquals("/warn_message", ActionUtils.MACRO_WARN_MESSAGE);
    assertEquals("/info_message", ActionUtils.MACRO_INFO_MESSAGE);
    assertEquals("/success_message", ActionUtils.MACRO_SUCCESS_MESSAGE);
  }

  @Test
  void testParseAction_multipleMacros() {
    String command = "/say @npc says hello to @initiator";
    String result = ActionUtils.parseAction(command, null, null);
    assertEquals(command, result);
  }

  @Test
  void testParseAction_preserveStructure() {
    String command = "/execute as @initiator run say test";
    String result = ActionUtils.parseAction(command, null, null);
    assertTrue(result.startsWith("/execute"));
    assertTrue(result.contains("@initiator"));
  }

  @Test
  void testParseAction_specialCharacters() {
    String command = "/say Hello! @#$%";
    String result = ActionUtils.parseAction(command, null, null);
    assertEquals(command, result);
  }

  @Test
  void testParseAction_trimWhitespace() {
    String result = ActionUtils.parseAction("/error_message   Test   ", null, null);
    assertTrue(result.contains("Test"));
    assertFalse(result.contains("   Test   "));
  }

  @Test
  void testParseAction_scoreMacroWithoutPlayer() {
    String command = "/say Your score is @score(kills)";
    String result = ActionUtils.parseAction(command, null, null);
    assertEquals(command, result);
  }

  @Test
  void testParseAction_jsonEscaping() {
    String result = ActionUtils.parseAction("/error_message Test\\nNew\"Line", null, null);
    assertTrue(result.contains("\\\\"));
    assertTrue(result.contains("\\\""));
    assertTrue(result.contains("\\n"));
  }

  @Test
  void testParseAction_backslashEscaping() {
    String result = ActionUtils.parseAction("/info_message C:\\\\Path\\\\File", null, null);
    assertTrue(result.contains("\\\\"));
  }
}
