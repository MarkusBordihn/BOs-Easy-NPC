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

package de.markusbordihn.easynpc.data.dialog;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("DialogPriority Tests")
class DialogPriorityTest {

  @Test
  @DisplayName("Should calculate HIGH priority for 'default' label")
  void testDefaultLabel() {
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("default"));
  }

  @Test
  @DisplayName("Should calculate HIGH priority for 'Default' label (case insensitive)")
  void testDefaultLabelCaseInsensitive() {
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("Default"));
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("DEFAULT"));
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("DeFaUlT"));
  }

  @Test
  @DisplayName("Should calculate HIGH priority for 'start' label")
  void testStartLabel() {
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("start"));
  }

  @Test
  @DisplayName("Should calculate HIGH priority for 'Start' label (case insensitive)")
  void testStartLabelCaseInsensitive() {
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("Start"));
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("START"));
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("StArT"));
  }

  @Test
  @DisplayName("Should calculate HIGH priority for 'welcome' label")
  void testWelcomeLabel() {
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("welcome"));
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("Welcome"));
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("WELCOME"));
  }

  @Test
  @DisplayName("Should calculate HIGH priority for 'greeting' label")
  void testGreetingLabel() {
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("greeting"));
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("Greeting"));
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("GREETING"));
  }

  @Test
  @DisplayName("Should calculate HIGH priority for 'intro' and 'introduction' labels")
  void testIntroLabels() {
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("intro"));
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("Intro"));
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("introduction"));
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("Introduction"));
  }

  @Test
  @DisplayName("Should calculate NORMAL priority for 'main' label")
  void testMainLabel() {
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("main"));
  }

  @Test
  @DisplayName("Should calculate NORMAL priority for 'Main' label (case insensitive)")
  void testMainLabelCaseInsensitive() {
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("Main"));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("MAIN"));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("MaIn"));
  }

  @Test
  @DisplayName("Should calculate NORMAL priority for 'question' label")
  void testQuestionLabel() {
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("question"));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("Question"));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("QUESTION"));
  }

  @Test
  @DisplayName("Should calculate NORMAL priority for 'help' label")
  void testHelpLabel() {
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("help"));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("Help"));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("HELP"));
  }

  @Test
  @DisplayName("Should calculate NORMAL priority for 'info' and 'information' labels")
  void testInfoLabels() {
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("info"));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("Info"));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("information"));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("Information"));
  }

  @Test
  @DisplayName("Should calculate NORMAL priority for 'talk' and 'conversation' labels")
  void testTalkLabels() {
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("talk"));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("Talk"));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("conversation"));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("Conversation"));
  }

  @Test
  @DisplayName("Should calculate LOW priority for 'bye' and 'goodbye' labels")
  void testByeLabels() {
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("bye"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("Bye"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("goodbye"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("Goodbye"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("farewell"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("Farewell"));
  }

  @Test
  @DisplayName("Should calculate LOW priority for 'exit' and 'leave' labels")
  void testExitLabels() {
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("exit"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("Exit"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("leave"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("Leave"));
  }

  @Test
  @DisplayName("Should calculate LOW priority for 'thanks' and 'thankyou' labels")
  void testThanksLabels() {
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("thanks"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("Thanks"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("thankyou"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("ThankYou"));
  }

  @Test
  @DisplayName("Should calculate LOW priority for 'idle' and 'random' labels")
  void testIdleAndRandomLabels() {
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("idle"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("Idle"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("random"));
    assertEquals(DialogPriority.LOW, DialogPriority.calculateDefaultPriority("Random"));
  }

  @Test
  @DisplayName("Should calculate FALLBACK priority for custom labels")
  void testCustomLabels() {
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("quest_1"));
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("custom_dialog"));
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("special"));
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("trader"));
  }

  @Test
  @DisplayName("Should handle labels with whitespace")
  void testLabelsWithWhitespace() {
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("  default  "));
    assertEquals(DialogPriority.HIGH, DialogPriority.calculateDefaultPriority("  start  "));
    assertEquals(DialogPriority.NORMAL, DialogPriority.calculateDefaultPriority("  main  "));
  }

  @Test
  @DisplayName("Should calculate FALLBACK priority for null label")
  void testNullLabel() {
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority(null));
  }

  @Test
  @DisplayName("Should calculate FALLBACK priority for empty label")
  void testEmptyLabel() {
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority(""));
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("   "));
  }

  @Test
  @DisplayName("Should calculate FALLBACK priority for similar but different labels")
  void testSimilarButDifferentLabels() {
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("defaults"));
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("started"));
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("mainly"));
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("_default"));
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("default_"));
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("welcomed"));
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("questions"));
    assertEquals(DialogPriority.FALLBACK, DialogPriority.calculateDefaultPriority("helper"));
  }

  @Test
  @DisplayName("Should return correct name for each priority level")
  void testGetNameForPriority() {
    assertEquals("Manual Only", DialogPriority.getNameForPriority(DialogPriority.MANUAL_ONLY));
    assertEquals("Fallback", DialogPriority.getNameForPriority(DialogPriority.FALLBACK));
    assertEquals("Low", DialogPriority.getNameForPriority(DialogPriority.LOW));
    assertEquals("Normal", DialogPriority.getNameForPriority(DialogPriority.NORMAL));
    assertEquals("High", DialogPriority.getNameForPriority(DialogPriority.HIGH));
    assertEquals("Critical", DialogPriority.getNameForPriority(DialogPriority.CRITICAL));
  }

  @Test
  @DisplayName("Should return 'Custom' for unknown priority values")
  void testGetNameForCustomPriority() {
    assertEquals("Custom", DialogPriority.getNameForPriority(2));
    assertEquals("Custom", DialogPriority.getNameForPriority(7));
    assertEquals("Custom", DialogPriority.getNameForPriority(50));
    assertEquals("Custom", DialogPriority.getNameForPriority(-5));
  }

  @Test
  @DisplayName("Should return correct display name with priority value")
  void testGetDisplayName() {
    assertEquals("Manual Only (-1)", DialogPriority.getDisplayName(DialogPriority.MANUAL_ONLY));
    assertEquals("Fallback (0)", DialogPriority.getDisplayName(DialogPriority.FALLBACK));
    assertEquals("Low (1)", DialogPriority.getDisplayName(DialogPriority.LOW));
    assertEquals("Normal (5)", DialogPriority.getDisplayName(DialogPriority.NORMAL));
    assertEquals("High (10)", DialogPriority.getDisplayName(DialogPriority.HIGH));
    assertEquals("Critical (100)", DialogPriority.getDisplayName(DialogPriority.CRITICAL));
  }

  @Test
  @DisplayName("Should return custom display name for unknown priority values")
  void testGetDisplayNameCustom() {
    assertEquals("Custom (3)", DialogPriority.getDisplayName(3));
    assertEquals("Custom (42)", DialogPriority.getDisplayName(42));
  }

  @Test
  @DisplayName("Should verify priority constants are correctly ordered")
  void testPriorityOrdering() {
    assertTrue(DialogPriority.MANUAL_ONLY < DialogPriority.FALLBACK);
    assertTrue(DialogPriority.FALLBACK < DialogPriority.LOW);
    assertTrue(DialogPriority.LOW < DialogPriority.NORMAL);
    assertTrue(DialogPriority.NORMAL < DialogPriority.HIGH);
    assertTrue(DialogPriority.HIGH < DialogPriority.CRITICAL);
  }

  @Test
  @DisplayName("Should verify priority constants have expected values")
  void testPriorityValues() {
    assertEquals(-1, DialogPriority.MANUAL_ONLY);
    assertEquals(0, DialogPriority.FALLBACK);
    assertEquals(1, DialogPriority.LOW);
    assertEquals(5, DialogPriority.NORMAL);
    assertEquals(10, DialogPriority.HIGH);
    assertEquals(100, DialogPriority.CRITICAL);
  }
}
