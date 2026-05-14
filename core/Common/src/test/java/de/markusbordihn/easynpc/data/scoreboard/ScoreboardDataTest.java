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

package de.markusbordihn.easynpc.data.scoreboard;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Set;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ScoreboardDataTest {

  @Test
  @DisplayName("Should extract single @score macro from text")
  void testExtractObjectiveNames_singleScore() {
    String text = "Your score is @score(test_score).";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertEquals(1, result.size());
    assertTrue(result.contains("test_score"));
  }

  @Test
  @DisplayName("Should extract multiple @score macros from text")
  void testExtractObjectiveNames_multipleScores() {
    String text = "You have @score(kills) kills and @score(deaths) deaths.";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertEquals(2, result.size());
    assertTrue(result.contains("kills"));
    assertTrue(result.contains("deaths"));
  }

  @Test
  void testExtractObjectiveNames_withUnderscores() {
    String text = "Score: @score(test_score_val)";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertEquals(1, result.size());
    assertTrue(result.contains("test_score_val"));
  }

  @Test
  void testExtractObjectiveNames_withHyphens() {
    String text = "Score: @score(test-score-name)";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertEquals(1, result.size());
    assertTrue(result.contains("test-score-name"));
  }

  @Test
  void testExtractObjectiveNames_withDots() {
    String text = "Score: @score(my.score.value)";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertEquals(1, result.size());
    assertTrue(result.contains("my.score.value"));
  }

  @Test
  void testExtractObjectiveNames_noMacros() {
    String text = "This is just plain text without any macros.";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertTrue(result.isEmpty());
  }

  @Test
  void testExtractObjectiveNames_nullText() {
    Set<String> result = ScoreboardData.parseScoreMacros(null);

    assertTrue(result.isEmpty());
  }

  @Test
  void testExtractObjectiveNames_emptyText() {
    Set<String> result = ScoreboardData.parseScoreMacros("");

    assertTrue(result.isEmpty());
  }

  @Test
  void testExtractObjectiveNames_mixedMacros() {
    String text = "Hello @initiator, your score is @score(player_score). Talk to @npc.";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertEquals(1, result.size());
    assertTrue(result.contains("player_score"));
  }

  @Test
  void testExtractObjectiveNames_invalidCharacters() {
    String text = "@score(invalid@name) and @score(bad#score)";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertTrue(result.isEmpty());
  }

  @Test
  void testExtractObjectiveNames_atStart() {
    String text = "@score(first) is your score";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertEquals(1, result.size());
    assertTrue(result.contains("first"));
  }

  @Test
  void testExtractObjectiveNames_atEnd() {
    String text = "Your score is @score(last)";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertEquals(1, result.size());
    assertTrue(result.contains("last"));
  }

  @Test
  void testExtractObjectiveNames_duplicates() {
    String text = "@score(test) and again @score(test) and @score(test)";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertEquals(1, result.size());
    assertTrue(result.contains("test"));
  }

  @Test
  void testExtractObjectiveNames_noParentheses() {
    String text = "@score without parentheses";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertTrue(result.isEmpty());
  }

  @Test
  void testExtractObjectiveNames_emptyParentheses() {
    String text = "@score()";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertTrue(result.isEmpty());
  }

  @Test
  void testExtractObjectiveNames_numericNames() {
    String text = "@score(score123) and @score(456test)";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertEquals(2, result.size());
    assertTrue(result.contains("score123"));
    assertTrue(result.contains("456test"));
  }

  @Test
  @DisplayName("Should accept objective names up to 32 characters")
  void testExtractObjectiveNames_longNames() {
    String text = "@score(easy_npc_test_score) and @score(this_is_exactly_32_characters)";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertEquals(2, result.size());
    assertTrue(result.contains("easy_npc_test_score"));
    assertTrue(result.contains("this_is_exactly_32_characters"));
  }

  @Test
  @DisplayName("Should reject objective names longer than 32 characters")
  void testExtractObjectiveNames_tooLongNames() {
    String text = "@score(this_objective_name_is_way_too_long_more_than_32_chars)";
    Set<String> result = ScoreboardData.parseScoreMacros(text);

    assertTrue(result.isEmpty());
  }
}
