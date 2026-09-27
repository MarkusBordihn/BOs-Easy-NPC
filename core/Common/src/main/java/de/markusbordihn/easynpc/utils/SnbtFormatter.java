/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.utils;

public class SnbtFormatter {

  private static final String INDENT = "  ";
  private static final int MAX_DEPTH = 20;

  private SnbtFormatter() {}

  public static String format(String snbt) {
    if (snbt == null || snbt.isEmpty()) {
      return snbt;
    }

    for (int depth = 0; depth < MAX_DEPTH; depth++) {
      String formatted = formatSinglePass(snbt, depth);
      if (formatted.equals(snbt)) {
        break;
      }
      snbt = formatted;
    }

    return snbt;
  }

  private static String formatSinglePass(String snbt, int targetDepth) {
    FormattingContext context = new FormattingContext(snbt, targetDepth);
    int length = snbt.length();

    for (int index = 0; index < length; index++) {
      context.updateCurrentPosition(index);

      if (context.handleStringCharacters()) {
        continue;
      }

      context.processStructureCharacter();
    }

    return context.getFormattedOutput();
  }

  private static boolean shouldToggleStringMode(
      char currentChar, char previousChar, boolean isInsideString, char activeQuoteChar) {
    if (previousChar == '\\') {
      return false;
    }

    if (!isInsideString) {
      return currentChar == '"' || currentChar == '\'';
    }

    return currentChar == activeQuoteChar;
  }

  private static class FormattingContext {
    private final String inputSnbt;
    private final int inputLength;
    private final int targetDepth;
    private final StringBuilder formattedOutput;
    private final StringParsingState state;

    private char currentChar;
    private char nextChar;
    private char previousChar;
    private int currentIndex;

    public FormattingContext(String snbt, int targetDepth) {
      this.inputSnbt = snbt;
      this.inputLength = snbt.length();
      this.targetDepth = targetDepth;
      this.formattedOutput = new StringBuilder(snbt.length() * 2);
      this.state = new StringParsingState();
    }

    public void updateCurrentPosition(int index) {
      this.currentIndex = index;
      this.currentChar = this.inputSnbt.charAt(index);
      this.nextChar = this.getCharAtOrDefault(index + 1);
      this.previousChar = this.getCharAtOrDefault(index - 1);
    }

    public boolean handleStringCharacters() {
      if (this.state.handleQuoteCharacter(this.currentChar, this.previousChar)) {
        this.formattedOutput.append(this.currentChar);
        return true;
      }

      if (this.state.isInsideString()) {
        this.formattedOutput.append(this.currentChar);
        return true;
      }

      return false;
    }

    public void processStructureCharacter() {
      switch (this.currentChar) {
        case '{' -> this.handleOpenBrace();
        case '}' -> this.handleCloseBrace();
        case ',' -> this.handleComma();
        default -> this.formattedOutput.append(this.currentChar);
      }
    }

    private void handleOpenBrace() {
      this.formattedOutput.append(this.currentChar);
      this.state.increaseDepth();
      if (this.state.getCurrentDepth() == this.targetDepth + 1 && this.nextChar != '}') {
        this.formattedOutput.append('\n');
        this.appendIndent(this.state.getCurrentDepth());
      }
    }

    private void handleCloseBrace() {
      if (this.state.getCurrentDepth() == this.targetDepth + 1 && this.previousChar != '{') {
        this.formattedOutput.append('\n');
        this.appendIndent(this.state.getCurrentDepth() - 1);
      }
      this.formattedOutput.append(this.currentChar);
      this.state.decreaseDepth();
    }

    private void handleComma() {
      this.formattedOutput.append(this.currentChar);
      if (this.state.getCurrentDepth() == this.targetDepth + 1
          && !this.isInsideArray(this.currentIndex)) {
        this.formattedOutput.append('\n');
        this.appendIndent(this.state.getCurrentDepth());
      }
    }

    private void appendIndent(int depth) {
      this.formattedOutput.append(INDENT.repeat(Math.max(0, depth)));
    }

    private boolean isInsideArray(int position) {
      int arrayBracketDepth = 0;
      boolean isInsideString = false;
      char activeQuoteChar = '\0';

      for (int index = 0; index < position; index++) {
        char charAtIndex = this.inputSnbt.charAt(index);
        char charBeforeIndex = this.getCharAtOrDefault(index - 1);

        if (shouldToggleStringMode(charAtIndex, charBeforeIndex, isInsideString, activeQuoteChar)) {
          isInsideString = !isInsideString;
          activeQuoteChar = isInsideString ? charAtIndex : '\0';
          continue;
        }

        if (!isInsideString) {
          if (charAtIndex == '[') {
            arrayBracketDepth++;
          } else if (charAtIndex == ']') {
            arrayBracketDepth--;
          }
        }
      }

      return arrayBracketDepth > 0;
    }

    private char getCharAtOrDefault(int index) {
      return index >= 0 && index < this.inputLength ? this.inputSnbt.charAt(index) : '\0';
    }

    public String getFormattedOutput() {
      return this.formattedOutput.toString();
    }
  }

  private static class StringParsingState {
    private int currentDepth = 0;
    private boolean isInsideString = false;
    private char activeQuoteChar = '\0';

    public boolean handleQuoteCharacter(char currentChar, char previousChar) {
      if (!shouldToggleStringMode(
          currentChar, previousChar, this.isInsideString, this.activeQuoteChar)) {
        return false;
      }

      this.isInsideString = !this.isInsideString;
      this.activeQuoteChar = this.isInsideString ? currentChar : '\0';
      return true;
    }

    public boolean isInsideString() {
      return this.isInsideString;
    }

    public int getCurrentDepth() {
      return this.currentDepth;
    }

    public void increaseDepth() {
      this.currentDepth++;
    }

    public void decreaseDepth() {
      this.currentDepth--;
    }
  }
}
