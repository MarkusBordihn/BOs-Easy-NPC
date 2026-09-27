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

import de.markusbordihn.easynpc.utils.EnumUtils;
import java.util.Locale;

public enum ScoreboardOperation {
  INCREASE("action.increase_value"),
  DECREASE("action.decrease_value"),
  SET("action.set_value");

  private final String commandName = this.name().toLowerCase(Locale.ROOT);
  private final String translationKey;

  ScoreboardOperation(String translationKey) {
    this.translationKey = translationKey;
  }

  public static ScoreboardOperation fromCommandName(String commandName) {
    return EnumUtils.getIgnoreCase(ScoreboardOperation.class, commandName, INCREASE);
  }

  public static ScoreboardOperation fromCommand(String command) {
    if (command == null || command.isEmpty()) {
      return INCREASE;
    }

    String[] parts = command.split(":", 2);
    return fromCommandName(parts[0]);
  }

  public String getCommandName() {
    return this.commandName;
  }

  public String getTranslationKey() {
    return this.translationKey;
  }
}
