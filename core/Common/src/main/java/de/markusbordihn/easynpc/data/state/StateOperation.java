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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.utils.EnumUtils;
import java.util.Locale;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public enum StateOperation {
  SET,
  INCREASE,
  DECREASE,
  TOGGLE,
  REMOVE;

  private final String commandName = this.name().toLowerCase(Locale.ROOT);

  private static final int DEFAULT_AMOUNT = 1;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static StateOperation get(String stateOperation) {
    return EnumUtils.getIgnoreCase(StateOperation.class, stateOperation, SET);
  }

  public static StateOperation find(String stateOperation) {
    return EnumUtils.getIgnoreCase(StateOperation.class, stateOperation, null);
  }

  private static StateEntry add(StateEntry currentStateEntry, int amount) {
    if (currentStateEntry != null && currentStateEntry.isText()) {
      log.warn(
          "State with the text '{}' becomes a number by counting it",
          currentStateEntry.textValue());
    }

    long updatedValue = (currentStateEntry != null ? currentStateEntry.asNumber() : 0L) + amount;
    return StateEntry.of(
        (int) Math.max(Integer.MIN_VALUE, Math.min(Integer.MAX_VALUE, updatedValue)));
  }

  private static int parseAmount(String value) {
    try {
      return Integer.parseInt(value.trim());
    } catch (NumberFormatException ignored) {
      log.warn("Unable to read '{}' as a number, using {} instead", value, DEFAULT_AMOUNT);
      return DEFAULT_AMOUNT;
    }
  }

  public String getCommandName() {
    return this.commandName;
  }

  public boolean requiresValue() {
    return this == SET || this == INCREASE || this == DECREASE;
  }

  public StateEntry apply(StateEntry currentStateEntry, String value) {
    return this.apply(currentStateEntry, null, value);
  }

  public StateEntry apply(StateEntry currentStateEntry, StateValueType valueType, String value) {
    return switch (this) {
      case SET -> (valueType != null ? valueType : StateValueType.detect(value)).parse(value);
      case INCREASE -> add(currentStateEntry, parseAmount(value));
      case DECREASE -> add(currentStateEntry, -parseAmount(value));
      case TOGGLE -> StateEntry.of(currentStateEntry == null || !currentStateEntry.asFlag());
      case REMOVE -> null;
    };
  }
}
