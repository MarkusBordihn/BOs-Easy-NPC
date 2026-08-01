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

import com.mojang.datafixers.util.Either;
import com.mojang.serialization.Codec;

public record StateEntry(int numberValue, String textValue) {

  public static final int MAX_TEXT_VALUE_LENGTH = 64;
  public static final StateEntry EMPTY = new StateEntry(0, "");
  public static final Codec<StateEntry> CODEC =
      Codec.either(Codec.INT, Codec.STRING)
          .xmap(
              either -> either.map(StateEntry::of, StateEntry::of),
              stateEntry ->
                  stateEntry.isText()
                      ? Either.right(stateEntry.textValue())
                      : Either.left(stateEntry.numberValue()));

  public StateEntry {
    textValue = textValue == null ? "" : textValue;
    numberValue = textValue.isEmpty() ? numberValue : 0;
  }

  public static StateEntry of(int numberValue) {
    return new StateEntry(numberValue, "");
  }

  public static StateEntry of(String textValue) {
    if (textValue == null) {
      return EMPTY;
    }

    String trimmedTextValue = textValue.trim();
    if (trimmedTextValue.isEmpty()) {
      return EMPTY;
    }

    return new StateEntry(
        0,
        trimmedTextValue.length() > MAX_TEXT_VALUE_LENGTH
            ? trimmedTextValue.substring(0, MAX_TEXT_VALUE_LENGTH)
            : trimmedTextValue);
  }

  public static StateEntry of(boolean flagValue) {
    return of(flagValue ? 1 : 0);
  }

  public boolean isNumber() {
    return this.textValue.isEmpty();
  }

  public boolean isText() {
    return !this.textValue.isEmpty();
  }

  public boolean asFlag() {
    return this.isText() || this.numberValue != 0;
  }

  public int asNumber() {
    return this.numberValue;
  }

  public String asText() {
    return this.textValue;
  }
}
