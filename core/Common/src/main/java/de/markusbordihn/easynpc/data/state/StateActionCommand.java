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

public record StateActionCommand(
    StateOperation operation, String stateName, StateValueType valueType, String value) {

  public static final StateActionCommand EMPTY =
      new StateActionCommand(StateOperation.SET, "", StateValueType.NUMBER, "1");

  public static StateActionCommand parse(String command) {
    String[] arguments = command != null ? command.trim().split("\\s+", 3) : new String[0];
    if (arguments.length == 0 || arguments[0].isEmpty()) {
      return EMPTY;
    }

    StateOperation operation = StateOperation.find(arguments[0]);
    String stateName = arguments.length > 1 ? arguments[1] : "";
    String value = arguments.length > 2 ? arguments[2] : "";
    if (operation != StateOperation.SET) {
      return new StateActionCommand(operation, stateName, StateValueType.NUMBER, value);
    }

    String[] valueArguments = value.split("\\s+", 2);
    StateValueType valueType = StateValueType.find(valueArguments[0]);
    if (valueType != null && valueArguments.length > 1) {
      return new StateActionCommand(operation, stateName, valueType, valueArguments[1]);
    }

    return new StateActionCommand(operation, stateName, StateValueType.detect(value), value);
  }

  public boolean hasStateName() {
    return this.stateName != null && !this.stateName.trim().isEmpty();
  }

  public boolean hasValue() {
    return this.value != null && !this.value.trim().isEmpty();
  }

  public StateEntry apply(StateEntry currentStateEntry) {
    return this.operation.apply(currentStateEntry, this.valueType, this.value);
  }

  public String toCommand() {
    StringBuilder command =
        new StringBuilder(this.operation.getCommandName())
            .append(' ')
            .append(this.stateName.trim());
    if (!this.operation.requiresValue()) {
      return command.toString();
    }

    if (this.operation == StateOperation.SET) {
      command.append(' ').append(this.valueType.getCommandName());
    }

    return command.append(' ').append(this.value.trim()).toString();
  }
}
