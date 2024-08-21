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

package de.markusbordihn.easynpc.data.action;

public enum ActionDataType {
  NONE,
  COMMAND,
  CLOSE_DIALOG(false),
  INTERACT_BLOCK,
  OPEN_TRADING_SCREEN(false),
  OPEN_DEFAULT_DIALOG(false),
  OPEN_NAMED_DIALOG;

  private final boolean requiresArgument;

  ActionDataType() {
    this.requiresArgument = true;
  }

  ActionDataType(boolean requiresArgument) {
    this.requiresArgument = requiresArgument;
  }

  public static ActionDataType get(String actionType) {
    if (actionType == null || actionType.isEmpty()) {
      return ActionDataType.NONE;
    }
    try {
      return ActionDataType.valueOf(actionType);
    } catch (IllegalArgumentException e) {
      return ActionDataType.NONE;
    }
  }

  public boolean requiresArgument() {
    return this.requiresArgument;
  }

  public String getId() {
    return "actionDataType." + this.name().toLowerCase();
  }
}
