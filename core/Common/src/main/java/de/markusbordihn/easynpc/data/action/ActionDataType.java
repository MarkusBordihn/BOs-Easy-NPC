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

import de.markusbordihn.easynpc.utils.EnumUtils;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Locale;
import java.util.Set;

public enum ActionDataType {
  NONE,
  COMMAND,
  CLOSE_DIALOG(false),
  INTERACT_BLOCK,
  OPEN_TRADING_SCREEN(false),
  OPEN_DEFAULT_DIALOG(false),
  OPEN_NAMED_DIALOG,
  OPEN_NAMED_DIALOG_CONDITIONAL,
  SCOREBOARD,
  NPC_STATE,
  SET_POSE,
  RESET_POSE(false),
  PLAY_ANIMATION,
  STOP_ANIMATION(false),
  RESTART_ANIMATION(false),
  MESSAGE,
  SOUND,
  WAIT,
  CUSTOM,
  MOVE_TO,
  MOVE_TO_AND_WAIT,
  SET_OPACITY;

  private static final Set<ActionDataType> BLOCKING_ACTION_TYPES =
      Collections.unmodifiableSet(EnumSet.of(WAIT, MOVE_TO_AND_WAIT));

  private final boolean requiresArgument;
  private final String id = "actionDataType." + this.name().toLowerCase(Locale.ROOT);

  ActionDataType() {
    this.requiresArgument = true;
  }

  ActionDataType(boolean requiresArgument) {
    this.requiresArgument = requiresArgument;
  }

  public static ActionDataType get(String actionType) {
    return EnumUtils.get(ActionDataType.class, actionType, NONE);
  }

  public static Set<ActionDataType> getBlockingTypes() {
    return BLOCKING_ACTION_TYPES;
  }

  public boolean requiresArgument() {
    return this.requiresArgument;
  }

  public boolean isBlocking() {
    return BLOCKING_ACTION_TYPES.contains(this);
  }

  public String getId() {
    return this.id;
  }
}
