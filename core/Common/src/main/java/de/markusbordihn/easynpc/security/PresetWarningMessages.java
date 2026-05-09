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

package de.markusbordihn.easynpc.security;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class PresetWarningMessages {

  private PresetWarningMessages() {}

  public static List<String> toPlayerMessages(PresetSanitizationResult result) {
    if (result == null || result.notices() == null || result.notices().isEmpty()) {
      return List.of();
    }

    Set<String> messages = new LinkedHashSet<>();
    for (PresetSanitizationNotice notice : result.notices()) {
      String message = getMessage(notice);
      if (message != null) {
        messages.add(message);
      }
    }

    return new ArrayList<>(messages);
  }

  private static String getMessage(PresetSanitizationNotice notice) {
    return switch (notice) {
      case OWNER_REWRITTEN, OWNER_REMOVED -> "Owner data was updated.";
      case ACTION_PERMISSION_CLAMPED, COMMAND_PERMISSION_CLAMPED -> "Command levels were reduced.";
      case TRADING_REMOVED -> "Trading was removed.";
      case COMMAND_ACTION_REMOVED -> "Command actions were removed.";
      case SCOREBOARD_ACTION_REMOVED -> "Scoreboard actions were removed.";
      case BLOCK_ACTION_REMOVED -> "Block actions were removed.";
      case TRADING_ACTION_REMOVED -> "Trading actions were removed.";
      case OBJECTIVE_REMOVED -> "Objectives were removed.";
      case MOVEMENT_REMOVED -> "Movement data was removed.";
      case POSITION_REMOVED -> "Position data was removed.";
      case ATTRIBUTE_REMOVED -> "Some attribute data was removed.";
    };
  }
}
