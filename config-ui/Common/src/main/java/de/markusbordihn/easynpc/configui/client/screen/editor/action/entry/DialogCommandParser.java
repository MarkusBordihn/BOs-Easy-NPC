/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.configui.client.screen.editor.action.entry;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DialogCommandParser {

  private static final Pattern DIALOG_OPEN_PATTERN =
      Pattern.compile(
          "^/?easy_npc\\s+dialog\\s+open\\s+([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})\\s+\\S+\\s+(\\S+)$");

  private DialogCommandParser() {}

  public static ActionDataEntry parseDialogCommand(String command) {
    if (command == null || command.isEmpty()) {
      return null;
    }

    Matcher matcher = DIALOG_OPEN_PATTERN.matcher(command.trim());
    if (matcher.matches()) {
      try {
        UUID targetUUID = UUID.fromString(matcher.group(1));
        String dialogName = matcher.group(2);
        return new ActionDataEntry(ActionDataType.OPEN_NAMED_DIALOG, targetUUID, dialogName);
      } catch (IllegalArgumentException e) {
        return null;
      }
    }

    return null;
  }

  public static boolean isDialogOpenCommand(String command) {
    return command != null && DIALOG_OPEN_PATTERN.matcher(command.trim()).matches();
  }
}
