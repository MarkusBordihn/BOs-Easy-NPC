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

import java.util.Locale;

public enum UnsafeNpcCommand {
  BAN_IP("ban-ip"),
  BAN("ban"),
  BANLIST("banlist"),
  DEBUG("debug"),
  DEOP("deop"),
  DIFFICULTY("difficulty"),
  FORCELOAD("forceload"),
  FUNCTION("function"),
  GAMERULE("gamerule"),
  KICK("kick"),
  OP("op"),
  PARDON("pardon"),
  RELOAD("reload"),
  SAVE_ALL("save-all"),
  SAVE_OFF("save-off"),
  SAVE_ON("save-on"),
  SCHEDULE("schedule"),
  SET_IDLE_TIMEOUT("setidletimeout"),
  SET_WORLD_SPAWN("setworldspawn"),
  SPAWNPOINT("spawnpoint"),
  SPREADPLAYERS("spreadplayers"),
  STOP("stop"),
  WHITELIST("whitelist");

  private final String commandName;

  UnsafeNpcCommand(String commandName) {
    this.commandName = commandName;
  }

  public static boolean matches(String command) {
    String commandName = extractCommandName(command);
    if (commandName == null) {
      return false;
    }

    for (UnsafeNpcCommand unsafeNpcCommand : values()) {
      if (unsafeNpcCommand.commandName.equals(commandName)) {
        return true;
      }
    }

    return false;
  }

  private static String extractCommandName(String command) {
    if (command == null || command.isBlank()) {
      return null;
    }

    String normalizedCommand = command.trim();
    if (normalizedCommand.startsWith("/")) {
      normalizedCommand = normalizedCommand.substring(1);
    }

    String[] runParts = normalizedCommand.split("\\s+run\\s+");
    String relevantCommand = runParts[runParts.length - 1].trim();
    if (relevantCommand.startsWith("/")) {
      relevantCommand = relevantCommand.substring(1);
    }

    if (relevantCommand.isEmpty()) {
      return null;
    }

    return relevantCommand.split("\\s+")[0].toLowerCase(Locale.ROOT);
  }

  public String commandName() {
    return this.commandName;
  }
}
