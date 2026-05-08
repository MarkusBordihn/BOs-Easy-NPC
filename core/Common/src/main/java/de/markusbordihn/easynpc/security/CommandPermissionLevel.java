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

import net.minecraft.commands.Commands;

public enum CommandPermissionLevel {
  ALL(Commands.LEVEL_ALL),
  MODERATORS(Commands.LEVEL_MODERATORS),
  GAMEMASTERS(Commands.LEVEL_GAMEMASTERS),
  ADMINS(Commands.LEVEL_ADMINS),
  OWNERS(Commands.LEVEL_OWNERS);

  private final int minecraftLevel;

  CommandPermissionLevel(int minecraftLevel) {
    this.minecraftLevel = minecraftLevel;
  }

  public static CommandPermissionLevel fromMinecraftLevel(int minecraftLevel) {
    CommandPermissionLevel result = ALL;
    for (CommandPermissionLevel permissionLevel : values()) {
      if (minecraftLevel >= permissionLevel.minecraftLevel) {
        result = permissionLevel;
      }
    }
    return result;
  }

  public static CommandPermissionLevel parse(String value, CommandPermissionLevel defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }

    String normalizedValue = value.trim();
    try {
      return fromMinecraftLevel(Integer.parseInt(normalizedValue));
    } catch (NumberFormatException ignored) {
      // Fall through to enum parsing for the canonical config representation.
    }

    try {
      return valueOf(normalizedValue.toUpperCase(java.util.Locale.ROOT));
    } catch (IllegalArgumentException exception) {
      return defaultValue;
    }
  }

  public static CommandPermissionLevel min(
      CommandPermissionLevel first, CommandPermissionLevel second) {
    if (first == null) {
      return second;
    }

    if (second == null) {
      return first;
    }

    return first.minecraftLevel <= second.minecraftLevel ? first : second;
  }

  public static CommandPermissionLevel max(
      CommandPermissionLevel first, CommandPermissionLevel second) {
    if (first == null) {
      return second;
    }

    if (second == null) {
      return first;
    }

    return first.minecraftLevel >= second.minecraftLevel ? first : second;
  }

  public int minecraftLevel() {
    return this.minecraftLevel;
  }

  public boolean allows(CommandPermissionLevel required) {
    return required != null && this.minecraftLevel >= required.minecraftLevel;
  }

  public CommandPermissionLevel clamp(CommandPermissionLevel min, CommandPermissionLevel max) {
    return max(min != null ? min : ALL, min(this, max != null ? max : OWNERS));
  }
}
