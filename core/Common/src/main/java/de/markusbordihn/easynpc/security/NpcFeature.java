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

public enum NpcFeature {
  DIALOG("Dialogs"),
  TRADING("Trading"),
  COMMAND_ACTION("Commands"),
  SCOREBOARD_ACTION("Scoreboard actions"),
  INTERACT_BLOCK_ACTION("Block actions"),
  OPEN_TRADING_ACTION("Trading actions"),
  OBJECTIVE("Objectives"),
  MOVEMENT("Movement"),
  POSITION("Position"),
  COMBAT_ATTRIBUTE("Combat attributes"),
  BASE_ATTRIBUTE("Base attributes"),
  SPAWN_NPC("Spawn NPC"),
  WORLD_PRESET("World preset import/export"),
  CUSTOM_PRESET("Custom preset import/export"),
  LOCAL_PRESET("Local preset export"),
  DEFAULT_PRESET_IMPORT("Default preset import"),
  URL_RESOURCE("URL resource loading");

  private final String displayName;

  NpcFeature(String displayName) {
    this.displayName = displayName;
  }

  public String displayName() {
    return this.displayName;
  }
}
