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

package de.markusbordihn.easynpc.configui;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.UUID;
import net.minecraft.resources.ResourceLocation;

public final class Constants {

  public static final String MOD_ID = "easy_npc_config_ui";
  public static final String MOD_NAME = "Easy NPC: Config UI";
  public static final String MOD_COMMAND = "easy_npc_config_ui";
  public static final String MOD_PREFIX = MOD_ID + ".";
  public static final String MOD_PREFIX_ID = MOD_ID + ":";
  public static final String LOG_ICON = "🗣";
  public static final String LOG_NAME = "Easy NPC: Config UI";
  public static final String LOG_REGISTER_PREFIX = LOG_ICON + " Register " + LOG_NAME;
  public static final UUID EMPTY_UUID = new UUID(0L, 0L);

  public static final String TEXT_PREFIX = "text." + MOD_ID + ".";
  public static final String TEXT_CONFIG_PREFIX = TEXT_PREFIX + "config.";
  public static final String TEXT_ITEM_PREFIX = TEXT_PREFIX + "item.";
  public static final String MINECRAFT_PREFIX = "minecraft";
  public static final String MINECRAFT_RESOURCE_PREFIX = MINECRAFT_PREFIX + ":";
  public static final ResourceLocation TEXTURE_CONFIGURATION =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "textures/gui/configuration.png");
  public static final ResourceLocation TEXTURE_DEMO_BACKGROUND =
      ResourceLocation.fromNamespaceAndPath(MINECRAFT_PREFIX, "textures/gui/demo_background.png");
  public static final ResourceLocation TEXTURE_INVENTORY =
      ResourceLocation.fromNamespaceAndPath(
          MINECRAFT_PREFIX, "textures/gui/container/inventory.png");

  public static final int FONT_COLOR_BLACK = 0;
  public static final int FONT_COLOR_DARK_GREEN = 43520;
  public static final int FONT_COLOR_DEFAULT = 4210752;
  public static final int FONT_COLOR_GRAY = 11184810;
  public static final int FONT_COLOR_GREEN = 5635925;
  public static final int FONT_COLOR_LIGHT_GRAY = 10526880;
  public static final int FONT_COLOR_RED = 16733525;
  public static final int FONT_COLOR_WHITE = 16777215;
  public static final int FONT_COLOR_YELLOW = 16777045;

  // Directories
  public static Path GAME_DIR = Paths.get("").toAbsolutePath();
  public static Path CONFIG_DIR = GAME_DIR.resolve("config");
  public static Path WORLD_DIR = GAME_DIR.resolve("world");

  private Constants() {}
}
