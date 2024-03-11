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

package de.markusbordihn.easynpc;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.UUID;
import net.minecraft.resources.ResourceLocation;

public final class Constants {

  // General Mod definitions
  public static final String LOG_NAME = "Easy NPC";
  public static final String LOG_ICON = "🗣";
  public static final String LOG_CREATE_PREFIX = LOG_ICON + " Create Easy NPC";
  public static final String LOG_REGISTER_PREFIX = LOG_ICON + " Register Easy NPC";
  public static final String MOD_COMMAND = "easy_npc";
  public static final String MOD_ID = "easy_npc";
  public static final String MOD_NAME = "Easy NPC";
  public static final String MOD_URL = "https://www.curseforge.com/minecraft/mc-mods/easy-npc";
  // Prefixes
  public static final String MINECRAFT_PREFIX = "minecraft";
  public static final String TEXT_PREFIX = "text.easy_npc.";
  public static final String TEXT_CONFIG_PREFIX = TEXT_PREFIX + "config.";
  public static final String TEXT_ITEM_PREFIX = TEXT_PREFIX + "item.";
  public static final String CONTAINER_PREFIX = TEXT_PREFIX + "container.";
  // Suffices
  public static final String NPC_NBT_SUFFIX = ".npc.nbt";
  // Colors
  public static final int FONT_COLOR_BLACK = 0;
  public static final int FONT_COLOR_DARK_GREEN = 43520;
  public static final int FONT_COLOR_DEFAULT = 4210752;
  public static final int FONT_COLOR_GRAY = 11184810;
  public static final int FONT_COLOR_GREEN = 5635925;
  public static final int FONT_COLOR_RED = 16733525;
  public static final int FONT_COLOR_WHITE = 16777215;
  public static final int FONT_COLOR_YELLOW = 16777045;
  public static final int FONT_COLOR_LIGHT_GRAY = 10526880;
  // Textures
  public static final UUID BLANK_UUID = new UUID(0L, 0L);
  public static final ResourceLocation BLANK_ENTITY_TEXTURE =
      new ResourceLocation(Constants.MOD_ID, "textures/entity/blank.png");
  public static final ResourceLocation TEXTURE_DEMO_BACKGROUND =
      new ResourceLocation(MINECRAFT_PREFIX, "textures/gui/demo_background.png");
  public static final ResourceLocation TEXTURE_DIALOG =
      new ResourceLocation(Constants.MOD_ID, "textures/gui/dialog.png");

  public static final ResourceLocation TEXTURE_CONFIGURATION =
      new ResourceLocation(Constants.MOD_ID, "textures/gui/configuration.png");
  public static final ResourceLocation TEXTURE_SPAWNER =
      new ResourceLocation(Constants.MOD_ID, "textures/gui/spawner.png");
  public static final ResourceLocation TEXTURE_INVENTORY =
      new ResourceLocation(MINECRAFT_PREFIX, "textures/gui/container/inventory.png");
  // Animation Math
  public static final float MATH_27DEG_TO_RAD = 0.47123894F;
  public static final float MATH_27DEG_TO_RAD_INVERTED = -0.47123894F;

  // Data Specific definitions
  public static final int NPC_DATA_VERSION = 1;

  // Mod definitions for adding additional support and compatibility
  public static final String MOD_ARMOURERS_WORKSHOP_ID = "armourers_workshop";
  public static final String MOD_ARMOURERS_WORKSHOP_NAME = "Armourer's Workshop";

  public static Path GAME_DIR = Paths.get("").toAbsolutePath();

  private Constants() {
  }
}
