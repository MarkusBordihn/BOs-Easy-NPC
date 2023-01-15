/**
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

import net.minecraft.resources.ResourceLocation;

public final class Constants {

  protected Constants() {}

  // General Mod definitions
  public static final String LOG_NAME = "Easy NPC";
  public static final String LOG_ICON = "🧙";
  public static final String LOG_ICON_NAME = LOG_ICON + " " + LOG_NAME;
  public static final String LOG_REGISTER_PREFIX = LOG_ICON + " Register Easy NPC";
  public static final String MOD_COMMAND = "easy_npc";
  public static final String MOD_ID = "easy_npc";
  public static final String MOD_NAME = "Easy NPC";

  // Prefixes
  public static final String MINECRAFT_PREFIX = "minecraft";
  public static final String TEXT_PREFIX = "text.easy_npc.";

  // Textures
  public static final ResourceLocation TEXTURE_DEMO_BACKGROUND =
      new ResourceLocation(MINECRAFT_PREFIX, "textures/gui/demo_background.png");
  public static final ResourceLocation TEXTURE_DIALOG =
      new ResourceLocation(Constants.MOD_ID, "textures/gui/dialog.png");
  public static final ResourceLocation TEXTURE_GENERIC_54 =
      new ResourceLocation(MINECRAFT_PREFIX, "textures/gui/container/generic_54.png");
  public static final ResourceLocation TEXTURE_ICONS =
      new ResourceLocation(Constants.MOD_ID, "textures/container/icons.png");
}
