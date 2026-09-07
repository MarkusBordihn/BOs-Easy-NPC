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

package de.markusbordihn.easynpc.data.configuration;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.utils.EnumUtils;
import java.util.Locale;
import net.minecraft.resources.ResourceLocation;

public enum ConfigurationType {
  NONE,
  ABILITIES_ATTRIBUTE,
  ADVANCED_DIALOG,
  ADVANCED_POSE,
  ADVANCED_SKIN,
  ADVANCED_TRADING,
  ATTACK_OBJECTIVE,
  BASE_ATTRIBUTE,
  BASIC_ACTION,
  BASIC_DIALOG,
  BASIC_OBJECTIVE,
  BASIC_POSE,
  BASIC_SOUND,
  BASIC_TRADING,
  COMBAT_ATTRIBUTE,
  COMBAT_SOUND,
  COBBLEMON_MODEL,
  CUSTOM_MODEL,
  CUSTOM_POSE,
  CUSTOM_PRESET_EXPORT,
  CUSTOM_PRESET_IMPORT,
  CUSTOM_SKIN,
  CUSTOM_TRADING,
  DEFAULT_MODEL,
  DEFAULT_POSE,
  DEFAULT_POSITION,
  DEFAULT_PRESET_IMPORT,
  DEFAULT_ROTATION,
  DEFAULT_SKIN,
  DIALOG(true),
  DIALOG_ACTION,
  DISPLAY_ATTRIBUTE,
  DISTANCE_ACTION,
  EASY_MODEL_ENTITIES_MODEL,
  EQUIPMENT,
  FLEE_OBJECTIVE,
  INTERACTION_SOUND,
  INTERVAL_ACTION,
  FOLLOW_OBJECTIVE,
  LOCAL_PRESET_EXPORT,
  LOCAL_PRESET_IMPORT,
  LOOK_OBJECTIVE,
  MAIN,
  MISC_ATTRIBUTE,
  NONE_DIALOG,
  NONE_TRADING,
  PLAYER_SKIN,
  POSE(true),
  SCALING,
  SKIN(true),
  TARGET_OBJECTIVE,
  TRADE_SOUND,
  TRADING(true),
  URL_SKIN,
  WORLD_PRESET_EXPORT,
  WORLD_PRESET_IMPORT,
  YES_NO_DIALOG;

  private final String configurationName = this.name().toLowerCase(Locale.ROOT) + "_configuration";
  private final ResourceLocation id =
      new ResourceLocation(Constants.MOD_ID, this.configurationName);
  private boolean isAlias = false;

  ConfigurationType() {}

  ConfigurationType(boolean isAlias) {
    this.isAlias = isAlias;
  }

  public static ConfigurationType get(String configurationType) {
    return EnumUtils.get(ConfigurationType.class, configurationType, NONE);
  }

  public boolean isAlias() {
    return isAlias;
  }

  public ResourceLocation getId() {
    return this.id;
  }

  public String getName() {
    return this.configurationName;
  }
}
