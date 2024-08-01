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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;

public enum ConfigurationType {
  NONE,
  ABILITIES_ATTRIBUTE,
  ADVANCED_DIALOG,
  ADVANCED_POSE,
  ADVANCED_TRADING,
  ATTACK_OBJECTIVE,
  BASE_ATTRIBUTE,
  BASIC_ACTION,
  BASIC_DIALOG,
  BASIC_OBJECTIVE,
  BASIC_TRADING,
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
  EQUIPMENT,
  FOLLOW_OBJECTIVE,
  LOCAL_PRESET_IMPORT,
  LOOK_OBJECTIVE,
  MAIN,
  NONE_DIALOG,
  NONE_SKIN,
  NONE_TRADING,
  PLAYER_SKIN,
  POSE(true),
  SCALING,
  SKIN(true),
  TRADING(true),
  URL_SKIN,
  WORLD_PRESET_EXPORT,
  WORLD_PRESET_IMPORT,
  YES_NO_DIALOG;

  private boolean isAlias = false;

  ConfigurationType() {}

  ConfigurationType(boolean isAlias) {
    this.isAlias = isAlias;
  }

  public static ConfigurationType get(String configurationType) {
    if (configurationType == null || configurationType.isEmpty()) {
      return ConfigurationType.NONE;
    }
    try {
      return ConfigurationType.valueOf(configurationType);
    } catch (IllegalArgumentException e) {
      return ConfigurationType.NONE;
    }
  }

  public boolean isAlias() {
    return isAlias;
  }

  public ResourceLocation getId() {
    return new ResourceLocation(Constants.MOD_ID, this.name().toLowerCase() + "_configuration");
  }

  public String getName() {
    return this.name().toLowerCase() + "_configuration";
  }

  public Component getConfigurationTitle(final EasyNPC<?> easyNPC) {
    String translationKey = Constants.TEXT_CONFIG_PREFIX + this.name().toLowerCase() + ".title";
    return new TranslatableComponent(translationKey, easyNPC.getEntity().getName().getString(16));
  }
}
