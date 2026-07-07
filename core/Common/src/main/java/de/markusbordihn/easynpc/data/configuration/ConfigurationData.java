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

package de.markusbordihn.easynpc.data.configuration;

import java.util.Collections;
import java.util.EnumSet;
import java.util.Set;

public record ConfigurationData(
    Set<ConfigurationType> enabledTypes, Set<ConfigurationType> experimentalTypes) {

  private static final Set<ConfigurationType> ALL_TYPES = EnumSet.allOf(ConfigurationType.class);
  public static final ConfigurationData STANDARD =
      new ConfigurationData(
          exclude(
              ALL_TYPES,
              ConfigurationType.PLAYER_SKIN,
              ConfigurationType.CUSTOM_MODEL,
              ConfigurationType.COBBLEMON_MODEL,
              ConfigurationType.EASY_MODEL_ENTITIES_MODEL));
  public static final ConfigurationData RAW =
      new ConfigurationData(
          exclude(
              STANDARD.enabledTypes(),
              ConfigurationType.BASIC_OBJECTIVE,
              ConfigurationType.ATTACK_OBJECTIVE,
              ConfigurationType.FLEE_OBJECTIVE,
              ConfigurationType.FOLLOW_OBJECTIVE,
              ConfigurationType.LOOK_OBJECTIVE,
              ConfigurationType.DEFAULT_MODEL));
  public static final ConfigurationData EPIC_FIGHT =
      new ConfigurationData(
          exclude(
              STANDARD.enabledTypes(),
              ConfigurationType.ADVANCED_POSE,
              ConfigurationType.BASIC_POSE,
              ConfigurationType.DEFAULT_POSE,
              ConfigurationType.POSE,
              ConfigurationType.DEFAULT_ROTATION,
              ConfigurationType.DEFAULT_MODEL,
              ConfigurationType.SCALING,
              ConfigurationType.BASIC_OBJECTIVE,
              ConfigurationType.ATTACK_OBJECTIVE,
              ConfigurationType.FLEE_OBJECTIVE,
              ConfigurationType.FOLLOW_OBJECTIVE,
              ConfigurationType.LOOK_OBJECTIVE),
          EnumSet.of(
              ConfigurationType.DEFAULT_ROTATION,
              ConfigurationType.SCALING,
              ConfigurationType.BASIC_OBJECTIVE,
              ConfigurationType.ATTACK_OBJECTIVE,
              ConfigurationType.FLEE_OBJECTIVE,
              ConfigurationType.FOLLOW_OBJECTIVE,
              ConfigurationType.LOOK_OBJECTIVE));
  public static final ConfigurationData DOPPLER =
      new ConfigurationData(
          include(
              exclude(
                  STANDARD.enabledTypes(),
                  ConfigurationType.ADVANCED_POSE,
                  ConfigurationType.BASIC_POSE,
                  ConfigurationType.DEFAULT_POSE,
                  ConfigurationType.POSE,
                  ConfigurationType.SKIN,
                  ConfigurationType.DEFAULT_SKIN,
                  ConfigurationType.CUSTOM_SKIN,
                  ConfigurationType.PLAYER_SKIN,
                  ConfigurationType.URL_SKIN,
                  ConfigurationType.NONE_SKIN,
                  ConfigurationType.SCALING,
                  ConfigurationType.DEFAULT_ROTATION),
              ConfigurationType.CUSTOM_MODEL));

  public static final ConfigurationData COBBLEMON =
      new ConfigurationData(
          include(
              exclude(
                  STANDARD.enabledTypes(),
                  ConfigurationType.ADVANCED_POSE,
                  ConfigurationType.BASIC_POSE,
                  ConfigurationType.DEFAULT_POSE,
                  ConfigurationType.POSE,
                  ConfigurationType.SKIN,
                  ConfigurationType.DEFAULT_SKIN,
                  ConfigurationType.CUSTOM_SKIN,
                  ConfigurationType.PLAYER_SKIN,
                  ConfigurationType.URL_SKIN,
                  ConfigurationType.NONE_SKIN,
                  ConfigurationType.SCALING,
                  ConfigurationType.DEFAULT_ROTATION),
              ConfigurationType.COBBLEMON_MODEL));
  public static final ConfigurationData EASY_MODEL =
      new ConfigurationData(
          include(
              exclude(
                  STANDARD.enabledTypes(),
                  ConfigurationType.ADVANCED_POSE,
                  ConfigurationType.BASIC_POSE,
                  ConfigurationType.DEFAULT_POSE,
                  ConfigurationType.SKIN,
                  ConfigurationType.DEFAULT_SKIN,
                  ConfigurationType.CUSTOM_SKIN,
                  ConfigurationType.PLAYER_SKIN,
                  ConfigurationType.URL_SKIN,
                  ConfigurationType.NONE_SKIN,
                  ConfigurationType.DEFAULT_ROTATION),
              ConfigurationType.EASY_MODEL_ENTITIES_MODEL));
  public static final ConfigurationData HUMANOID =
      new ConfigurationData(
          exclude(
              ALL_TYPES,
              ConfigurationType.CUSTOM_MODEL,
              ConfigurationType.COBBLEMON_MODEL,
              ConfigurationType.EASY_MODEL_ENTITIES_MODEL));

  public ConfigurationData(Set<ConfigurationType> enabledTypes) {
    this(enabledTypes, Collections.emptySet());
  }

  private static EnumSet<ConfigurationType> exclude(
      Set<ConfigurationType> base, ConfigurationType... types) {
    EnumSet<ConfigurationType> result = EnumSet.copyOf(base);
    for (ConfigurationType type : types) {
      result.remove(type);
    }
    return result;
  }

  private static EnumSet<ConfigurationType> include(
      Set<ConfigurationType> base, ConfigurationType... types) {
    EnumSet<ConfigurationType> result = EnumSet.copyOf(base);
    Collections.addAll(result, types);
    return result;
  }

  public boolean isEnabled(ConfigurationType type) {
    return enabledTypes.contains(type);
  }

  public boolean isExperimental(ConfigurationType type) {
    return experimentalTypes.contains(type);
  }

  public boolean hasExperimentalTypes() {
    return !experimentalTypes.isEmpty();
  }
}
