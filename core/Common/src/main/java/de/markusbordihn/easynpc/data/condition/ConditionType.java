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

package de.markusbordihn.easynpc.data.condition;

import de.markusbordihn.easynpc.data.state.StateValueType;
import de.markusbordihn.easynpc.utils.EnumUtils;
import java.util.EnumSet;
import java.util.Set;

public enum ConditionType {
  NONE(ConditionTypeRequirements.NONE),
  SCOREBOARD(ConditionTypeRequirements.NAME_VALUE_OPERATION),
  EXECUTION_LIMIT(ConditionTypeRequirements.VALUE_ONLY, DurationType.class),
  CHANCE(ConditionTypeRequirements.VALUE_ONLY),
  HAS_ITEM_IN_INVENTORY(ConditionTypeRequirements.NAME_ONLY),
  HAS_ITEM_IN_HAND(ConditionTypeRequirements.NAME_ONLY, HandItemType.class),
  ADVANCEMENT(ConditionTypeRequirements.NAME_ONLY),
  EXPERIENCE_LEVEL(ConditionTypeRequirements.VALUE_AND_OPERATION),
  PLAYER_HEALTH(ConditionTypeRequirements.VALUE_AND_OPERATION),
  NPC_HEALTH(ConditionTypeRequirements.VALUE_AND_OPERATION),
  ENTITY_HEALTH(ConditionTypeRequirements.NAME_VALUE_OPERATION),
  PLAYER_TAG(ConditionTypeRequirements.NAME_ONLY),
  TEAM(ConditionTypeRequirements.NAME_ONLY),
  GAMEMODE(ConditionTypeRequirements.NAME_ONLY),
  PLAYER_IDLE(ConditionTypeRequirements.VALUE_AND_OPERATION),
  TIME_OF_DAY(ConditionTypeRequirements.VALUE_AND_OPERATION),
  WEATHER(ConditionTypeRequirements.NONE, WeatherType.class),
  NPC_STATE(ConditionTypeRequirements.NAME_VALUE_OPERATION, StateValueType.class),
  RELATIONSHIP(ConditionTypeRequirements.NONE, RelationshipType.class),
  FALLBACK(ConditionTypeRequirements.NONE),
  CUSTOM(ConditionTypeRequirements.NONE),
  ;

  /** Conditions outside this set require an initiator. */
  private static final Set<ConditionType> PLAYER_INDEPENDENT_TYPES =
      EnumSet.of(
          NONE,
          NPC_HEALTH,
          ENTITY_HEALTH,
          NPC_STATE,
          TIME_OF_DAY,
          WEATHER,
          CHANCE,
          FALLBACK,
          CUSTOM);

  private final ConditionTypeRequirements requirements;
  private final Class<? extends ConditionSubTypeEntry> subTypeClass;

  ConditionType(ConditionTypeRequirements requirements) {
    this(requirements, null);
  }

  ConditionType(
      ConditionTypeRequirements requirements, Class<? extends ConditionSubTypeEntry> subTypeClass) {
    this.requirements = requirements;
    this.subTypeClass = subTypeClass;
  }

  public static ConditionType get(String conditionType) {
    return EnumUtils.get(ConditionType.class, conditionType, NONE);
  }

  public boolean requiresName() {
    return this.requirements.requiresName();
  }

  public boolean requiresValue() {
    return this.requirements.requiresValue();
  }

  public boolean requiresOperation() {
    return this.requirements.requiresOperation();
  }

  public boolean requiresPlayer() {
    return !PLAYER_INDEPENDENT_TYPES.contains(this);
  }

  public boolean hasSubTypes() {
    return this.subTypeClass != null;
  }

  public ConditionSubTypeEntry[] getSubTypes() {
    if (this.subTypeClass == null) {
      return new ConditionSubTypeEntry[0];
    }

    return this.subTypeClass.getEnumConstants();
  }

  public ConditionSubTypeEntry getSubType(String name) {
    if (this.subTypeClass == null || name == null || name.isEmpty()) {
      return null;
    }

    for (ConditionSubTypeEntry subTypeEntry : this.subTypeClass.getEnumConstants()) {
      if (((Enum<?>) subTypeEntry).name().equals(name)) {
        return subTypeEntry;
      }
    }
    return null;
  }
}
