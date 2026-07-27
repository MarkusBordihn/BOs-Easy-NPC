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

package de.markusbordihn.easynpc.data.objective;

import java.util.Locale;
import java.util.Optional;

public enum ObjectiveType {
  ATTACK_ANIMAL("animal", 2),
  ATTACK_ENTITY_BY_TAG("entity_by_tag", 2),
  ATTACK_ENTITY_BY_TEAM("entity_by_team", 2),
  ATTACK_ENTITY_BY_UUID("entity_by_uuid", 2),
  ATTACK_HOSTILE_FACTIONS("hostile_factions", 2),
  ATTACK_MOB_WITHOUT_CREEPER("mob_without_creeper", 2),
  ATTACK_MOB("mob", 2),
  ATTACK_MONSTER("monster", 2),
  ATTACK_PLAYER_BY_NAME("player_by_name", 2),
  ATTACK_PLAYER_WITHOUT_OWNER("player_without_owner", 2),
  ATTACK_PLAYER("player", 2),
  ATTACK_VILLAGER("villager", 2),
  AVOID_SUN("avoid_sun", 2),
  BOW_ATTACK("bow", 4),
  CLOSE_DOOR("close_door", 8),
  CUSTOM("custom", 5, true),
  CROSSBOW_ATTACK("crossbow", 4),
  FACTION_HURT_BY_TARGET("faction_hurt_by_target", 2),
  FLEE_CREEPER("creeper", 3),
  FLEE_MOB("mob", 3),
  FLEE_MONSTER("monster", 3),
  FLEE_PLAYER("player", 3),
  FLEE_SUN("flee_sun", 3),
  FLEE_VILLAGER("villager", 3),
  FLOAT("float", 0),
  FOLLOW_ENTITY_BY_UUID("entity", 7),
  FOLLOW_ITEM("item", 7),
  FOLLOW_OWNER("owner", 6),
  FOLLOW_PLAYER("player", 7),
  GUN_ATTACK("gun", 4),
  HURT_BY_TARGET("hurt_by_target", 2),
  LOOK_AT_ANIMAL("animal", 10, false),
  LOOK_AT_ENTITY_BY_UUID("entity", 9, false),
  LOOK_AT_ITEM("item", 9, false),
  LOOK_AT_MOB("mob", 10, false),
  LOOK_AT_OWNER("owner", 9, false),
  LOOK_AT_PLAYER("player", 9, false),
  LOOK_AT_RESET("reset", 9, false),
  LOOK_RANDOM_AROUND("random", 10, false),
  MELEE_ATTACK("melee", 2),
  MOVE_BACK_TO_HOME("back_home", 8),
  MOVE_BACK_TO_VILLAGE("back_village", 8),
  MOVE_THROUGH_VILLAGE("through_village", 9),
  NONE("none", false),
  OPEN_DOOR("open_door", 8),
  OWNER_HURT_BY_TARGET("owner_hurt_by_target", 2),
  PANIC("panic", 1),
  RANDOM_STROLL_AROUND_HOME("around_home", 10),
  RANDOM_STROLL_IN_VILLAGE("in_village", 10),
  RANDOM_STROLL("stroll", 11),
  RANDOM_SWIMMING("swimming", 4),
  WATER_AVOIDING_RANDOM_STROLL("avoid_water", 11),
  ZOMBIE_ATTACK("zombie", 2);

  private final String friendlyName;
  private final boolean hasTravelObjective;
  private final int defaultPriority;
  private final String objectiveName = this.name().toLowerCase(Locale.ROOT);

  ObjectiveType(String friendlyName, boolean hasTravelObjective) {
    this(friendlyName, 5, hasTravelObjective);
  }

  ObjectiveType(String friendlyName, int defaultPriority) {
    this(friendlyName, defaultPriority, true);
  }

  ObjectiveType(String friendlyName, int defaultPriority, boolean hasTravelObjective) {
    this.friendlyName = friendlyName;
    this.defaultPriority = defaultPriority;
    this.hasTravelObjective = hasTravelObjective;
  }

  public static ObjectiveType get(String objectiveType) {
    if (objectiveType == null || objectiveType.isEmpty()) {
      return ObjectiveType.NONE;
    }
    try {
      return ObjectiveType.valueOf(objectiveType.toUpperCase(Locale.ROOT));
    } catch (IllegalArgumentException e) {
      return ObjectiveType.NONE;
    }
  }

  public static Optional<ObjectiveType> byString(String objectiveType) {
    try {
      return Optional.of(ObjectiveType.valueOf(objectiveType));
    } catch (IllegalArgumentException e) {
      return Optional.empty();
    }
  }

  public boolean hasTravelObjective() {
    return this.hasTravelObjective;
  }

  public int getDefaultPriority() {
    return this.defaultPriority;
  }

  public String getObjectiveName() {
    return this.objectiveName;
  }

  public String getFriendlyName() {
    return this.friendlyName;
  }
}
