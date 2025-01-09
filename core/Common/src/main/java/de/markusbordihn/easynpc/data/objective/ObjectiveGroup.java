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

import java.util.EnumSet;
import java.util.Set;

public class ObjectiveGroup {

  public static final Set<ObjectiveType> ATTACK_TYPE =
      EnumSet.of(
          ObjectiveType.MELEE_ATTACK,
          ObjectiveType.ZOMBIE_ATTACK,
          ObjectiveType.CROSSBOW_ATTACK,
          ObjectiveType.BOW_ATTACK,
          ObjectiveType.GUN_ATTACK);

  public static final Set<ObjectiveType> FOLLOW =
      EnumSet.of(
          ObjectiveType.FOLLOW_ENTITY_BY_UUID,
          ObjectiveType.FOLLOW_OWNER,
          ObjectiveType.FOLLOW_PLAYER);

  public static final Set<ObjectiveType> ATTACK_TARGET =
      EnumSet.of(
          ObjectiveType.ATTACK_PLAYER,
          ObjectiveType.ATTACK_PLAYER_WITHOUT_OWNER,
          ObjectiveType.ATTACK_VILLAGER,
          ObjectiveType.ATTACK_ANIMAL,
          ObjectiveType.ATTACK_MONSTER,
          ObjectiveType.ATTACK_MOB,
          ObjectiveType.ATTACK_MOB_WITHOUT_CREEPER);

  private ObjectiveGroup() {}
}
