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

package de.markusbordihn.easynpc.data.attribute;

import net.minecraft.nbt.CompoundTag;

public record CombatAttributes(
    boolean isAttackableByPlayers,
    boolean isAttackableByMonsters,
    boolean isInvulnerable,
    double healthRegeneration)
    implements EntityAttributesInterface {

  public static final String IS_ATTACKABLE_BY_PLAYERS_TAG =
      CombatAttributeType.IS_ATTACKABLE_BY_PLAYERS.getTagName();
  public static final String IS_ATTACKABLE_BY_MONSTERS_TAG =
      CombatAttributeType.IS_ATTACKABLE_BY_MONSTERS.getTagName();
  public static final String IS_INVULNERABLE_TAG = CombatAttributeType.IS_INVULNERABLE.getTagName();
  public static final String HEALTH_REGENERATION_TAG =
      CombatAttributeType.HEALTH_REGENERATION.getTagName();

  public CombatAttributes() {
    this(false, false, true, 0.0);
  }

  public static CombatAttributes decode(CompoundTag compoundTag) {
    return new CombatAttributes(
        compoundTag.getBoolean(IS_ATTACKABLE_BY_PLAYERS_TAG),
        compoundTag.getBoolean(IS_ATTACKABLE_BY_MONSTERS_TAG),
        compoundTag.getBoolean(IS_INVULNERABLE_TAG),
        compoundTag.getDouble(HEALTH_REGENERATION_TAG));
  }

  public CombatAttributes withHealthRegeneration(double healthRegeneration) {
    return new CombatAttributes(
        isAttackableByPlayers, isAttackableByMonsters, isInvulnerable, healthRegeneration);
  }

  public CombatAttributes withIsAttackableByPlayers(boolean isAttackableByPlayers) {
    return new CombatAttributes(
        isAttackableByPlayers, isAttackableByMonsters, isInvulnerable, healthRegeneration);
  }

  public CombatAttributes withIsAttackableByMonsters(boolean isAttackableByMonsters) {
    return new CombatAttributes(
        isAttackableByPlayers, isAttackableByMonsters, isInvulnerable, healthRegeneration);
  }

  public CombatAttributes withIsInvulnerable(boolean isInvulnerable) {
    return new CombatAttributes(
        isAttackableByPlayers, isAttackableByMonsters, isInvulnerable, healthRegeneration);
  }

  public CompoundTag encode(CompoundTag compoundTag) {
    compoundTag.putBoolean(IS_ATTACKABLE_BY_PLAYERS_TAG, isAttackableByPlayers());
    compoundTag.putBoolean(IS_ATTACKABLE_BY_MONSTERS_TAG, isAttackableByMonsters());
    compoundTag.putBoolean(IS_INVULNERABLE_TAG, isInvulnerable());
    compoundTag.putDouble(HEALTH_REGENERATION_TAG, healthRegeneration());
    return compoundTag;
  }
}
