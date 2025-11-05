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

package de.markusbordihn.easynpc.data.skin;

import java.util.Locale;

public enum SkinModel {
  ALLAY,
  BOGGED(true),
  CAT,
  CHICKEN,
  CREEPER,
  FAIRY,
  FOX,
  DROWNED,
  EVOKER,
  ENDER_MAN,
  HORSE,
  HUMANOID_SLIM(true),
  HUMANOID(true),
  HUSK(true),
  ILLAGER,
  ILLUSIONER,
  IRON_GOLEM,
  ORC,
  PIG,
  PIGLIN,
  PIGLIN_BRUTE,
  PILLAGER,
  PLAYER,
  SKELETON(true),
  SPIDER,
  VEX,
  VILLAGER(true),
  VINDICATOR,
  WITCH,
  WOLF,
  ZOMBIE_VILLAGER(true),
  ZOMBIE(true),
  ZOMBIFIED_PIGLIN,
  STRAY,
  WITHER_SKELETON;

  private final boolean hasArmourersWorkshopSupport;

  SkinModel() {
    this(false);
  }

  SkinModel(boolean hasArmourersWorkshopSupport) {
    this.hasArmourersWorkshopSupport = hasArmourersWorkshopSupport;
  }

  public static SkinModel get(String skinModel) {
    if (skinModel == null || skinModel.isEmpty()) {
      return SkinModel.HUMANOID;
    }
    try {
      return SkinModel.valueOf(skinModel);
    } catch (IllegalArgumentException e) {
      return SkinModel.HUMANOID;
    }
  }

  public boolean hasArmourersWorkshopSupport() {
    return this.hasArmourersWorkshopSupport;
  }

  public String getName() {
    return this.name()
        .toLowerCase(Locale.ROOT)
        .replaceAll("[^a-zA-Z0-9/._-]", "")
        .replace("..", "");
  }
}
