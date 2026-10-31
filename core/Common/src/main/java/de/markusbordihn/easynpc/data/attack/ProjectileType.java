/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.data.attack;

import com.mojang.serialization.Codec;
import java.util.Arrays;
import java.util.Locale;
import java.util.Optional;

public enum ProjectileType {
  BULLET,
  ARROW,
  FIRE_ARROW,
  SPECTRAL_ARROW,
  TRIDENT,
  SNOWBALL,
  EGG,
  SMALL_FIREBALL,
  LARGE_FIREBALL,
  WITHER_SKULL,
  ITEM;

  public static final ProjectileType DEFAULT = BULLET;

  public static final Codec<ProjectileType> CODEC =
      Codec.STRING.xmap(
          serializedName -> parse(serializedName).orElse(DEFAULT),
          ProjectileType::getSerializedName);

  private static final ProjectileType[] VALUES = values();

  private final String serializedName = this.name().toLowerCase(Locale.ROOT);

  public static Optional<ProjectileType> parse(String serializedName) {
    if (serializedName == null) {
      return Optional.empty();
    }

    String normalizedName = serializedName.trim().toLowerCase(Locale.ROOT);
    return Arrays.stream(VALUES)
        .filter(projectileType -> projectileType.serializedName.equals(normalizedName))
        .findFirst();
  }

  public static ProjectileType byOrdinal(int ordinal) {
    return ordinal >= 0 && ordinal < VALUES.length ? VALUES[ordinal] : DEFAULT;
  }

  public String getSerializedName() {
    return this.serializedName;
  }
}
