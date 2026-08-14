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

package de.markusbordihn.easynpc.data.render;

import com.mojang.serialization.Codec;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Optional;

public enum ModelTextureBlend {
  CUTOUT,
  TRANSLUCENT;

  public static final ModelTextureBlend DEFAULT = CUTOUT;

  public static final Codec<ModelTextureBlend> CODEC =
      Codec.STRING.xmap(
          serializedName -> parse(serializedName).orElse(DEFAULT),
          ModelTextureBlend::getSerializedName);

  private static final ModelTextureBlend[] VALUES = values();

  private final String serializedName = this.name().toLowerCase(Locale.ROOT);

  public static Optional<ModelTextureBlend> parse(String serializedName) {
    if (serializedName == null) {
      return Optional.empty();
    }

    String normalizedName = serializedName.trim().toLowerCase(Locale.ROOT);
    return Arrays.stream(VALUES)
        .filter(blend -> blend.serializedName.equals(normalizedName))
        .findFirst();
  }

  public static List<String> serializedNames() {
    return Arrays.stream(VALUES).map(ModelTextureBlend::getSerializedName).toList();
  }

  public String getSerializedName() {
    return this.serializedName;
  }
}
