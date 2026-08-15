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

import com.mojang.datafixers.util.Either;
import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import java.util.Optional;
import java.util.function.Function;
import net.minecraft.resources.Identifier;

public record ModelTextureSlot(Optional<Identifier> texture, ModelTextureBlend blend) {

  public static final ModelTextureSlot EMPTY =
      new ModelTextureSlot(Optional.empty(), ModelTextureBlend.DEFAULT);

  private static final Codec<ModelTextureSlot> OBJECT_CODEC =
      RecordCodecBuilder.create(
          instance ->
              instance
                  .group(
                      Identifier.CODEC
                          .optionalFieldOf("texture")
                          .forGetter(ModelTextureSlot::texture),
                      ModelTextureBlend.CODEC
                          .optionalFieldOf("blend", ModelTextureBlend.DEFAULT)
                          .forGetter(ModelTextureSlot::blend))
                  .apply(instance, ModelTextureSlot::new));

  public static final Codec<ModelTextureSlot> CODEC =
      Codec.either(Identifier.CODEC, OBJECT_CODEC)
          .xmap(
              either -> either.map(ModelTextureSlot::new, Function.identity()),
              slot ->
                  slot.isSimple() ? Either.left(slot.texture().orElseThrow()) : Either.right(slot));

  public ModelTextureSlot {
    texture = texture == null ? Optional.empty() : texture;
    blend = blend == null ? ModelTextureBlend.DEFAULT : blend;
  }

  public ModelTextureSlot(Identifier texture) {
    this(Optional.ofNullable(texture), ModelTextureBlend.DEFAULT);
  }

  public static ModelTextureSlot of(Identifier texture, ModelTextureBlend blend) {
    return new ModelTextureSlot(Optional.ofNullable(texture), blend);
  }

  public static ModelTextureSlot of(ModelTextureBlend blend) {
    return new ModelTextureSlot(Optional.empty(), blend);
  }

  public boolean isSimple() {
    return this.texture.isPresent() && this.blend == ModelTextureBlend.DEFAULT;
  }

  public boolean isEmpty() {
    return this.texture.isEmpty() && this.blend == ModelTextureBlend.DEFAULT;
  }

  public ModelTextureSlot withTexture(Identifier texture) {
    return new ModelTextureSlot(Optional.ofNullable(texture), this.blend);
  }

  public ModelTextureSlot withBlend(ModelTextureBlend blend) {
    return new ModelTextureSlot(this.texture, blend);
  }
}
