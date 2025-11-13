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

package de.markusbordihn.easynpc.data.skin.variant;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.skin.VariantTexture;
import net.minecraft.resources.ResourceLocation;

public enum CatSkinVariant implements VariantTexture {
  ALL_BLACK(Constants.MINECRAFT_PREFIX, "textures/entity/cat/all_black.png"),
  BLACK(Constants.MINECRAFT_PREFIX, "textures/entity/cat/black.png"),
  BRITISH_SHORTHAIR(Constants.MINECRAFT_PREFIX, "textures/entity/cat/british_shorthair.png"),
  CALICO(Constants.MINECRAFT_PREFIX, "textures/entity/cat/calico.png"),
  JELLIE(Constants.MINECRAFT_PREFIX, "textures/entity/cat/jellie.png"),
  OCELOT(Constants.MINECRAFT_PREFIX, "textures/entity/cat/ocelot.png"),
  PERSIAN(Constants.MINECRAFT_PREFIX, "textures/entity/cat/persian.png"),
  RAGDOLL(Constants.MINECRAFT_PREFIX, "textures/entity/cat/ragdoll.png"),
  RED(Constants.MINECRAFT_PREFIX, "textures/entity/cat/red.png"),
  SIAMESE(Constants.MINECRAFT_PREFIX, "textures/entity/cat/siamese.png"),
  TABBY(Constants.MINECRAFT_PREFIX, "textures/entity/cat/tabby.png"),
  WHITE(Constants.MINECRAFT_PREFIX, "textures/entity/cat/white.png");

  private final ResourceLocation textureLocation;

  CatSkinVariant(String namespace, String path) {
    this.textureLocation = parseTextureLocation(namespace, path);
  }

  @Override
  public ResourceLocation getTextureLocation() {
    return textureLocation;
  }
}
