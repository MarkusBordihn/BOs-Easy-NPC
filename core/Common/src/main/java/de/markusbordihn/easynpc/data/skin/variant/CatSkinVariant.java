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
import de.markusbordihn.easynpc.api.skin.VariantTexture;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.animal.CatVariant;

public enum CatSkinVariant implements VariantTexture {
  ALL_BLACK(Constants.MINECRAFT_PREFIX, "textures/entity/cat/all_black.png", CatVariant.ALL_BLACK),
  BLACK(Constants.MINECRAFT_PREFIX, "textures/entity/cat/black.png", CatVariant.BLACK),
  BRITISH_SHORTHAIR(
      Constants.MINECRAFT_PREFIX,
      "textures/entity/cat/british_shorthair.png",
      CatVariant.BRITISH_SHORTHAIR),
  CALICO(Constants.MINECRAFT_PREFIX, "textures/entity/cat/calico.png", CatVariant.CALICO),
  JELLIE(Constants.MINECRAFT_PREFIX, "textures/entity/cat/jellie.png", CatVariant.JELLIE),
  OCELOT(Constants.MINECRAFT_PREFIX, "textures/entity/cat/ocelot.png", CatVariant.TABBY),
  PERSIAN(Constants.MINECRAFT_PREFIX, "textures/entity/cat/persian.png", CatVariant.PERSIAN),
  RAGDOLL(Constants.MINECRAFT_PREFIX, "textures/entity/cat/ragdoll.png", CatVariant.RAGDOLL),
  RED(Constants.MINECRAFT_PREFIX, "textures/entity/cat/red.png", CatVariant.RED),
  SIAMESE(Constants.MINECRAFT_PREFIX, "textures/entity/cat/siamese.png", CatVariant.SIAMESE),
  TABBY(Constants.MINECRAFT_PREFIX, "textures/entity/cat/tabby.png", CatVariant.TABBY),
  WHITE(Constants.MINECRAFT_PREFIX, "textures/entity/cat/white.png", CatVariant.WHITE);

  private final ResourceLocation textureLocation;
  private final ResourceKey<CatVariant> vanillaVariant;

  CatSkinVariant(String namespace, String path, ResourceKey<CatVariant> vanillaVariant) {
    this.textureLocation = parseTextureLocation(namespace, path);
    this.vanillaVariant = vanillaVariant;
  }

  public ResourceKey<CatVariant> getVanillaVariant() {
    return this.vanillaVariant;
  }

  @Override
  public ResourceLocation getTextureLocation() {
    return this.textureLocation;
  }
}
