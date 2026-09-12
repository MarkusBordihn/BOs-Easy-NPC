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
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.animal.feline.CatVariant;
import net.minecraft.world.entity.animal.feline.CatVariants;

public enum CatSkinVariant implements VariantTexture {
  ALL_BLACK(
      Constants.MINECRAFT_PREFIX, "textures/entity/cat/cat_all_black.png", CatVariants.ALL_BLACK),
  BLACK(Constants.MINECRAFT_PREFIX, "textures/entity/cat/cat_black.png", CatVariants.BLACK),
  BRITISH_SHORTHAIR(
      Constants.MINECRAFT_PREFIX,
      "textures/entity/cat/cat_british_shorthair.png",
      CatVariants.BRITISH_SHORTHAIR),
  CALICO(Constants.MINECRAFT_PREFIX, "textures/entity/cat/cat_calico.png", CatVariants.CALICO),
  JELLIE(Constants.MINECRAFT_PREFIX, "textures/entity/cat/cat_jellie.png", CatVariants.JELLIE),
  OCELOT(Constants.MINECRAFT_PREFIX, "textures/entity/cat/ocelot.png", CatVariants.TABBY),
  PERSIAN(Constants.MINECRAFT_PREFIX, "textures/entity/cat/cat_persian.png", CatVariants.PERSIAN),
  RAGDOLL(Constants.MINECRAFT_PREFIX, "textures/entity/cat/cat_ragdoll.png", CatVariants.RAGDOLL),
  RED(Constants.MINECRAFT_PREFIX, "textures/entity/cat/cat_red.png", CatVariants.RED),
  SIAMESE(Constants.MINECRAFT_PREFIX, "textures/entity/cat/cat_siamese.png", CatVariants.SIAMESE),
  TABBY(Constants.MINECRAFT_PREFIX, "textures/entity/cat/cat_tabby.png", CatVariants.TABBY),
  WHITE(Constants.MINECRAFT_PREFIX, "textures/entity/cat/cat_white.png", CatVariants.WHITE),
  BENGAL(Constants.MOD_ID, "textures/entity/cat/cat_bengal.png", CatVariants.TABBY);

  private final Identifier textureLocation;
  private final ResourceKey<CatVariant> vanillaVariant;

  CatSkinVariant(String namespace, String path, ResourceKey<CatVariant> vanillaVariant) {
    this.textureLocation = parseTextureLocation(namespace, path);
    this.vanillaVariant = vanillaVariant;
  }

  public ResourceKey<CatVariant> getVanillaVariant() {
    return this.vanillaVariant;
  }

  @Override
  public Identifier getTextureLocation() {
    return this.textureLocation;
  }
}
