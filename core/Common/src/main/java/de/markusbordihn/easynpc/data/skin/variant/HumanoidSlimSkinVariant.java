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

public enum HumanoidSlimSkinVariant implements VariantTexture {
  ALEX(Constants.MINECRAFT_PREFIX, "textures/entity/player/slim/alex.png"),
  ARI(Constants.MINECRAFT_PREFIX, "textures/entity/player/slim/ari.png"),
  EFE(Constants.MINECRAFT_PREFIX, "textures/entity/player/slim/efe.png"),
  KAI(Constants.MINECRAFT_PREFIX, "textures/entity/player/slim/kai.png"),
  MAKENA(Constants.MINECRAFT_PREFIX, "textures/entity/player/slim/makena.png"),
  NOOR(Constants.MINECRAFT_PREFIX, "textures/entity/player/slim/noor.png"),
  STEVE(Constants.MINECRAFT_PREFIX, "textures/entity/player/slim/steve.png"),
  SUNNY(Constants.MINECRAFT_PREFIX, "textures/entity/player/slim/sunny.png"),
  ZURI(Constants.MINECRAFT_PREFIX, "textures/entity/player/slim/zuri.png"),
  KAWORRU(Constants.MOD_ID, "textures/entity/humanoid_slim/kaworru.png");

  private final ResourceLocation textureLocation;

  HumanoidSlimSkinVariant(String namespace, String path) {
    this.textureLocation = parseTextureLocation(namespace, path);
  }

  @Override
  public ResourceLocation getTextureLocation() {
    return textureLocation;
  }
}
