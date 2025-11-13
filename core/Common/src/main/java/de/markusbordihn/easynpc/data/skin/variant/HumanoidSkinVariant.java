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

public enum HumanoidSkinVariant implements VariantTexture {
  ALEX(Constants.MINECRAFT_PREFIX, "textures/entity/player/wide/alex.png"),
  ARI(Constants.MINECRAFT_PREFIX, "textures/entity/player/wide/ari.png"),
  EFE(Constants.MINECRAFT_PREFIX, "textures/entity/player/wide/efe.png"),
  KAI(Constants.MINECRAFT_PREFIX, "textures/entity/player/wide/kai.png"),
  MAKENA(Constants.MINECRAFT_PREFIX, "textures/entity/player/wide/makena.png"),
  NOOR(Constants.MINECRAFT_PREFIX, "textures/entity/player/wide/noor.png"),
  STEVE(Constants.MINECRAFT_PREFIX, "textures/entity/player/wide/steve.png"),
  SUNNY(Constants.MINECRAFT_PREFIX, "textures/entity/player/wide/sunny.png"),
  ZURI(Constants.MINECRAFT_PREFIX, "textures/entity/player/wide/zuri.png"),
  JAYJASONBO(Constants.MOD_ID, "textures/entity/humanoid/jayjasonbo.png"),
  PROFESSOR_01(Constants.MOD_ID, "textures/entity/humanoid/professor_01.png"),
  SECURITY_01(Constants.MOD_ID, "textures/entity/humanoid/security_01.png"),
  KNIGHT_01(Constants.MOD_ID, "textures/entity/humanoid/knight_01.png"),
  KNIGHT_02(Constants.MOD_ID, "textures/entity/humanoid/knight_02.png");

  private final ResourceLocation textureLocation;

  HumanoidSkinVariant(String namespace, String path) {
    this.textureLocation = parseTextureLocation(namespace, path);
  }

  @Override
  public ResourceLocation getTextureLocation() {
    return textureLocation;
  }
}
