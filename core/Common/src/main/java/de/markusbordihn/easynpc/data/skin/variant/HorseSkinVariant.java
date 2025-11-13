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

public enum HorseSkinVariant implements VariantTexture {
  WHITE(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_white.png"),
  WHITE_SADDLED(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_white.png"),
  CREAMY(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_creamy.png"),
  CREAMY_SADDLED(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_creamy.png"),
  CHESTNUT(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_chestnut.png"),
  CHESTNUT_SADDLED(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_chestnut.png"),
  BROWN(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_brown.png"),
  BROWN_SADDLED(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_brown.png"),
  BLACK(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_black.png"),
  BLACK_SADDLED(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_black.png"),
  GRAY(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_gray.png"),
  GRAY_SADDLED(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_gray.png"),
  DARKBROWN(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_darkbrown.png"),
  DARKBROWN_SADDLED(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_darkbrown.png"),
  SKELETON(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_skeleton.png"),
  SKELETON_SADDLED(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_skeleton.png"),
  ZOMBIE(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_zombie.png"),
  ZOMBIE_SADDLED(Constants.MINECRAFT_PREFIX, "textures/entity/horse/horse_zombie.png"),
  DONKEY(Constants.MINECRAFT_PREFIX, "textures/entity/horse/donkey.png"),
  DONKEY_SADDLED(Constants.MINECRAFT_PREFIX, "textures/entity/horse/donkey.png"),
  MULE(Constants.MINECRAFT_PREFIX, "textures/entity/horse/mule.png"),
  MULE_SADDLED(Constants.MINECRAFT_PREFIX, "textures/entity/horse/mule.png");

  private final ResourceLocation textureLocation;

  HorseSkinVariant(String namespace, String path) {
    this.textureLocation = parseTextureLocation(namespace, path);
  }

  @Override
  public ResourceLocation getTextureLocation() {
    return textureLocation;
  }
}
