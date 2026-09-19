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

public enum WolfSkinVariant implements VariantTexture {
  WOLF(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf.png"),
  TAMED(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_tame.png"),
  ANGRY(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_angry.png"),
  ASHEN(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_ashen.png"),
  ASHEN_TAMED(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_ashen_tame.png"),
  ASHEN_ANGRY(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_ashen_angry.png"),
  BLACK(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_black.png"),
  BLACK_TAMED(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_black_tame.png"),
  BLACK_ANGRY(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_black_angry.png"),
  CHESTNUT(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_chestnut.png"),
  CHESTNUT_TAMED(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_chestnut_tame.png"),
  CHESTNUT_ANGRY(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_chestnut_angry.png"),
  SNOWY(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_snowy.png"),
  SNOWY_TAMED(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_snowy_tame.png"),
  SNOWY_ANGRY(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_snowy_angry.png"),
  SPOTTED(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_spotted.png"),
  SPOTTED_TAMED(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_spotted_tame.png"),
  SPOTTED_ANGRY(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_spotted_angry.png"),
  STRIPED(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_striped.png"),
  STRIPED_TAMED(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_striped_tame.png"),
  STRIPED_ANGRY(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_striped_angry.png"),
  RUSTY(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_rusty.png"),
  RUSTY_TAMED(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_rusty_tame.png"),
  RUSTY_ANGRY(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_rusty_angry.png"),
  WOODS(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_woods.png"),
  WOODS_TAMED(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_woods_tame.png"),
  WOODS_ANGRY(Constants.MINECRAFT_PREFIX, "textures/entity/wolf/wolf_woods_angry.png"),
  BRINDLE(Constants.MOD_ID, "textures/entity/wolf/wolf_brindle.png");

  private final Identifier textureLocation;

  WolfSkinVariant(String namespace, String path) {
    this.textureLocation = parseTextureLocation(namespace, path);
  }

  @Override
  public Identifier getTextureLocation() {
    return this.textureLocation;
  }
}
