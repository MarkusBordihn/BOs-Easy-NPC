/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.easynpc.client.texture;

import de.markusbordihn.easynpc.Constants;
import java.util.Locale;
import java.util.UUID;

public class TextureNameHelper {

  private static final String TEXTURE_PREFIX = Constants.MOD_ID + "_client_texture_";

  private TextureNameHelper() {}

  public static String getResourceName(TextureModelKey textureModelKey) {
    return getResourceName(textureModelKey.getUUID().toString(), textureModelKey.getSubType());
  }

  public static String getResourceName(String name, String type) {
    return (TEXTURE_PREFIX + type + "_" + name.replaceAll("[^a-zA-Z0-9_.-]", ""))
        .toLowerCase(Locale.ROOT);
  }

  public static String getFileName(UUID uuid) {
    return getFileName(uuid.toString());
  }

  public static String getFileName(String name) {
    return name.replaceAll("[^a-zA-Z0-9_.-]", "") + ".png";
  }
}
