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

import com.mojang.blaze3d.platform.NativeImage;
import de.markusbordihn.easynpc.Constants;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.texture.DynamicTexture;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TextureRegistrationHelper {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Texture Registration Helper]";

  private TextureRegistrationHelper() {}

  public static ResourceLocation registerTexture(
      TextureModelKey textureModelKey, NativeImage nativeImage) {
    // Using client Texture Manager
    Minecraft client = Minecraft.getInstance();
    net.minecraft.client.renderer.texture.TextureManager textureManager =
        client.getTextureManager();

    // Create dynamic texture from native image.
    DynamicTexture dynamicTexture;
    try {
      dynamicTexture = new DynamicTexture(nativeImage);
    } catch (Exception exception) {
      log.error(
          "{} Unable to create dynamic texture for {}:", LOG_PREFIX, textureModelKey, exception);
      nativeImage.close();
      return null;
    }

    // Register dynamic texture under resource location.
    String resourceName = TextureNameHelper.getResourceName(textureModelKey);
    ResourceLocation resourceLocation = textureManager.register(resourceName, dynamicTexture);
    log.info(
        "{} Registered texture {} with image {} with {}.",
        LOG_PREFIX,
        textureModelKey,
        nativeImage,
        resourceLocation);

    return resourceLocation;
  }
}
