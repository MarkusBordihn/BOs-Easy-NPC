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
import java.util.HashMap;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TextureErrorHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Texture Error Handler]";
  private static final Map<TextureModelKey, String> errorMessageMap = new HashMap<>();
  private static String lastErrorMessage;

  private TextureErrorHandler() {}

  public static void processingErrorMessage(
      TextureModelKey textureModelKey, String remoteUrl, String reason) {
    String errorMessage = String.format("Unable to process texture from %s: %s", remoteUrl, reason);
    log.error("{} {}", LOG_PREFIX, errorMessage);
    addErrorMessage(textureModelKey, errorMessage);
  }

  public static void urlLoadErrorMessage(
      TextureModelKey textureModelKey, String remoteUrl, String reason) {
    String errorMessage = String.format("Unable to load texture from %s: %s", remoteUrl, reason);
    log.error("{} {}", LOG_PREFIX, errorMessage);
    addErrorMessage(textureModelKey, errorMessage);
  }

  private static void addErrorMessage(TextureModelKey textureModelKey, String errorMessage) {
    errorMessageMap.put(textureModelKey, errorMessage);
    lastErrorMessage = errorMessage;
  }

  public static boolean hasLastErrorMessage() {
    return lastErrorMessage != null && !lastErrorMessage.isEmpty();
  }

  public static String getLastErrorMessage() {
    return lastErrorMessage;
  }

  public static void clearLastErrorMessage() {
    lastErrorMessage = null;
  }
}
