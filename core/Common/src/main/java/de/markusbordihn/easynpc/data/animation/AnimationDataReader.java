/*
 * Copyright 2024 Markus Bordihn
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

package de.markusbordihn.easynpc.data.animation;

import com.google.gson.Gson;
import de.markusbordihn.easynpc.Constants;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class AnimationDataReader {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String LOG_PREFIX = "[Animation Data Reader]";
  private static final String SUPPORTED_FORMAT_VERSION = "1.8.0";
  private static final Gson GSON = new Gson();

  public static AnimationData parseAnimationStream(InputStream inputStream, String sourceName)
      throws IOException {
    try (Reader reader = new InputStreamReader(inputStream, StandardCharsets.UTF_8)) {
      return parseAnimation(reader, sourceName);
    }
  }

  private static AnimationData parseAnimation(Reader reader, String sourceName) {
    AnimationData animationData = GSON.fromJson(reader, AnimationData.class);

    if (animationData == null) {
      log.error(
          "{} Failed to parse animation from {}, data is empty or invalid.",
          LOG_PREFIX,
          sourceName);
      return null;
    }

    if (!SUPPORTED_FORMAT_VERSION.equals(animationData.getFormatVersion())) {
      log.warn(
          "{} Unsupported format version {} in {}, will try to load it anyway.",
          LOG_PREFIX,
          animationData.getFormatVersion(),
          sourceName);
    }

    if (animationData.getAnimations() == null || animationData.getAnimations().isEmpty()) {
      log.error("{} No animations found in {}.", LOG_PREFIX, sourceName);
      return null;
    }

    for (Map.Entry<String, AnimationData.Animation> entry :
        animationData.getAnimations().entrySet()) {
      entry.getValue().setName(entry.getKey());
    }

    return animationData;
  }
}
