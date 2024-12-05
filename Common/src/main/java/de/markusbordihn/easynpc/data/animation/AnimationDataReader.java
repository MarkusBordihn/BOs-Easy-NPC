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
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class AnimationDataReader {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String LOG_PREFIX = "[Animation Data Reader]";
  private static final String SUPPORTED_FORMAT_VERSION = "1.8.0";

  public static AnimationData parseAnimationFile(String filePath) throws IOException {
    return parseAnimationFile(Paths.get(filePath));
  }

  public static AnimationData parseAnimationFile(Path filePath) throws IOException {
    Gson gson = new Gson();
    Reader reader = Files.newBufferedReader(filePath);
    AnimationData animationData = gson.fromJson(reader, AnimationData.class);

    // Warn if format version is not supported
    if (!SUPPORTED_FORMAT_VERSION.equals(animationData.getFormatVersion())) {
      log.warn(
          "{} Unsupported format version {} in file {}, will try to load it anyway.",
          LOG_PREFIX,
          animationData.getFormatVersion(),
          filePath);
    }

    // Adding meta data to the animation data
    for (Map.Entry<String, AnimationData.Animation> entry :
        animationData.getAnimations().entrySet()) {
      entry.getValue().setName(entry.getKey());
    }

    return animationData;
  }
}
