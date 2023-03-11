/**
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

package de.markusbordihn.easynpc.utils;

import java.net.URISyntaxException;
import java.util.Optional;
import java.util.regex.Pattern;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import cpw.mods.modlauncher.Launcher;
import cpw.mods.modlauncher.api.IEnvironment;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.EasyNPC;

public class StopModReposts {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String STOP_MOD_REPOSTS_URL = "https://stopmodreposts.org/";

  private static Optional<String> version =
      Launcher.INSTANCE.environment().getProperty(IEnvironment.Keys.VERSION.get());

  private static boolean isDevEnvironment =
      version.isPresent() && version.get() != null && "MOD_DEV".equals(version.get());

  private static String modFileFormatRegEx = Constants.MOD_ID + "_1.19-\\d+.\\d+.\\d+.jar";

  private static Pattern expectedFilePattern = Pattern.compile(modFileFormatRegEx);

  protected StopModReposts() {}

  public static void checkStopModReposts() {
    if (isDevEnvironment) {
      log.debug("Detected MDK environment, will skip Stop Mod Reposts checks.");
      return;
    }
    String jarFilePath = null;
    try {
      jarFilePath = EasyNPC.class.getProtectionDomain().getCodeSource().getLocation()
          .toURI().getPath();
    } catch (SecurityException | URISyntaxException | NullPointerException exception) {
      log.error("Unable to get jar file path: {}", exception);
    }
    if (jarFilePath == null || jarFilePath.isEmpty()) {
      log.debug("Received empty jar file path!");
      return;
    }

    if (expectedFilePattern.matcher(jarFilePath).find()) {
      log.info("Thanks for using {} ({}). I hope you enjoy the mod. :)", Constants.MOD_NAME,
          Constants.MOD_URL);
    } else {
      log.error("");
      log.error("===============================================");
      log.error("=                                             =");
      log.error("=   WARNING: File modification detected !!!   =");
      log.error("=                                             =");
      log.error("===============================================");
      log.error("");
      log.error("It's seems that the mod file {} you are using was modified!", jarFilePath);
      log.error(
          "Please make sure to download the latest {} mod only from the original source at {}",
          Constants.MOD_NAME, Constants.MOD_URL);
      log.error(
          "If you downloaded this mod from other sources we could not make sure that it works as expected or does not includes any unwanted modification (e.g. adware, malware, ...).");
      log.error("");
      log.error("See the following page for more details: {}", STOP_MOD_REPOSTS_URL);
      log.error("");
    }

  }
}
