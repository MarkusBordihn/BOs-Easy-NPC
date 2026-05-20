/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.config;

import java.io.File;
import java.util.Properties;

@SuppressWarnings({"java:S1104", "java:S1444", "java:S3008"})
public class DialogOptionsConfig extends Config {

  public static final String CONFIG_FILE_NAME = "dialog_options.cfg";
  public static final String CONFIG_FILE_HEADER =
"""
Dialog Options Configuration

allowEscClose: Allow players to close dialogs with the ESC key (default: true)
showCloseButton: Show the close button in dialogs (default: true)
displayAvatar: Display the NPC avatar in dialogs (default: true)

""";

  public static boolean ALLOW_ESC_CLOSE = true;
  public static boolean SHOW_CLOSE_BUTTON = true;
  public static boolean DISPLAY_AVATAR = true;

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    parseConfigFile();
  }

  public static void parseConfigFile() {
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodifiedProperties = (Properties) properties.clone();

    ALLOW_ESC_CLOSE = parseConfigValue(properties, "allowEscClose", ALLOW_ESC_CLOSE);
    SHOW_CLOSE_BUTTON = parseConfigValue(properties, "showCloseButton", SHOW_CLOSE_BUTTON);
    DISPLAY_AVATAR = parseConfigValue(properties, "displayAvatar", DISPLAY_AVATAR);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodifiedProperties);
  }
}
