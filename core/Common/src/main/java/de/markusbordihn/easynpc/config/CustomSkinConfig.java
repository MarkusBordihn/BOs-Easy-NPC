/*
 * Copyright 2026 Markus Bordihn
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

import de.markusbordihn.easynpc.utils.PngCompressionUtils;
import java.io.File;
import java.util.Properties;

@SuppressWarnings({"java:S1104", "java:S1444", "java:S3008"})
public class CustomSkinConfig extends Config {

  public static final String CONFIG_FILE_NAME = "custom_skin.cfg";
  public static final String CONFIG_FILE_HEADER =
"""
Custom Skin Configuration

shareServerSkins: Send custom skins from config/easy_npc/skin/<model>/ on the server to players who do not have them (default: true)
downloadServerSkins: Load missing custom skins from the server, kept in memory only and never written to disk (default: true)
serverSkinCompressionLevel: Recompress poorly compressed server skins in memory without changing any pixel, from 1 (fastest) to 9 (smallest), 0 disables it. The files on disk stay unchanged. (default: 9)

""";

  public static boolean SHARE_SERVER_SKINS = true;
  public static boolean DOWNLOAD_SERVER_SKINS = true;
  public static int SERVER_SKIN_COMPRESSION_LEVEL = PngCompressionUtils.MAX_COMPRESSION_LEVEL;

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    parseConfigFile();
  }

  public static void parseConfigFile() {
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodifiedProperties = (Properties) properties.clone();

    SHARE_SERVER_SKINS = parseConfigValue(properties, "shareServerSkins", SHARE_SERVER_SKINS);
    DOWNLOAD_SERVER_SKINS =
        parseConfigValue(properties, "downloadServerSkins", DOWNLOAD_SERVER_SKINS);
    SERVER_SKIN_COMPRESSION_LEVEL =
        parseConfigValue(
            properties,
            "serverSkinCompressionLevel",
            SERVER_SKIN_COMPRESSION_LEVEL,
            0,
            PngCompressionUtils.MAX_COMPRESSION_LEVEL);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodifiedProperties);
  }
}
