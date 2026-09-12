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

import java.io.File;
import java.util.Properties;

@SuppressWarnings({"java:S1104", "java:S1444", "java:S3008"})
public class ClientSpeechBubbleConfig extends Config {

  public static final String CONFIG_FILE_NAME = "speech_bubble_client.cfg";
  public static final String CONFIG_FILE_HEADER =
"""
Client Speech Bubble Configuration

occlusionMode: How speech bubbles behind blocks are drawn (default: ghost)
  ghost:  hidden parts are drawn semi-transparent, visible parts fully opaque
  always: the whole bubble is always drawn on top of the world
  never:  hidden parts are cut off by the world
ghostOpacity: Opacity of the hidden parts in percent, 0 - 100 (default: 60)
minLightLevel: Lowest light level a speech bubble is drawn with, 0 - 15 (default: 7)
  0 lets a speech bubble go as dark as its surroundings, 15 keeps it always fully lit
maxRenderDistance: Maximum distance in blocks to render a speech bubble (default: 64)
overlapResolutionEnabled: Move overlapping speech bubbles apart (default: true)

""";

  public static final int MIN_RENDER_DISTANCE = 8;
  public static final int MAX_RENDER_DISTANCE = 128;
  public static final int LIGHT_LEVEL_LIMIT = 15;

  public static OcclusionMode OCCLUSION_MODE = OcclusionMode.GHOST;
  public static int GHOST_OPACITY = 60;
  public static int MIN_LIGHT_LEVEL = 7;
  public static int MAX_RENDER_DISTANCE_BLOCKS = 64;
  public static boolean OVERLAP_RESOLUTION_ENABLED = true;

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    parseConfigFile();
  }

  public static void parseConfigFile() {
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodifiedProperties = (Properties) properties.clone();

    OCCLUSION_MODE =
        OcclusionMode.fromConfigValue(
            parseConfigValue(properties, "occlusionMode", OCCLUSION_MODE.getConfigValue()),
            OCCLUSION_MODE);
    GHOST_OPACITY = parseConfigValue(properties, "ghostOpacity", GHOST_OPACITY, 0, 100);
    MIN_LIGHT_LEVEL =
        parseConfigValue(properties, "minLightLevel", MIN_LIGHT_LEVEL, 0, LIGHT_LEVEL_LIMIT);
    MAX_RENDER_DISTANCE_BLOCKS =
        parseConfigValue(
            properties,
            "maxRenderDistance",
            MAX_RENDER_DISTANCE_BLOCKS,
            MIN_RENDER_DISTANCE,
            MAX_RENDER_DISTANCE);
    OVERLAP_RESOLUTION_ENABLED =
        parseConfigValue(properties, "overlapResolutionEnabled", OVERLAP_RESOLUTION_ENABLED);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodifiedProperties);
  }
}
