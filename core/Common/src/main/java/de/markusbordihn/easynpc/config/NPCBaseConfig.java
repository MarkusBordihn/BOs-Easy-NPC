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

import de.markusbordihn.easynpc.data.attack.ProjectileData;
import java.io.File;
import java.util.Properties;

@SuppressWarnings({"java:S1104", "java:S1444", "java:S3008"})
public class NPCBaseConfig extends Config {

  public static final String CONFIG_FILE_NAME = "npc_base.cfg";
  public static final String CONFIG_FILE_HEADER =
"""
NPC Base Configuration

allowBypassInvulnerability: Allows damage types like /kill command or void to bypass NPC invulnerability (default: true)
projectileMaximumExplosionPower: Maximum explosion power of custom NPC projectiles, 0 disables explosions (default: 2, range: 0-6)
projectileExplosionBlockDamage: Allows custom NPC projectile explosions to destroy blocks, also requires the mobGriefing game rule (default: false)

""";

  public static boolean ALLOW_BYPASS_INVULNERABILITY = true;
  public static int PROJECTILE_MAXIMUM_EXPLOSION_POWER = 2;
  public static boolean PROJECTILE_EXPLOSION_BLOCK_DAMAGE = false;

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    parseConfigFile();
  }

  public static void parseConfigFile() {
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodifiedProperties = (Properties) properties.clone();

    ALLOW_BYPASS_INVULNERABILITY =
        parseConfigValue(properties, "allowBypassInvulnerability", ALLOW_BYPASS_INVULNERABILITY);
    PROJECTILE_MAXIMUM_EXPLOSION_POWER =
        parseConfigValue(
            properties,
            "projectileMaximumExplosionPower",
            PROJECTILE_MAXIMUM_EXPLOSION_POWER,
            0,
            (int) ProjectileData.MAXIMUM_EXPLOSION_POWER);
    PROJECTILE_EXPLOSION_BLOCK_DAMAGE =
        parseConfigValue(
            properties, "projectileExplosionBlockDamage", PROJECTILE_EXPLOSION_BLOCK_DAMAGE);

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodifiedProperties);
  }
}
