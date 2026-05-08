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

import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import de.markusbordihn.easynpc.security.NpcFeature;
import de.markusbordihn.easynpc.security.NpcSecurityRole;
import java.io.File;
import java.util.EnumMap;
import java.util.Map;
import java.util.Properties;

@SuppressWarnings({"java:S1104", "java:S1444", "java:S3008"})
public class SecurityConfig extends Config {

  public static final String CONFIG_FILE_NAME = "security.cfg";
  public static final String CONFIG_FILE_HEADER =
"""
Easy NPC Security Configuration

Permission values use enum names: ALL, MODERATORS, GAMEMASTERS, ADMINS, OWNERS.
Legacy numeric Minecraft command permission levels are accepted and rewritten to enum names.

normalPlayerCommandLevel: Max command level for normal player-owned imports (default: ALL)
creativePlayerCommandLevel: Max command level for creative non-admin imports (default: MODERATORS)
maxAdminImportedCommandLevel: Max command level for admin imports (default: ADMINS)
serverTrustedCommandLevel: Max command level for server-side trusted imports without player context (default: ADMINS)
blockUnsafeNpcCommands: Blocks critical server management commands from NPC command execution (default: true)
npcSpawnRateLimitCreative: Max new NPCs a creative (non-admin) player may spawn via browser per minute (default: 5)
npcSpawnRateLimitAdmin: Max new NPCs an admin player may spawn via browser per minute (default: 20)
Feature values use enum names: NORMAL_PLAYER, CREATIVE_PLAYER, ADMIN, SERVER_TRUSTED.
feature.SPAWN_NPC: Minimum role required to spawn new NPCs from the preset browser (default: CREATIVE_PLAYER)
feature.WORLD_PRESET: Minimum role required to import/export world presets (default: CREATIVE_PLAYER)
feature.CUSTOM_PRESET: Minimum role required to import/export custom server presets (default: CREATIVE_PLAYER)
feature.LOCAL_PRESET: Minimum role required to export presets locally to the client (default: NORMAL_PLAYER)
feature.DEFAULT_PRESET_IMPORT: Minimum role required to import built-in default presets (default: CREATIVE_PLAYER)
feature.URL_RESOURCE: Minimum role required to use URL-based skin loading (default: CREATIVE_PLAYER)

""";
  private static final EnumMap<NpcFeature, NpcSecurityRole> DEFAULT_FEATURE_ROLES =
      new EnumMap<>(NpcFeature.class);
  private static final EnumMap<NpcFeature, NpcSecurityRole> FEATURE_ROLES =
      new EnumMap<>(NpcFeature.class);
  public static CommandPermissionLevel NORMAL_PLAYER_COMMAND_LEVEL = CommandPermissionLevel.ALL;
  public static CommandPermissionLevel CREATIVE_PLAYER_COMMAND_LEVEL =
      CommandPermissionLevel.MODERATORS;
  public static CommandPermissionLevel MAX_ADMIN_IMPORTED_COMMAND_LEVEL =
      CommandPermissionLevel.ADMINS;
  public static CommandPermissionLevel SERVER_TRUSTED_COMMAND_LEVEL = CommandPermissionLevel.ADMINS;
  public static boolean BLOCK_UNSAFE_NPC_COMMANDS = true;
  public static int NPC_SPAWN_RATE_LIMIT_CREATIVE = 5;
  public static int NPC_SPAWN_RATE_LIMIT_ADMIN = 20;

  static {
    DEFAULT_FEATURE_ROLES.put(NpcFeature.DIALOG, NpcSecurityRole.NORMAL_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.TRADING, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.COMMAND_ACTION, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.SCOREBOARD_ACTION, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.INTERACT_BLOCK_ACTION, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.OPEN_TRADING_ACTION, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.OBJECTIVE, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.MOVEMENT, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.POSITION, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.COMBAT_ATTRIBUTE, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.BASE_ATTRIBUTE, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.SPAWN_NPC, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.WORLD_PRESET, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.CUSTOM_PRESET, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.LOCAL_PRESET, NpcSecurityRole.NORMAL_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.DEFAULT_PRESET_IMPORT, NpcSecurityRole.CREATIVE_PLAYER);
    DEFAULT_FEATURE_ROLES.put(NpcFeature.URL_RESOURCE, NpcSecurityRole.CREATIVE_PLAYER);
    FEATURE_ROLES.putAll(DEFAULT_FEATURE_ROLES);
  }

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    parseConfigFile();
  }

  public static void parseConfigFile() {
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodifiedProperties = (Properties) properties.clone();

    NORMAL_PLAYER_COMMAND_LEVEL =
        parseConfigValue(properties, "normalPlayerCommandLevel", NORMAL_PLAYER_COMMAND_LEVEL);
    CREATIVE_PLAYER_COMMAND_LEVEL =
        parseConfigValue(properties, "creativePlayerCommandLevel", CREATIVE_PLAYER_COMMAND_LEVEL);
    MAX_ADMIN_IMPORTED_COMMAND_LEVEL =
        parseConfigValue(
            properties, "maxAdminImportedCommandLevel", MAX_ADMIN_IMPORTED_COMMAND_LEVEL);
    SERVER_TRUSTED_COMMAND_LEVEL =
        parseConfigValue(properties, "serverTrustedCommandLevel", SERVER_TRUSTED_COMMAND_LEVEL);
    BLOCK_UNSAFE_NPC_COMMANDS =
        parseConfigValue(properties, "blockUnsafeNpcCommands", BLOCK_UNSAFE_NPC_COMMANDS);
    NPC_SPAWN_RATE_LIMIT_CREATIVE =
        parseConfigValue(properties, "npcSpawnRateLimitCreative", NPC_SPAWN_RATE_LIMIT_CREATIVE);
    NPC_SPAWN_RATE_LIMIT_ADMIN =
        parseConfigValue(properties, "npcSpawnRateLimitAdmin", NPC_SPAWN_RATE_LIMIT_ADMIN);
    for (Map.Entry<NpcFeature, NpcSecurityRole> entry : DEFAULT_FEATURE_ROLES.entrySet()) {
      FEATURE_ROLES.put(
          entry.getKey(),
          parseConfigValue(properties, "feature." + entry.getKey().name(), entry.getValue()));
    }

    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodifiedProperties);
  }

  public static NpcSecurityRole getRequiredRole(NpcFeature feature) {
    return FEATURE_ROLES.getOrDefault(
        feature, DEFAULT_FEATURE_ROLES.getOrDefault(feature, NpcSecurityRole.ADMIN));
  }

  private static CommandPermissionLevel parseConfigValue(
      Properties properties, String key, CommandPermissionLevel defaultValue) {
    if (properties.containsKey(key)) {
      CommandPermissionLevel parsedValue =
          CommandPermissionLevel.parse(properties.getProperty(key), defaultValue);
      properties.setProperty(key, parsedValue.name());
      return parsedValue;
    }

    properties.setProperty(key, defaultValue.name());

    return defaultValue;
  }

  private static NpcSecurityRole parseConfigValue(
      Properties properties, String key, NpcSecurityRole defaultValue) {
    if (properties.containsKey(key)) {
      NpcSecurityRole parsedValue =
          NpcSecurityRole.parse(properties.getProperty(key), defaultValue);
      properties.setProperty(key, parsedValue.name());
      return parsedValue;
    }

    properties.setProperty(key, defaultValue.name());

    return defaultValue;
  }
}
