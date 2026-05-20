/*
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

package de.markusbordihn.easynpc.config;

import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;

public class RenderEntityTypeSupportConfig extends Config {

  public static final String CONFIG_FILE_NAME = "render_entity_type_support.cfg";
  public static final String CONFIG_FILE_HEADER =
"""
Render Entity Type Support Configuration

Please note that this configuration file only includes confirmed entity types.
If an entity type is not listed here, it doesn't mean it's automatically supported or unsupported!
""";
  private static final Set<String> supportedEntityTypes = new HashSet<>();
  private static final Set<String> unsupportedEntityTypes = new HashSet<>();

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    initializeDefaultEntityTypes();
    log.info(
        "Initialized {} supported and {} unsupported entity types from defaults.",
        supportedEntityTypes.size(),
        unsupportedEntityTypes.size());
    parseConfigFile();
    log.info(
        "After config parsing: {} supported and {} unsupported entity types.",
        supportedEntityTypes.size(),
        unsupportedEntityTypes.size());
  }

  private static void initializeDefaultEntityTypes() {
    supportedEntityTypes.addAll(RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES);
    supportedEntityTypes.addAll(
        RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_THIRD_PARTY_ENTITY_TYPES);

    unsupportedEntityTypes.addAll(RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_ENTITY_TYPES);
    unsupportedEntityTypes.addAll(
        RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_THIRD_PARTY_ENTITY_TYPES);
  }

  public static void parseConfigFile() {
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodifiedProperties = (Properties) properties.clone();

    // Parse known entity types and update them based on configuration overrides.
    for (Set<String> entityTypes :
        List.of(
            RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_ENTITY_TYPES,
            RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES,
            RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_THIRD_PARTY_ENTITY_TYPES,
            RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_THIRD_PARTY_ENTITY_TYPES)) {
      boolean defaultValue =
          entityTypes == RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES
              || entityTypes
                  == RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_THIRD_PARTY_ENTITY_TYPES;
      parseKnownEntityTypes(properties, entityTypes, defaultValue);
    }

    // Parse the rest of the configuration file.
    for (String entityType : properties.stringPropertyNames()) {
      if (isKnownEntityType(entityType)) {
        continue;
      }
      if (isInvalidEntityType(entityType)) {
        log.error("Remove invalid entity type {} from {}.", entityType, CONFIG_FILE_NAME);
        properties.remove(entityType);
        continue;
      }
      if (parseConfigValue(properties, entityType, false)) {
        addSupportedEntityType(entityType);
      } else {
        addUnsupportedEntityType(entityType);
      }
    }

    // Update config file if needed
    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodifiedProperties);
  }

  private static void parseKnownEntityTypes(
      Properties properties, Set<String> entityTypes, boolean defaultValue) {
    for (String entityType : entityTypes) {
      boolean configuredValue = parseConfigValue(properties, entityType, defaultValue);
      if (configuredValue == defaultValue) {
        continue;
      }

      if (configuredValue) {
        addSupportedEntityType(entityType);
      } else {
        addUnsupportedEntityType(entityType);
      }
    }
  }

  public static void addUnsupportedEntityType(String entityType) {
    unsupportedEntityTypes.add(entityType);
    supportedEntityTypes.remove(entityType);
  }

  public static void addSupportedEntityType(String entityType) {
    supportedEntityTypes.add(entityType);
    unsupportedEntityTypes.remove(entityType);
  }

  public static boolean isSupportedEntityType(String entityType) {
    return supportedEntityTypes.contains(entityType);
  }

  public static boolean isUnsupportedEntityType(String entityType) {
    return unsupportedEntityTypes.contains(entityType);
  }

  public static Set<String> getSupportedEntityTypes() {
    return new HashSet<>(supportedEntityTypes);
  }

  public static Set<String> getUnsupportedEntityTypes() {
    return new HashSet<>(unsupportedEntityTypes);
  }

  private static boolean isKnownEntityType(String entityType) {
    return RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_ENTITY_TYPES.contains(entityType)
        || RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_ENTITY_TYPES.contains(entityType)
        || RenderEntityTypeSupportDefaults.KNOWN_SUPPORTED_THIRD_PARTY_ENTITY_TYPES.contains(
            entityType)
        || RenderEntityTypeSupportDefaults.KNOWN_UNSUPPORTED_THIRD_PARTY_ENTITY_TYPES.contains(
            entityType);
  }

  private static boolean isInvalidEntityType(String entityType) {
    return entityType == null || !entityType.contains(":");
  }
}
