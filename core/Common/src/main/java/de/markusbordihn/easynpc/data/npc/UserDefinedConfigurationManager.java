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

package de.markusbordihn.easynpc.data.npc;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;
import de.markusbordihn.easynpc.Constants;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class UserDefinedConfigurationManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Gson GSON = new GsonBuilder().setPrettyPrinting().create();
  private static final String CONFIG_FILE_NAME = "user_defined_npcs.json";
  private static final String CONFIG_DIRECTORY = "config/" + Constants.MOD_ID;
  private static final File CONFIG_DIRECTORY_FILE = new File(CONFIG_DIRECTORY);
  private static final File CONFIG_FILE = new File(CONFIG_DIRECTORY, CONFIG_FILE_NAME);

  private static final Map<String, UserDefinedConfiguration> configurations = new HashMap<>();
  private static final String JSON_FIELD_WIDTH = "width";
  private static final String JSON_FIELD_HEIGHT = "height";
  private static final String JSON_FIELD_DESCRIPTION = "description";
  private static boolean initialized = false;

  private UserDefinedConfigurationManager() {}

  public static void initialize() {
    if (initialized) {
      return;
    }

    loadConfigurations();
    initialized = true;
  }

  public static Map<String, UserDefinedConfiguration> getUserDefinedConfigurations() {
    if (!initialized) {
      initialize();
    }
    return Collections.unmodifiableMap(configurations);
  }

  public static UserDefinedConfiguration getConfiguration(String configurationId) {
    if (!initialized) {
      initialize();
    }
    return configurations.get(configurationId);
  }

  public static boolean addConfiguration(UserDefinedConfiguration configuration) {
    if (!configuration.isValid()) {
      log.warn("Invalid configuration: {}", configuration);
      return false;
    }

    if (configurations.containsKey(configuration.id())) {
      log.warn("Configuration with ID {} already exists", configuration.id());
      return false;
    }

    configurations.put(configuration.id(), configuration);
    saveConfigurations();
    return true;
  }

  public static boolean removeConfiguration(String configurationId) {
    if (configurations.remove(configurationId) != null) {
      saveConfigurations();
      return true;
    }
    return false;
  }

  private static void loadConfigurations() {
    configurations.clear();

    if (!CONFIG_FILE.exists()) {
      log.info(
          "Configuration file {} does not exist, creating with example configurations",
          CONFIG_FILE_NAME);
      createExampleConfiguration();
      return;
    }

    try (FileReader reader = new FileReader(CONFIG_FILE)) {
      JsonElement jsonElement = JsonParser.parseReader(reader);
      if (!jsonElement.isJsonObject()) {
        log.error("Invalid JSON format in configuration file");
        return;
      }

      JsonObject rootObject = jsonElement.getAsJsonObject();
      JsonArray configurationsArray = rootObject.getAsJsonArray("user_defined_npcs");

      if (configurationsArray == null) {
        log.warn("No 'user_defined_npcs' array found in configuration file");
        return;
      }

      for (JsonElement configElement : configurationsArray) {
        try {
          UserDefinedConfiguration config = parseConfiguration(configElement.getAsJsonObject());
          if (config != null && config.isValid()) {
            configurations.put(config.id(), config);
          }
        } catch (Exception e) {
          log.error("Failed to parse configuration: {}", e.getMessage(), e);
        }
      }

      log.info("Loaded {} user-defined NPC configurations", configurations.size());

    } catch (IOException | JsonSyntaxException e) {
      log.error("Failed to load user-defined configurations: {}", e.getMessage(), e);
    }
  }

  private static UserDefinedConfiguration parseConfiguration(JsonObject configObject) {
    return parseConfigurationSafely(configObject);
  }

  private static UserDefinedConfiguration parseConfigurationSafely(JsonObject configObject) {
    try {
      String id = configObject.get("id").getAsString();
      String name = configObject.get("name").getAsString();
      String baseEntityTypeId = configObject.get("base_entity_type").getAsString();
      float width =
          configObject.has(JSON_FIELD_WIDTH)
              ? configObject.get(JSON_FIELD_WIDTH).getAsFloat()
              : 0.6F;
      float height =
          configObject.has(JSON_FIELD_HEIGHT)
              ? configObject.get(JSON_FIELD_HEIGHT).getAsFloat()
              : 1.95F;
      String description =
          configObject.has(JSON_FIELD_DESCRIPTION)
              ? configObject.get(JSON_FIELD_DESCRIPTION).getAsString()
              : "";

      EntityType<?> baseEntityType = parseEntityType(baseEntityTypeId);
      if (baseEntityType == null) {
        log.error("Unknown entity type: {}", baseEntityTypeId);
        return null;
      }

      return new UserDefinedConfiguration(id, name, baseEntityType, width, height, description);

    } catch (Exception e) {
      log.error("Failed to parse configuration object: {}", e.getMessage(), e);
      return null;
    }
  }

  private static EntityType<?> parseEntityType(String entityTypeId) {
    try {
      ResourceLocation resourceLocation = ResourceLocation.parse(entityTypeId);
      return BuiltInRegistries.ENTITY_TYPE.get(resourceLocation);
    } catch (Exception e) {
      log.error("Invalid entity type format: {}", entityTypeId, e);
      return null;
    }
  }

  private static void saveConfigurations() {
    try {
      ensureConfigDirectoryExists();

      JsonObject rootObject = new JsonObject();
      JsonArray configurationsArray = getConfigurationsArray();

      rootObject.add("user_defined_npcs", configurationsArray);

      try (FileWriter writer = new FileWriter(CONFIG_FILE)) {
        GSON.toJson(rootObject, writer);
      }

      log.debug("Saved {} user-defined NPC configurations", configurations.size());

    } catch (IOException e) {
      log.error("Failed to save user-defined configurations: {}", e.getMessage(), e);
    }
  }

  private static JsonArray getConfigurationsArray() {
    JsonArray configurationsArray = new JsonArray();

    for (UserDefinedConfiguration config : configurations.values()) {
      JsonObject configObject = new JsonObject();
      configObject.addProperty("id", config.id());
      configObject.addProperty("name", config.name());
      configObject.addProperty("base_entity_type", config.getBaseEntityTypeId());
      configObject.addProperty("width", config.width());
      configObject.addProperty("height", config.height());
      configObject.addProperty("description", config.description());
      configurationsArray.add(configObject);
    }
    return configurationsArray;
  }

  private static void createExampleConfiguration() {
    try {
      ensureConfigDirectoryExists();

      UserDefinedConfiguration exampleVillagerNPC =
          new UserDefinedConfiguration(
              "example_villager",
              "Villager NPC",
              EntityType.VILLAGER,
              "An example NPC based on villager");
      configurations.put(exampleVillagerNPC.id(), exampleVillagerNPC);

      UserDefinedConfiguration exampleZombieNPC =
          new UserDefinedConfiguration(
              "example_zombie", "Zombie NPC", EntityType.ZOMBIE, "An example NPC based on zombie");
      configurations.put(exampleZombieNPC.id(), exampleZombieNPC);

      saveConfigurations();

    } catch (Exception e) {
      log.error("Failed to create example configuration: {}", e.getMessage(), e);
    }
  }

  private static void ensureConfigDirectoryExists() {
    if (!CONFIG_DIRECTORY_FILE.exists() && !CONFIG_DIRECTORY_FILE.mkdirs()) {
      throw new ConfigurationDirectoryException(
          "Failed to create configuration directory: " + CONFIG_DIRECTORY);
    }
  }

  private static class ConfigurationDirectoryException extends RuntimeException {
    public ConfigurationDirectoryException(String message) {
      super(message);
    }
  }
}
