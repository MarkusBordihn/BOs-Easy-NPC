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

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.npc.UserDefinedConfiguration;
import de.markusbordihn.easynpc.data.npc.UserDefinedConfigurationManager;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.world.entity.EntityType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class UserDefinedEntityRegistry {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<UserDefinedEntityType, EntityType<?>> registeredEntityTypes =
      new ConcurrentHashMap<>();
  private static final Set<UserDefinedEntityType> availableEntityTypes =
      ConcurrentHashMap.newKeySet();
  private static boolean initialized = false;

  static {
    initialize();
  }

  private UserDefinedEntityRegistry() {}

  public static void initialize() {
    if (initialized) {
      log.debug("User-defined entity registry already initialized");
      return;
    }

    log.info("Initializing user-defined NPC registry...");

    try {
      Map<String, UserDefinedConfiguration> configurations =
          UserDefinedConfigurationManager.getUserDefinedConfigurations();

      if (configurations.isEmpty()) {
        log.info("No user-defined NPCs found in configuration");
        return;
      }

      int successfulRegistrations = 0;
      for (Map.Entry<String, UserDefinedConfiguration> entry : configurations.entrySet()) {
        String npcId = entry.getKey();
        UserDefinedConfiguration configuration = entry.getValue();

        if (registerUserDefinedNPC(configuration)) {
          successfulRegistrations++;
          log.debug("Successfully registered user-defined NPC: {}", npcId);
        } else {
          log.warn("Failed to register user-defined NPC: {}", npcId);
        }
      }

      log.info(
          "Successfully registered {} out of {} user-defined NPCs",
          successfulRegistrations,
          configurations.size());

    } catch (Exception e) {
      log.error("Failed to initialize user-defined entity registry: {}", e.getMessage(), e);
    } finally {
      initialized = true;
    }
  }

  private static boolean registerUserDefinedNPC(UserDefinedConfiguration configuration) {
    if (!configuration.isValid()) {
      log.error("Invalid configuration for NPC: {}", configuration.id());
      return false;
    }

    try {
      UserDefinedEntityType entityType = createEntityType(configuration);
      availableEntityTypes.add(entityType);
      return true;
    } catch (Exception e) {
      log.error(
          "Failed to register user-defined NPC {}: {}", configuration.id(), e.getMessage(), e);
      return false;
    }
  }

  private static UserDefinedEntityType createEntityType(UserDefinedConfiguration configuration) {
    return new UserDefinedEntityType(
        configuration.id(),
        configuration.baseEntityType(),
        configuration.width(),
        configuration.height());
  }

  public static Set<UserDefinedEntityType> getAvailableEntityTypes() {
    if (!initialized) {
      initialize();
    }
    return Collections.unmodifiableSet(availableEntityTypes);
  }

  public static EntityType<?> getRegisteredEntityType(UserDefinedEntityType userDefinedType) {
    return registeredEntityTypes.get(userDefinedType);
  }

  public static void registerEntityType(
      UserDefinedEntityType userDefinedType, EntityType<?> entityType) {
    if (userDefinedType == null || entityType == null) {
      log.error("Cannot register null entity type or user-defined type");
      return;
    }

    registeredEntityTypes.put(userDefinedType, entityType);
    log.debug("Registered entity type for user-defined NPC: {}", userDefinedType.getId());
  }

  public static boolean isRegistered(UserDefinedEntityType userDefinedType) {
    return registeredEntityTypes.containsKey(userDefinedType);
  }

  public static int getRegisteredCount() {
    return registeredEntityTypes.size();
  }

  public static int getAvailableCount() {
    return availableEntityTypes.size();
  }

  public static void reload() {
    log.info("Reloading user-defined entity registry...");
    availableEntityTypes.clear();
    registeredEntityTypes.clear();
    initialized = false;
    initialize();
  }
}
