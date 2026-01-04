/*
 * Copyright 2022 Markus Bordihn
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

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import com.mojang.authlib.GameProfile;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.validator.NameValidator;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.players.GameProfileCache;
import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PlayersUtils {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String TEXTURES_STRING = "textures";
  private static final String SESSION_PROFILE_URL =
      "https://sessionserver.mojang.com/session/minecraft/profile/%s";
  private static final String API_PROFILE_URL =
      "https://api.mojang.com/users/profiles/minecraft/%s";
  private static final Map<String, UUID> userUUIDCache = new ConcurrentHashMap<>();
  private static final Map<UUID, Long> sessionServerRequestProtection = new ConcurrentHashMap<>();
  private static final long SESSION_REQUEST_COOLDOWN = 1000;

  protected PlayersUtils() {}

  public static UUID getUserUUID(MinecraftServer server, String username) {
    if (username == null || username.isEmpty() || !NameValidator.isValidPlayerName(username)) {
      log.error("Unable to get user UUID with invalid username: {}", username);
      return null;
    }

    UUID uuid = getUUIDfromString(username);
    if (uuid != null) {
      return uuid;
    }

    if (server != null) {
      try {
        GameProfileCache gameProfileCache = server.getProfileCache();
        Optional<GameProfile> optionalGameProfile = gameProfileCache.get(username);
        if (optionalGameProfile.isPresent()) {
          UUID serverUUID = optionalGameProfile.get().getId();
          log.debug("Found user {} with UUID {} from server cache", username, serverUUID);
          userUUIDCache.put(username, serverUUID);
          return serverUUID;
        }
      } catch (Exception e) {
        log.debug("Unable to get UUID from server cache for {}: {}", username, e.getMessage());
      }
    }

    // Check cache for already known or failed usernames.
    if (userUUIDCache.containsKey(username)) {
      UUID cachedUUID = userUUIDCache.get(username);
      if (cachedUUID != null) {
        log.debug("Found user {} with UUID {} from local cache", username, cachedUUID);
      }
      return cachedUUID;
    }

    // Get user UUID over API.
    try {
      String url = String.format(API_PROFILE_URL, username);
      String json = IOUtils.toString(new URL(url), StandardCharsets.UTF_8);
      JsonObject jsonObject = JsonParser.parseString(json).getAsJsonObject();
      String uuidString = jsonObject.get("id").getAsString();

      // Verify UUID string
      if (uuidString == null || uuidString.isEmpty()) {
        log.error("Unable to get user UUID with invalid response: {}", json);
        userUUIDCache.put(username, null);
        return null;
      }

      // Format UUID string and return UUID.
      String formattedUUID =
          uuidString.replaceFirst("(\\w{8})(\\w{4})(\\w{4})(\\w{4})(\\w{12})", "$1-$2-$3-$4-$5");
      UUID userUUID = UUID.fromString(formattedUUID);
      log.debug("Found user {} with UUID {} from online API", username, userUUID);
      userUUIDCache.put(username, userUUID);
      return userUUID;
    } catch (IOException e) {
      log.error("Unable to get UUID from user {}: {}", username, e.getMessage());
      userUUIDCache.put(username, null);
      return null;
    }
  }

  public static UUID getUUIDfromString(String uuidString) {
    if (uuidString == null || uuidString.isEmpty()) {
      return null;
    }
    try {
      return UUID.fromString(uuidString);
    } catch (IllegalArgumentException exception) {
      return null;
    }
  }

  public static UUID getUserUUID(String username) {
    return getUserUUID(null, username);
  }

  public static String getUserTexture(UUID userUUID) {
    // Session server spam protection: prevent duplicate requests within cooldown period
    long currentTime = System.currentTimeMillis();
    Long lastRequest = sessionServerRequestProtection.get(userUUID);
    if (lastRequest != null && currentTime - lastRequest < SESSION_REQUEST_COOLDOWN) {
      log.debug(
          "Ignoring duplicate session server request for {} (within cooldown period)", userUUID);
      return null;
    }

    // Use putIfAbsent to avoid race condition - only one thread should make the request
    Long existingRequest = sessionServerRequestProtection.putIfAbsent(userUUID, currentTime);
    if (existingRequest != null && currentTime - existingRequest < SESSION_REQUEST_COOLDOWN) {
      log.debug(
          "Ignoring duplicate session server request for {} (another thread is handling it)",
          userUUID);
      return null;
    }

    // Create sessions request and parse result, if any.
    String sessionURL = String.format(SESSION_PROFILE_URL, userUUID);
    try {
      String data = IOUtils.toString(new URL(sessionURL), StandardCharsets.UTF_8);
      if (data == null || data.isEmpty()) {
        log.error("Unable to get user texture with {}", sessionURL);
        return null;
      }
      return getUserTextureFromSessionResponse(data);
    } catch (IOException ioException) {
      log.error("Unable to get user texture with {}:", sessionURL, ioException);
      return null;
    }
  }

  public static String getUserTextureFromSessionResponse(String data) {
    JsonObject jsonObject = getJsonObject(data);
    if (jsonObject == null || !jsonObject.has("properties")) {
      log.error("Unable to get valid JSON data from session response: {}", data);
      return "";
    }

    JsonArray properties = jsonObject.getAsJsonArray("properties");
    log.debug("getUserTextureFromSessionRequest: {}", properties);
    for (JsonElement property : properties) {
      JsonObject propertyObject = property.getAsJsonObject();
      if (propertyObject.has("name")
          && TEXTURES_STRING.equals(propertyObject.get("name").getAsString())
          && propertyObject.has("value")) {
        try {
          String textureData =
              new String(Base64.getDecoder().decode(propertyObject.get("value").getAsString()));

          // Parse texture data once and extract both URL and model
          JsonObject textureDataObject = getJsonObject(textureData);
          log.debug("getUserTextureFromTextureData: {}", textureDataObject);

          // Extract user texture URL and model
          String userTexture = extractUserTextureUrl(textureDataObject);
          String userTextureModel = extractUserTextureModel(textureDataObject);
          log.debug("Found user texture {} with model {} ...", userTexture, userTextureModel);
          return userTexture;
        } catch (IllegalArgumentException e) {
          log.error("Unable to decode Base64 texture data: {}", e.getMessage());
          return "";
        }
      }
    }

    log.error("Unable to get user texture from session response: {}", data);
    return "";
  }

  private static String extractUserTextureUrl(JsonObject textureDataObject) {
    if (textureDataObject != null && textureDataObject.has(TEXTURES_STRING)) {
      JsonObject textureObject = textureDataObject.getAsJsonObject(TEXTURES_STRING);
      if (textureObject.has("SKIN")) {
        JsonObject skinObject = textureObject.getAsJsonObject("SKIN");
        if (skinObject.has("url")) {
          return skinObject.get("url").getAsString();
        }
      }
    }
    log.error("Unable to get user texture from texture data: {}", textureDataObject);
    return "";
  }

  private static String extractUserTextureModel(JsonObject textureDataObject) {
    if (textureDataObject != null && textureDataObject.has(TEXTURES_STRING)) {
      JsonObject textureObject = textureDataObject.getAsJsonObject(TEXTURES_STRING);
      if (textureObject.has("SKIN")) {
        JsonObject skinObject = textureObject.getAsJsonObject("SKIN");
        if (skinObject.has("metadata")) {
          JsonObject metaDataObject = skinObject.getAsJsonObject("metadata");
          if (metaDataObject.has("model")) {
            return metaDataObject.get("model").getAsString();
          }
        }
      }
    }
    log.debug(
        "Unable to get user texture model from texture data, will use default: {}",
        textureDataObject);
    return "default";
  }

  public static JsonObject getJsonObject(String data) {
    if (data == null || data.isEmpty()) {
      return null;
    }
    try {
      JsonElement jsonElement = JsonParser.parseString(data);
      if (jsonElement != null && jsonElement.isJsonObject()) {
        return jsonElement.getAsJsonObject();
      }
    } catch (JsonParseException jsonParseException) {
      log.error("ERROR: Unable to parse json data: {}", data);
    }
    return null;
  }
}
