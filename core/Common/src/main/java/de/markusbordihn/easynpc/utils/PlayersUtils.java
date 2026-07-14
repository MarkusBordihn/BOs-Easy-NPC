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
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
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
  private static final Map<String, CachedUUID> userUUIDCache = new ConcurrentHashMap<>();
  private static final Map<UUID, Long> sessionServerRequestProtection = new ConcurrentHashMap<>();
  private static final long SESSION_REQUEST_COOLDOWN = 1000;
  private static final long NEGATIVE_CACHE_TTL = 5L * 60 * 1000;
  private static final int CONNECT_TIMEOUT = 5000;
  private static final int READ_TIMEOUT = 5000;
  private static final int SESSION_PROTECTION_PRUNE_THRESHOLD = 1000;

  protected PlayersUtils() {}

  private static String fetchString(String urlString) throws IOException {
    URLConnection connection = new URL(urlString).openConnection();
    connection.setConnectTimeout(CONNECT_TIMEOUT);
    connection.setReadTimeout(READ_TIMEOUT);
    try (InputStream inputStream = connection.getInputStream()) {
      return IOUtils.toString(inputStream, StandardCharsets.UTF_8);
    }
  }

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
          userUUIDCache.put(username, new CachedUUID(serverUUID, Long.MAX_VALUE));
          return serverUUID;
        }
      } catch (Exception e) {
        log.debug("Unable to get UUID from server cache for {}: {}", username, e.getMessage());
      }
    }

    // Check cache for already known or failed usernames (failed lookups are cached briefly).
    CachedUUID cachedResult = userUUIDCache.get(username);
    if (cachedResult != null) {
      if (cachedResult.isExpired()) {
        userUUIDCache.remove(username);
      } else {
        if (cachedResult.uuid() != null) {
          log.debug("Found user {} with UUID {} from local cache", username, cachedResult.uuid());
        }
        return cachedResult.uuid();
      }
    }

    // Get user UUID over API.
    try {
      String url = String.format(API_PROFILE_URL, username);
      String json = fetchString(url);
      JsonObject jsonObject = JsonParser.parseString(json).getAsJsonObject();
      String uuidString = jsonObject.get("id").getAsString();

      if (uuidString == null || uuidString.isEmpty()) {
        log.error("Unable to get user UUID with invalid response: {}", json);
        cacheFailedLookup(username);
        return null;
      }

      String formattedUUID =
          uuidString.replaceFirst("(\\w{8})(\\w{4})(\\w{4})(\\w{4})(\\w{12})", "$1-$2-$3-$4-$5");
      UUID userUUID = UUID.fromString(formattedUUID);
      log.debug("Found user {} with UUID {} from online API", username, userUUID);
      userUUIDCache.put(username, new CachedUUID(userUUID, Long.MAX_VALUE));
      return userUUID;
    } catch (IOException | RuntimeException e) {
      log.error("Unable to get UUID from user {}: {}", username, e.getMessage());
      cacheFailedLookup(username);
      return null;
    }
  }

  private static void cacheFailedLookup(String username) {
    userUUIDCache.put(
        username, new CachedUUID(null, System.currentTimeMillis() + NEGATIVE_CACHE_TTL));
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
    long currentTime = System.currentTimeMillis();
    AtomicBoolean requestAllowed = new AtomicBoolean();
    sessionServerRequestProtection.compute(
        userUUID,
        (key, lastRequest) -> {
          if (lastRequest != null && currentTime - lastRequest < SESSION_REQUEST_COOLDOWN) {
            return lastRequest;
          }
          requestAllowed.set(true);
          return currentTime;
        });
    if (!requestAllowed.get()) {
      log.debug(
          "Ignoring duplicate session server request for {} (within cooldown period)", userUUID);
      return null;
    }

    if (sessionServerRequestProtection.size() > SESSION_PROTECTION_PRUNE_THRESHOLD) {
      sessionServerRequestProtection
          .entrySet()
          .removeIf(entry -> currentTime - entry.getValue() > SESSION_REQUEST_COOLDOWN);
    }

    String sessionURL = String.format(SESSION_PROFILE_URL, userUUID);
    try {
      String data = fetchString(sessionURL);
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
          JsonObject textureDataObject = getJsonObject(textureData);
          log.debug("getUserTextureFromTextureData: {}", textureDataObject);

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

  private static JsonObject getSkinObject(JsonObject textureDataObject) {
    if (textureDataObject != null && textureDataObject.has(TEXTURES_STRING)) {
      JsonObject textureObject = textureDataObject.getAsJsonObject(TEXTURES_STRING);
      if (textureObject.has("SKIN")) {
        return textureObject.getAsJsonObject("SKIN");
      }
    }
    return null;
  }

  private static String extractUserTextureUrl(JsonObject textureDataObject) {
    JsonObject skinObject = getSkinObject(textureDataObject);
    if (skinObject != null && skinObject.has("url")) {
      return skinObject.get("url").getAsString();
    }
    log.error("Unable to get user texture from texture data: {}", textureDataObject);
    return "";
  }

  private static String extractUserTextureModel(JsonObject textureDataObject) {
    JsonObject skinObject = getSkinObject(textureDataObject);
    if (skinObject != null && skinObject.has("metadata")) {
      JsonObject metaDataObject = skinObject.getAsJsonObject("metadata");
      if (metaDataObject.has("model")) {
        return metaDataObject.get("model").getAsString();
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

  private record CachedUUID(UUID uuid, long expiresAt) {
    boolean isExpired() {
      return System.currentTimeMillis() > this.expiresAt;
    }
  }
}
