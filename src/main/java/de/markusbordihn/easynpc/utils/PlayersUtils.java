/**
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

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;

import com.mojang.authlib.GameProfile;

import net.minecraft.network.chat.Component;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.players.GameProfileCache;

import de.markusbordihn.easynpc.Constants;

public class PlayersUtils {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String USER_REGEX = "^\\w{2,16}$";
  private static final String TEXTURES_STRING = "textures";

  // Internal Cache
  private static UUID lastUserUUIDForUserTexture;

  protected PlayersUtils() {}

  public static Optional<GameProfile> getGameProfile(MinecraftServer server, Component component) {
    return getGameProfile(server, component.getString());
  }

  public static Optional<GameProfile> getGameProfile(MinecraftServer server, String username) {
    if (server == null || username == null || username.isEmpty()) {
      return Optional.empty();
    }
    final GameProfileCache gameProfileCache = server.getProfileCache();
    return gameProfileCache != null ? gameProfileCache.get(username) : Optional.empty();
  }

  public static UUID getUserUUID(MinecraftServer server, String username) {
    // Verify is username is not already a user uuid
    UUID uuid = getUUIDfromString(username);
    if (uuid != null) {
      return uuid;
    }

    Optional<GameProfile> gameProfile = PlayersUtils.getGameProfile(server, username);
    if (gameProfile.isPresent() && gameProfile.get() != null && gameProfile.get().getId() != null) {
      String userUUID = gameProfile.get().getId().toString();
      if (userUUID != null && !userUUID.isEmpty()) {
        return getUUIDfromString(userUUID);
      }
    }
    return null;
  }

  public static UUID getUUIDfromString(String uuidString) {
    try {
      UUID uuid = UUID.fromString(uuidString);
      if (uuid != null) {
        return uuid;
      }
    } catch (IllegalArgumentException exception) {
      // Ignore the case where string is not valid UUID
    }
    return null;
  }

  public static String getUserTexture(UUID userUUID) {
    // Simple reload protected to avoid spawning to the session server.
    if (lastUserUUIDForUserTexture != null && lastUserUUIDForUserTexture.equals(userUUID)) {
      log.error("Ignore duplicated user texture request for {}!", userUUID);
      return null;
    }
    lastUserUUIDForUserTexture = userUUID;

    // Create sessions request and parse result, if any.
    String sessionURL =
        String.format("https://sessionserver.mojang.com/session/minecraft/profile/%s", userUUID);
    try {
      String data = IOUtils.toString(new URL(sessionURL), StandardCharsets.UTF_8);
      if (data == null || data.isEmpty()) {
        log.error("Unable to get user texture with {}", sessionURL);
        return null;
      }
      return getUserTextureFromSessionResponse(data);
    } catch (IOException ioException) {
      log.error("Unable to get user texture with {}, because of: {}", sessionURL, ioException);
      return null;
    }
  }

  public static String getUserTextureFromSessionResponse(String data) {
    JsonObject jsonObject = getJsonObject(data);
    if (jsonObject != null && jsonObject.has("properties")) {
      JsonArray properties = jsonObject.getAsJsonArray("properties");
      log.debug("getUserTextureFromSessionRequest: {}", properties);
      for (JsonElement property : properties) {
        JsonObject propertyObject = property.getAsJsonObject();
        if (propertyObject.has("name")
            && TEXTURES_STRING.equals(propertyObject.get("name").getAsString())
            && propertyObject.has("value")) {
          String textureData =
              new String(Base64.getDecoder().decode(propertyObject.get("value").getAsString()));
          String userTexture = getUserTextureFromTextureData(textureData);
          String userTextureModel = getUserTextureModelFromTextureData(textureData);
          log.debug("Found user texture {} with model {} ...", userTexture, userTextureModel);
          return userTexture;
        }
      }
    }
    return "";
  }

  public static String getUserTextureFromTextureData(String data) {
    JsonObject jsonObject = getJsonObject(data);
    log.debug("getUserTextureFromTextureData: {}", jsonObject);
    if (jsonObject != null && jsonObject.has(TEXTURES_STRING)) {
      JsonObject textureObject = jsonObject.getAsJsonObject(TEXTURES_STRING);
      if (textureObject.has("SKIN")) {
        JsonObject skinObject = textureObject.getAsJsonObject("SKIN");
        if (skinObject.has("url")) {
          return skinObject.get("url").getAsString();
        }
      }
    }
    return "";
  }

  public static String getUserTextureModelFromTextureData(String data) {
    JsonObject jsonObject = getJsonObject(data);
    log.debug("getUserTextureFromTextureData: {}", jsonObject);
    if (jsonObject != null && jsonObject.has(TEXTURES_STRING)) {
      JsonObject textureObject = jsonObject.getAsJsonObject(TEXTURES_STRING);
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
    return "";
  }

  public static JsonObject getJsonObject(String data) {
    if (data == null || data.isEmpty()) {
      return null;
    }
    JsonElement jsonElement;
    try {
      jsonElement = JsonParser.parseString(data);
      if (jsonElement != null && jsonElement.isJsonObject()) {
        return jsonElement.getAsJsonObject();
      }
    } catch (JsonParseException jsonParseException) {
      log.error("ERROR: Unable to parse json data: {}", data);
    }
    return null;
  }

  public static boolean isValidPlayerName(String name) {
    return name != null && !name.isEmpty() && !name.startsWith("http") && !name.equals("htt")
        && name.length() >= 3 && name.length() <= 16 && name.matches(USER_REGEX);
  }

  public static boolean isValidUrl(String url) {
    if (url == null || url.isEmpty()
        || (!url.startsWith("http://") && !url.startsWith("https://"))) {
      return false;
    }
    try {
      new URL(url).toURI();
    } catch (MalformedURLException | URISyntaxException e) {
      return false;
    }
    return true;
  }

}
