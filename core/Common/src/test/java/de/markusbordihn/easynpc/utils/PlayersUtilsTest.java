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

package de.markusbordihn.easynpc.utils;

import static org.junit.jupiter.api.Assertions.*;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("PlayersUtils Tests")
class PlayersUtilsTest {

  @Test
  @DisplayName("Should parse JSON object from valid string")
  void testGetJsonObject_validJson() {
    String jsonString = "{\"key\":\"value\",\"number\":123}";
    JsonObject result = PlayersUtils.getJsonObject(jsonString);

    assertNotNull(result);
    assertTrue(result.has("key"));
    assertEquals("value", result.get("key").getAsString());
    assertTrue(result.has("number"));
    assertEquals(123, result.get("number").getAsInt());
  }

  @Test
  @DisplayName("Should return null for empty string")
  void testGetJsonObject_emptyString() {
    JsonObject result = PlayersUtils.getJsonObject("");

    assertNull(result);
  }

  @Test
  @DisplayName("Should return null for null string")
  void testGetJsonObject_nullString() {
    JsonObject result = PlayersUtils.getJsonObject(null);

    assertNull(result);
  }

  @Test
  @DisplayName("Should return null for invalid JSON")
  void testGetJsonObject_invalidJson() {
    String invalidJson = "{invalid json}";

    JsonObject result = PlayersUtils.getJsonObject(invalidJson);

    assertNull(result);
  }

  @Test
  @DisplayName("Should extract texture URL from valid texture data")
  void testExtractUserTextureUrl() {
    String textureData =
        "{"
            + "\"textures\": {"
            + "\"SKIN\": {"
            + "\"url\": \"http://textures.minecraft.net/texture/abc123\""
            + "}"
            + "}"
            + "}";

    JsonObject jsonObject = JsonParser.parseString(textureData).getAsJsonObject();
  }

  @Test
  @DisplayName("Should extract slim model from texture data")
  void testExtractUserTextureModel_slim() {
    String textureData =
        "{"
            + "\"textures\": {"
            + "\"SKIN\": {"
            + "\"url\": \"http://test.url\","
            + "\"metadata\": {"
            + "\"model\": \"slim\""
            + "}"
            + "}"
            + "}"
            + "}";

    JsonObject jsonObject = JsonParser.parseString(textureData).getAsJsonObject();
  }

  @Test
  @DisplayName("Should return default model when no metadata present")
  void testExtractUserTextureModel_default() {
    String textureData =
        "{" + "\"textures\": {" + "\"SKIN\": {" + "\"url\": \"http://test.url\"" + "}" + "}" + "}";

    JsonObject jsonObject = JsonParser.parseString(textureData).getAsJsonObject();
  }

  @Test
  @DisplayName("Should parse UUID from valid UUID string")
  void testGetUUIDfromString_validUUID() {
    String uuidString = "550e8400-e29b-41d4-a716-446655440000";
    UUID result = PlayersUtils.getUUIDfromString(uuidString);

    assertNotNull(result);
    assertEquals(uuidString, result.toString());
  }

  @Test
  @DisplayName("Should return null for invalid UUID string")
  void testGetUUIDfromString_invalidUUID() {
    String invalidUuid = "not-a-valid-uuid";
    UUID result = PlayersUtils.getUUIDfromString(invalidUuid);

    assertNull(result);
  }

  @Test
  @DisplayName("Should return null for empty UUID string")
  void testGetUUIDfromString_emptyString() {
    UUID result = PlayersUtils.getUUIDfromString("");

    assertNull(result);
  }

  @Test
  @DisplayName("Should return null for null UUID string")
  void testGetUUIDfromString_nullString() {
    UUID result = PlayersUtils.getUUIDfromString(null);

    assertNull(result);
  }

  @Test
  @DisplayName("Should parse session response with texture data")
  void testGetUserTextureFromSessionResponse_validResponse() {
    String sessionResponse =
        "{"
            + "\"properties\": ["
            + "{"
            + "\"name\": \"textures\","
            + "\"value\": \"eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXN0LnVybCJ9fX0=\""
            + // Base64 encoded: {"textures":{"SKIN":{"url":"http://test.url"}}}
            "}"
            + "]"
            + "}";
    String result = PlayersUtils.getUserTextureFromSessionResponse(sessionResponse);

    assertNotNull(result);
    assertEquals("http://test.url", result);
  }

  @Test
  @DisplayName("Should return empty string for invalid session response")
  void testGetUserTextureFromSessionResponse_invalidResponse() {
    String invalidResponse = "{\"invalid\": \"data\"}";
    String result = PlayersUtils.getUserTextureFromSessionResponse(invalidResponse);

    assertEquals("", result);
  }

  @Test
  @DisplayName("Should return empty string for empty session response")
  void testGetUserTextureFromSessionResponse_emptyResponse() {
    String result = PlayersUtils.getUserTextureFromSessionResponse("");

    assertEquals("", result);
  }

  @Test
  @DisplayName("Should return empty string for null session response")
  void testGetUserTextureFromSessionResponse_nullResponse() {
    String result = PlayersUtils.getUserTextureFromSessionResponse(null);

    assertEquals("", result);
  }

  @Test
  @DisplayName("Should handle session response without textures property")
  void testGetUserTextureFromSessionResponse_noTexturesProperty() {
    String sessionResponse =
        "{"
            + "\"properties\": ["
            + "{"
            + "\"name\": \"other\","
            + "\"value\": \"some_value\""
            + "}"
            + "]"
            + "}";
    String result = PlayersUtils.getUserTextureFromSessionResponse(sessionResponse);

    assertEquals("", result);
  }

  @Test
  @DisplayName("Should handle Base64 encoded texture data with slim model")
  void testGetUserTextureFromSessionResponse_slimModel() {
    // Base64 encoded: {"textures":{"SKIN":{"url":"http://test.url","metadata":{"model":"slim"}}}}
    String base64 =
        "eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXN0LnVybCIsIm1ldGFkYXRhIjp7Im1vZGVsIjoic2xpbSJ9fX19";
    String sessionResponse =
        "{"
            + "\"properties\": ["
            + "{"
            + "\"name\": \"textures\","
            + "\"value\": \""
            + base64
            + "\""
            + "}"
            + "]"
            + "}";
    String result = PlayersUtils.getUserTextureFromSessionResponse(sessionResponse);

    assertNotNull(result);
    assertEquals("http://test.url", result);
  }

  @Test
  @DisplayName("Should handle malformed Base64 in session response")
  void testGetUserTextureFromSessionResponse_malformedBase64() {
    String sessionResponse =
        "{"
            + "\"properties\": ["
            + "{"
            + "\"name\": \"textures\","
            + "\"value\": \"not-valid-base64!!!\""
            + "}"
            + "]"
            + "}";
    String result = PlayersUtils.getUserTextureFromSessionResponse(sessionResponse);

    assertEquals("", result);
  }
}
