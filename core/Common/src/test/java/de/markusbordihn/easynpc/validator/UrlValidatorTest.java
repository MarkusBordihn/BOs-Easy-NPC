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

package de.markusbordihn.easynpc.validator;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

@DisplayName("UrlValidator Tests")
class UrlValidatorTest {

  @ParameterizedTest
  @DisplayName("Should validate correct URLs")
  @ValueSource(
      strings = {
        "https://www.example.com/",
        "http://example.com/",
        "https://example.com/path/to/resource",
        "https://example.com:8080/path",
        "https://subdomain.example.com/",
        "https://example.com/file.png"
      })
  void testIsValidUrl_valid(String url) {
    assertTrue(UrlValidator.isValidUrl(url), "URL should be valid: " + url);
  }

  @ParameterizedTest
  @DisplayName("Should reject URLs with forbidden extensions")
  @ValueSource(
      strings = {
        "https://example.com/file.exe",
        "https://example.com/malware.jar",
        "https://example.com/script.bat",
        "https://example.com/archive.zip",
        "https://example.com/virus.dll",
        "https://example.com/hack.sh"
      })
  void testIsValidUrl_forbiddenExtensions(String url) {
    assertFalse(UrlValidator.isValidUrl(url));
  }

  @ParameterizedTest
  @DisplayName("Should reject invalid URL formats")
  @ValueSource(
      strings = {
        "not-a-url",
        "ftp://example.com",
        "javascript:alert('XSS')",
        "file:///etc/passwd",
        "//example.com",
        "https:/example.com"
      })
  void testIsValidUrl_invalidFormat(String url) {
    assertFalse(UrlValidator.isValidUrl(url));
  }

  @ParameterizedTest
  @NullAndEmptySource
  @DisplayName("Should reject null or empty URLs")
  void testIsValidUrl_nullOrEmpty(String url) {
    assertFalse(UrlValidator.isValidUrl(url));
  }

  @ParameterizedTest
  @DisplayName("Should validate secure remote URLs from whitelisted sources")
  @ValueSource(
      strings = {
        "https://www.minecraftskins.com/skin/123/player",
        "https://minecraft.novaskin.me/skin/12345",
        "https://mcskins.top/skin/test",
        "https://skinmc.net/skin/player"
      })
  void testIsSecureRemoteUrl_whitelisted(String url) {
    assertTrue(UrlValidator.isSecureRemoteUrl(url));
  }

  @ParameterizedTest
  @DisplayName("Should accept valid HTTPS URLs as secure (general case)")
  @ValueSource(
      strings = {
        "https://example.com/skin.png",
        "https://cdn.example.com/resource.png",
        "https://secure-site.com/data"
      })
  void testIsSecureRemoteUrl_validHttps(String url) {
    assertTrue(UrlValidator.isSecureRemoteUrl(url));
  }

  @ParameterizedTest
  @DisplayName("Should reject non-HTTPS URLs")
  @ValueSource(
      strings = {
        "http://example.com/skin.png",
        "http://www.minecraftskins.com/skin/123",
        "ftp://example.com"
      })
  void testIsSecureRemoteUrl_nonHttps(String url) {
    assertFalse(UrlValidator.isSecureRemoteUrl(url));
  }

  @Test
  @DisplayName("Should reject secure remote URL with forbidden extension")
  void testIsSecureRemoteUrl_forbiddenExtension() {
    assertFalse(UrlValidator.isSecureRemoteUrl("https://example.com/file.exe"));
  }

  @ParameterizedTest
  @NullAndEmptySource
  @DisplayName("Should reject null or empty secure remote URLs")
  void testIsSecureRemoteUrl_nullOrEmpty(String url) {
    assertFalse(UrlValidator.isSecureRemoteUrl(url));
  }

  @Test
  @DisplayName("Should handle malformed URLs correctly")
  void testIsValidUrl_malformed() {
    assertFalse(UrlValidator.isValidUrl("https://example .com"));
    assertFalse(UrlValidator.isValidUrl("https://[invalid]"));
  }

  @Test
  @DisplayName("Should validate URLs with query parameters")
  void testIsValidUrl_withQueryParams() {
    assertTrue(UrlValidator.isValidUrl("https://example.com/path?param=value&other=123"));
  }

  @Test
  @DisplayName("Should validate URLs with fragments")
  void testIsValidUrl_withFragments() {
    assertTrue(UrlValidator.isValidUrl("https://example.com/path#section"));
  }
}
