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

import java.text.Normalizer;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

public final class ResourceNameNormalizer {

  private static final Map<Character, String> TRANSLITERATIONS =
      Map.ofEntries(
          Map.entry('ä', "ae"),
          Map.entry('ö', "oe"),
          Map.entry('ü', "ue"),
          Map.entry('ß', "ss"),
          Map.entry('å', "aa"),
          Map.entry('æ', "ae"),
          Map.entry('ø', "oe"),
          Map.entry('œ', "oe"),
          Map.entry('þ', "th"),
          Map.entry('ð', "dh"),
          Map.entry('đ', "d"),
          Map.entry('ł', "l"),
          Map.entry('ı', "i"),
          Map.entry('ŋ', "ng"),
          Map.entry('&', "_and_"),
          Map.entry('+', "_plus_"));
  private static final Pattern COMBINING_MARKS = Pattern.compile("\\p{M}+");
  private static final Pattern VALID_IDENTIFIER = Pattern.compile("[a-z0-9_]+");
  private static final Pattern VALID_RESOURCE_PATH = Pattern.compile("[a-z0-9_./-]+");
  private static final Pattern VALID_RESOURCE_PATH_PATTERN = Pattern.compile("[a-z0-9_./*-]+");
  private static final Pattern VALID_FILE_NAME = Pattern.compile("[a-zA-Z0-9_.-]+");
  private static final Pattern INVALID_IDENTIFIER_CHARACTERS = Pattern.compile("[^a-z0-9_]+");
  private static final Pattern INVALID_RESOURCE_PATH_CHARACTERS = Pattern.compile("[^a-z0-9_./-]+");
  private static final Pattern INVALID_FILE_NAME_CHARACTERS = Pattern.compile("[^a-zA-Z0-9_.-]+");
  private static final Pattern REPEATED_UNDERSCORES = Pattern.compile("_{2,}");
  private static final Pattern LEADING_OR_TRAILING_SEPARATORS = Pattern.compile("^[_.-]+|[_.-]+$");
  private static final String RELATIVE_SEGMENT = "..";
  private static final int MINIMUM_PATTERN_PREFIX_LENGTH = 3;
  private static final int HASH_SEED = 0x811c9dc5;
  private static final int HASH_PRIME = 0x01000193;

  private ResourceNameNormalizer() {}

  public static String toIdentifier(String value) {
    if (value == null || value.isEmpty()) {
      return "";
    }
    if (VALID_IDENTIFIER.matcher(value).matches()) {
      return value;
    }

    return collapse(
        INVALID_IDENTIFIER_CHARACTERS.matcher(transliterate(value, true)).replaceAll("_"));
  }

  public static String toIdentifier(String value, String fallbackPrefix, int maxLength) {
    String identifier = toIdentifier(value);
    if (identifier.isEmpty()) {
      identifier = fallbackPrefix + "_" + hash(value);
    }

    return identifier.length() > maxLength ? identifier.substring(0, maxLength) : identifier;
  }

  public static String toResourcePath(String value) {
    if (value == null || value.isEmpty()) {
      return "";
    }
    if (VALID_RESOURCE_PATH.matcher(value).matches() && !hasRelativeSegment(value)) {
      return value;
    }

    String resourcePath =
        collapse(
            INVALID_RESOURCE_PATH_CHARACTERS.matcher(transliterate(value, true)).replaceAll("_"));
    return joinSegments(resourcePath);
  }

  public static Pattern toPresetPathPattern(String value) {
    if (value == null || value.isEmpty()) {
      return null;
    }

    String path = value.toLowerCase(Locale.ROOT);
    if (!VALID_RESOURCE_PATH_PATTERN.matcher(path).matches() || hasRelativeSegment(path)) {
      return null;
    }

    int firstWildcard = path.indexOf('*');
    if (firstWildcard >= 0 && firstWildcard < MINIMUM_PATTERN_PREFIX_LENGTH) {
      return null;
    }

    StringBuilder regularExpression = new StringBuilder();
    String[] literals = path.split("\\*", -1);
    for (int i = 0; i < literals.length; i++) {
      if (i > 0) {
        regularExpression.append(".*");
      }
      if (!literals[i].isEmpty()) {
        regularExpression.append(Pattern.quote(literals[i]));
      }
    }

    return Pattern.compile(regularExpression.toString());
  }

  public static String toFileName(String value) {
    if (value == null || value.isEmpty()) {
      return "";
    }
    if (VALID_FILE_NAME.matcher(value).matches() && !value.contains(RELATIVE_SEGMENT)) {
      return value;
    }

    String fileName =
        collapse(INVALID_FILE_NAME_CHARACTERS.matcher(transliterate(value, false)).replaceAll("_"));
    return fileName.contains(RELATIVE_SEGMENT)
        ? collapse(fileName.replace(RELATIVE_SEGMENT, "_"))
        : fileName;
  }

  public static String toFileName(String value, String fallbackPrefix) {
    String fileName = toFileName(value);
    return fileName.isEmpty() ? fallbackPrefix + "_" + hash(value) : fileName;
  }

  public static String toResourcePath(String value, String fallbackPrefix) {
    String resourcePath = toResourcePath(value);
    return resourcePath.isEmpty() ? fallbackPrefix + "_" + hash(value) : resourcePath;
  }

  public static String hash(String value) {
    int hash = HASH_SEED;
    String text = value == null ? "" : value;
    for (int index = 0; index < text.length(); index++) {
      hash ^= text.charAt(index);
      hash *= HASH_PRIME;
    }

    return String.format("%08x", hash);
  }

  private static String transliterate(String value, boolean lowerCase) {
    String source = lowerCase ? value.toLowerCase(Locale.ROOT) : value;
    StringBuilder transliterated = new StringBuilder(source.length());
    for (char character : source.toCharArray()) {
      String replacement = TRANSLITERATIONS.get(Character.toLowerCase(character));
      if (replacement == null) {
        transliterated.append(character);
      } else if (Character.isUpperCase(character)) {
        transliterated.append(Character.toUpperCase(replacement.charAt(0)));
        transliterated.append(replacement.substring(1));
      } else {
        transliterated.append(replacement);
      }
    }

    return COMBINING_MARKS
        .matcher(Normalizer.normalize(transliterated, Normalizer.Form.NFKD))
        .replaceAll("");
  }

  private static String collapse(String value) {
    return LEADING_OR_TRAILING_SEPARATORS
        .matcher(REPEATED_UNDERSCORES.matcher(value).replaceAll("_"))
        .replaceAll("");
  }

  private static boolean hasRelativeSegment(String value) {
    for (String segment : value.split("/")) {
      if (".".equals(segment) || RELATIVE_SEGMENT.equals(segment)) {
        return true;
      }
    }

    return false;
  }

  private static String joinSegments(String resourcePath) {
    StringBuilder joined = new StringBuilder(resourcePath.length());
    for (String segment : resourcePath.split("/")) {
      if (segment.isEmpty() || ".".equals(segment) || RELATIVE_SEGMENT.equals(segment)) {
        continue;
      }
      if (joined.length() > 0) {
        joined.append('/');
      }
      joined.append(segment);
    }

    return joined.toString();
  }
}
