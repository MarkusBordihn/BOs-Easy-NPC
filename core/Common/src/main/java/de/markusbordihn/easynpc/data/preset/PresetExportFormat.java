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

package de.markusbordihn.easynpc.data.preset;

import java.util.Locale;

public enum PresetExportFormat {
  NBT(".npc.nbt"),
  SNBT(".npc.snbt"),
  JSON(".npc.json"),
  UNKNOWN("");

  private final String fileExtension;

  PresetExportFormat(String fileExtension) {
    this.fileExtension = fileExtension;
  }

  public static PresetExportFormat getDefault() {
    return NBT;
  }

  public static PresetExportFormat getPresetExportFormat(String filename) {
    if (filename == null || filename.isEmpty()) {
      return UNKNOWN;
    }

    for (PresetExportFormat format : values()) {
      if (format != UNKNOWN && filename.endsWith(format.getFileExtension())) {
        return format;
      }
    }
    return UNKNOWN;
  }

  public static String getPresetExtension(String filename) {
    PresetExportFormat format = getPresetExportFormat(filename);
    return format != UNKNOWN ? format.getFileExtension() : null;
  }

  public static String removePresetExtension(String filename) {
    String extension = getPresetExtension(filename);
    if (extension != null) {
      return filename.substring(0, filename.length() - extension.length());
    }
    return filename != null ? filename : "";
  }

  public static boolean hasPresetExtension(String filename) {
    return getPresetExtension(filename) != null;
  }

  public static String extractCleanFilename(String pathOrFilename) {
    if (pathOrFilename == null || pathOrFilename.isEmpty()) {
      return "";
    }

    // Extract filename from path
    String filename = pathOrFilename;
    int lastSlash = filename.lastIndexOf('/');
    if (lastSlash >= 0) {
      filename = filename.substring(lastSlash + 1);
    }

    // Remove preset file extensions
    return removePresetExtension(filename);
  }

  public static String normalizeFilename(String filename) {
    if (filename == null || filename.isEmpty()) {
      return "";
    }

    // Convert to lowercase and replace spaces with underscores
    return filename.toLowerCase(Locale.ROOT).replaceAll("\\s+", "_");
  }

  public String getFileExtension() {
    return fileExtension;
  }
}
