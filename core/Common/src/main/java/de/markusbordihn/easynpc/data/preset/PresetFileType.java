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

package de.markusbordihn.easynpc.data.preset;

public enum PresetFileType {
  NBT(".npc.nbt"),
  SNBT(".npc.snbt");

  private final String extension;

  PresetFileType(String extension) {
    this.extension = extension;
  }

  public static PresetFileType fromFilename(String filename) {
    for (PresetFileType presetFileType : PresetFileType.values()) {
      if (filename.endsWith(presetFileType.extension)) {
        return presetFileType;
      }
    }
    return null;
  }

  public static boolean isNBT(String filename) {
    return filename.endsWith(NBT.extension);
  }

  public static boolean isSNBT(String filename) {
    return filename.endsWith(SNBT.extension);
  }

  public static boolean isSupported(String filename) {
    return isNBT(filename) || isSNBT(filename);
  }

  public static String removeFileExtension(String filename) {
    for (PresetFileType presetFileType : PresetFileType.values()) {
      if (filename.endsWith(presetFileType.extension)) {
        return filename.substring(0, filename.length() - presetFileType.extension.length());
      }
    }
    return filename;
  }

  public String getExtension() {
    return extension;
  }
}
