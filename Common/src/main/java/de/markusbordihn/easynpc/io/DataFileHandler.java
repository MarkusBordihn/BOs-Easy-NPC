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

package de.markusbordihn.easynpc.io;

import de.markusbordihn.easynpc.Constants;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import net.minecraft.client.Minecraft;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DataFileHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected DataFileHandler() {}

  public static Path getCustomDataFolder() {
    Path customDataFolder = Constants.CONFIG_DIR.resolve(Constants.MOD_ID);
    try {
      if (Files.exists(customDataFolder) && Files.isDirectory(customDataFolder)) {
        return customDataFolder;
      }
      log.info("Creating custom data folder at {} ...", customDataFolder);
      return Files.createDirectories(customDataFolder);
    } catch (Exception exception) {
      log.error("There was an error, creating the custom data folder:", exception);
    }
    return null;
  }

  public static Path getOrCreateCustomDataFolder(String dataLabel) {
    Path customDataFolder = getCustomDataFolder();
    if (customDataFolder == null) {
      return null;
    }
    Path customDataFolderPath = customDataFolder.resolve(dataLabel);
    try {
      if (Files.exists(customDataFolderPath) && Files.isDirectory(customDataFolderPath)) {
        return customDataFolderPath;
      }
      log.info("Creating custom data folder {} at {} ...", dataLabel, customDataFolder);
      return Files.createDirectories(customDataFolderPath);
    } catch (Exception exception) {
      log.error("There was an error, creating the custom data folder {}:", dataLabel, exception);
    }
    return null;
  }

  public static void copyResourceFile(ResourceLocation resourceLocation, File targetFile) {
    if (resourceLocation == null || targetFile == null) {
      return;
    }
    try {
      InputStream inputStream =
          Minecraft.getInstance()
              .getResourceManager()
              .getResource(resourceLocation)
              .getInputStream();
      try (OutputStream outputStream = new FileOutputStream(targetFile)) {
        byte[] buffer = new byte[1024];
        int length;
        while ((length = inputStream.read(buffer)) > 0) {
          outputStream.write(buffer, 0, length);
        }
      }
    } catch (Exception e) {
      log.error("Failed to load resource {}!", resourceLocation, e);
    }
  }
}
