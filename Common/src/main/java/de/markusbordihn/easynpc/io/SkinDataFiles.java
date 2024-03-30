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
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SkinDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String DATA_FOLDER_NAME = "skin";

  protected SkinDataFiles() {
  }

  public static void registerCustomSkinData() {
    log.info("{} custom skin data ...", Constants.LOG_REGISTER_PREFIX);

    // Prepare skin data folder
    Path skinDataFolder = getSkinDataFolder();
    if (skinDataFolder == null) {
      return;
    }

    // Prepare skin model folders
    for (SkinModel skinModel : SkinModel.values()) {
      Path skinModelFolder = getSkinDataFolder(skinModel);
      if (skinModelFolder == null) {
        continue;
      }

      // Copy example skin model template files, if any.
      String skinModelName = skinModel.getName();
      ResourceLocation resourceLocation =
          new ResourceLocation(
              Constants.MOD_ID,
              "textures/entity/" + skinModelName + "/" + skinModelName + "_template.png");
      File skinModelTemplateFile =
          skinModelFolder.resolve(skinModelName + "_template.png").toFile();
      if (skinModelTemplateFile.exists()) {
        log.warn(
            "Skin model template file {} already exists, skipping copy!", skinModelTemplateFile);
      } else {
        log.info(
            "Copy skin model template file {} to {} ...", resourceLocation, skinModelTemplateFile);
        DataFileHandler.copyResourceFile(resourceLocation, skinModelTemplateFile);
      }
    }

    registerTextureFiles();
  }

  public static void registerTextureFiles() {
    Path skinDataFolder = getSkinDataFolder();
    if (skinDataFolder == null) {
      return;
    }
    log.info("{} custom skins from {} ...", Constants.LOG_REGISTER_PREFIX, skinDataFolder);
    for (SkinModel skinModel : SkinModel.values()) {
      Path skinModelFolder = getSkinDataFolder(skinModel);
      if (skinModelFolder != null
          && skinModelFolder.toFile().exists()
          && skinModelFolder.toFile().isDirectory()) {
        for (String skinFileName : skinModelFolder.toFile().list()) {
          Path skinFilePath = skinModelFolder.resolve(skinFileName);
          File skinFile = skinFilePath.toFile();
          if (skinFile.exists() && skinFileName.endsWith(".png")) {
            CustomTextureManager.registerTexture(skinModel, skinFile);
          }
        }
      }
    }
  }

  public static void refreshRegisterTextureFiles() {
    CustomTextureManager.clearCustomTextureCache();
    registerTextureFiles();
  }

  public static Path getSkinDataFolder() {
    return DataFileHandler.getOrCreateCustomDataFolder(DATA_FOLDER_NAME);
  }

  public static Path getSkinDataFolder(SkinModel skinModel) {
    Path skinDataFolder = getSkinDataFolder();
    if (skinDataFolder == null) {
      return null;
    }
    String skinModelName = skinModel.getName();
    Path skinDataFolderPath = skinDataFolder.resolve(skinModelName);
    try {
      if (Files.exists(skinDataFolderPath) && Files.isDirectory(skinDataFolderPath)) {
        return skinDataFolderPath;
      }
      log.info("Created new skin data folder {} at {}!", skinModelName, skinDataFolderPath);
      return Files.createDirectories(skinDataFolderPath);
    } catch (IOException e) {
      log.error(
          "Could not create skin data folder {} at {}!",
          skinModelName,
          skinDataFolder.resolve(skinModelName));
    }
    return null;
  }
}
