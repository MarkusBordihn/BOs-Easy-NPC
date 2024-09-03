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
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PlayerSkinDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String DATA_FOLDER_NAME = "player_skin";

  private PlayerSkinDataFiles() {}

  public static void registerPlayerSkinData() {
    log.info("{} player skin data ...", Constants.LOG_REGISTER_PREFIX);

    // Prepare skin data folder
    Path skinDataFolder = getPlayerSkinDataFolder();
    if (skinDataFolder == null) {
      return;
    }

    // Prepare skin model folders and pre-register textures
    for (SkinModel skinModel : SkinModel.values()) {
      if (skinModel == SkinModel.HUMANOID || skinModel == SkinModel.HUMANOID_SLIM) {
        Path skinModelFolder = getPlayerSkinDataFolder(skinModel);
        if (skinModelFolder != null
            && Files.exists(skinModelFolder)
            && Files.isDirectory(skinModelFolder)) {
          for (String skinFileName : skinModelFolder.toFile().list()) {
            Path skinFilePath = skinModelFolder.resolve(skinFileName);
            File skinFile = skinFilePath.toFile();
            if (skinFile.exists() && skinFileName.endsWith(".png")) {
              PlayerTextureManager.registerTexture(skinModel, skinFile);
            }
          }
        }
      }
    }
  }

  public static Path getPlayerSkinDataFolder() {
    return DataFileHandler.getOrCreateCacheFolder(DATA_FOLDER_NAME);
  }

  public static Path getPlayerSkinDataFolder(SkinModel skinModel) {
    Path playerSkinDataFolder = getPlayerSkinDataFolder();
    if (playerSkinDataFolder == null) {
      return null;
    }
    String skinModelName = skinModel.getName();
    Path skinDataFolderPath = playerSkinDataFolder.resolve(skinModelName);
    try {
      if (Files.exists(skinDataFolderPath) && Files.isDirectory(skinDataFolderPath)) {
        return skinDataFolderPath;
      }
      log.info("Created new player skin data folder {} at {}!", skinModelName, skinDataFolderPath);
      return Files.createDirectories(skinDataFolderPath);
    } catch (IOException e) {
      log.error(
          "Could not create player skin data folder {} at {}:",
          skinModelName,
          playerSkinDataFolder.resolve(skinModelName),
          e);
    }
    return null;
  }
}
