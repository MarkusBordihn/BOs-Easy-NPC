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
import java.nio.file.Path;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PlayerSkinDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String DATA_FOLDER_NAME = "player_skin";

  private PlayerSkinDataFiles() {}

  public static void registerPlayerSkinData() {
    log.debug("{} player skin data ...", Constants.LOG_REGISTER_PREFIX);

    Path skinDataFolder = getPlayerSkinDataFolder();
    if (skinDataFolder == null) {
      return;
    }

    for (SkinModel skinModel : SkinModel.values()) {
      if (skinModel == SkinModel.HUMANOID || skinModel == SkinModel.HUMANOID_SLIM) {
        DataFileHandler.forEachPngFile(
            getPlayerSkinDataFolder(skinModel),
            skinFile -> PlayerTextureManager.registerTexture(skinModel, skinFile));
      }
    }
  }

  public static Path getPlayerSkinDataFolder() {
    return DataFileHandler.getOrCreateCacheFolder(DATA_FOLDER_NAME);
  }

  public static Path getPlayerSkinDataFolder(SkinModel skinModel) {
    return DataFileHandler.getOrCreateSubdirectory(
        getPlayerSkinDataFolder(), skinModel.getName(), "player skin data");
  }
}
