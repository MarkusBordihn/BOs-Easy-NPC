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

  protected SkinDataFiles() {}

  public static void registerCustomSkinData() {
    log.info("{} custom skin data ...", Constants.LOG_REGISTER_PREFIX);

    // Prepare skin data folder
    Path skinDataFolder = getSkinDataFolder();
    if (skinDataFolder == null) {
      return;
    }
    log.info("{} custom skin data folder at {} ...", Constants.LOG_CREATE_PREFIX, skinDataFolder);

    for (SkinModel skinModel : SkinModel.values()) {
      Path skinModelFolder = getSkinDataFolder(skinModel);
      if (skinModelFolder != null) {
        log.info(
            "{} skin model folder {} at {} ...",
            Constants.LOG_CREATE_PREFIX,
            skinModel,
            skinModelFolder);

        // Copy example skin files, if any.
        switch (skinModel) {
          case ALLAY:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(Constants.MOD_ID, "textures/entity/allay/allay_example.png"),
                skinModelFolder.resolve("allay_example.png").toFile());
            break;
          case CAT:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(Constants.MOD_ID, "textures/entity/cat/cat_example.png"),
                skinModelFolder.resolve("cat_example.png").toFile());
            break;
          case CHICKEN:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/chicken/chicken_example.png"),
                skinModelFolder.resolve("chicken_example.png").toFile());
            break;
          case FAIRY:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(Constants.MOD_ID, "textures/entity/fairy/fairy_example.png"),
                skinModelFolder.resolve("fairy_example.png").toFile());
            break;
          case HUMANOID:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/humanoid/humanoid_example.png"),
                skinModelFolder.resolve("humanoid_example.png").toFile());
            break;
          case HUMANOID_SLIM:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/humanoid_slim/humanoid_slim_example.png"),
                skinModelFolder.resolve("humanoid_slim_example.png").toFile());
            break;
          case ILLAGER:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/illager/illager_example.png"),
                skinModelFolder.resolve("illager_example.png").toFile());
            break;
          case IRON_GOLEM:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/iron_golem/iron_golem_example.png"),
                skinModelFolder.resolve("iron_golem_example.png").toFile());
            break;
          case SKELETON:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/skeleton/skeleton_example.png"),
                skinModelFolder.resolve("skeleton_example.png").toFile());
            break;
          case VILLAGER:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/villager/villager_example.png"),
                skinModelFolder.resolve("villager_example.png").toFile());
            break;
          case ZOMBIE:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(Constants.MOD_ID, "textures/entity/zombie/zombie_example.png"),
                skinModelFolder.resolve("zombie_example.png").toFile());
            break;
          case ZOMBIE_VILLAGER:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID,
                    "textures/entity/zombie_villager/zombie_villager_example.png"),
                skinModelFolder.resolve("zombie_villager_example.png").toFile());
            break;
          case PIG:
            DataFileHandler.copyResourceFile(
                new ResourceLocation(Constants.MOD_ID, "textures/entity/pig/pig_example.png"),
                skinModelFolder.resolve("pig_example.png").toFile());
            break;
          default:
        }
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
    String skinModelName = skinModel.name();
    if (skinDataFolder != null) {
      try {
        return Files.createDirectories(skinDataFolder.resolve(skinModelName));
      } catch (IOException e) {
        log.error(
            "Could not create skin data folder for {} at {}!",
            skinModelName,
            skinDataFolder.resolve(skinModelName));
      }
    }
    return null;
  }
}
