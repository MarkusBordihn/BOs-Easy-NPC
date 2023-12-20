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

package de.markusbordihn.easynpc.data;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.io.File;
import java.nio.file.Path;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.loading.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CustomSkinData {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String DATA_FOLDER_NAME = "skin";

  protected CustomSkinData() {}

  public static void registerCustomSkinData(final FMLClientSetupEvent event) {
    event.enqueueWork(
        () -> {
          prepareFolder();
          registerTextureFiles();
        });
  }

  public static void prepareFolder() {
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
          case FAIRY:
            CustomDataHandler.copyResourceFile(
                new ResourceLocation(Constants.MOD_ID, "textures/entity/fairy/fairy_example.png"),
                skinModelFolder.resolve("fairy_example.png").toFile());
            break;
          case HUMANOID:
            CustomDataHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/humanoid/humanoid_example.png"),
                skinModelFolder.resolve("humanoid_example.png").toFile());
            break;
          case HUMANOID_SLIM:
            CustomDataHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/humanoid_slim/humanoid_slim_example.png"),
                skinModelFolder.resolve("humanoid_slim_example.png").toFile());
            break;
          case ILLAGER:
            CustomDataHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/illager/illager_example.png"),
                skinModelFolder.resolve("illager_example.png").toFile());
            break;
          case IRON_GOLEM:
            CustomDataHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/iron_golem/iron_golem_example.png"),
                skinModelFolder.resolve("iron_golem_example.png").toFile());
            break;
          case SKELETON:
            CustomDataHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/skeleton/skeleton_example.png"),
                skinModelFolder.resolve("skeleton_example.png").toFile());
            break;
          case VILLAGER:
            CustomDataHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/villager/villager_example.png"),
                skinModelFolder.resolve("villager_example.png").toFile());
            break;
          case ZOMBIE:
            CustomDataHandler.copyResourceFile(
                new ResourceLocation(Constants.MOD_ID, "textures/entity/zombie/zombie_example.png"),
                skinModelFolder.resolve("zombie_example.png").toFile());
            break;
          case ZOMBIE_VILLAGER:
            CustomDataHandler.copyResourceFile(
                new ResourceLocation(
                    Constants.MOD_ID,
                    "textures/entity/zombie_villager/zombie_villager_example.png"),
                skinModelFolder.resolve("zombie_villager_example.png").toFile());
            break;
          default:
        }
      }
    }
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
          && skinModelFolder.toFile() != null
          && skinModelFolder.toFile().exists()
          && skinModelFolder.toFile().isDirectory()) {
        for (String skinFileName : skinModelFolder.toFile().list()) {
          Path skinFilePath = skinModelFolder.resolve(skinFileName);
          File skinFile = skinFilePath.toFile();
          if (skinFile != null && skinFile.exists() && skinFileName.endsWith(".png")) {
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
    return CustomDataHandler.getOrCreateCustomDataFolder(DATA_FOLDER_NAME);
  }

  public static Path getSkinDataFolder(SkinModel skinModel) {
    Path skinDataFolder = getSkinDataFolder();
    String skinModelName = skinModel.name();
    if (skinDataFolder != null && skinModelName != null) {
      return FileUtils.getOrCreateDirectory(skinDataFolder.resolve(skinModelName), skinModelName);
    }
    return null;
  }
}
