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
import de.markusbordihn.easynpc.client.pose.PoseManager;
import de.markusbordihn.easynpc.data.animation.AnimationData;
import de.markusbordihn.easynpc.data.animation.AnimationDataReader;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.stream.Stream;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.packs.resources.Resource;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CustomPoseDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String DATA_FOLDER_NAME = "pose";
  protected static final String TEMPLATE_PREFIX = "_poses.json";

  private CustomPoseDataFiles() {}

  public static void registerCustomPoseData(MinecraftServer minecraftServer) {
    log.info("{} custom pose data ...", Constants.LOG_REGISTER_PREFIX);

    // Prepare pose data folder
    Path poseDataFolder = getCustomPoseDataFolder();
    if (poseDataFolder == null) {
      return;
    }

    // Prepare pose model folders
    for (SkinModel skinModel : SkinModel.values()) {
      Path poseModelFolder = getCustomPoseDataFolder(skinModel);
      if (poseModelFolder == null) {
        continue;
      }

      // Get all pose files from this mod's resources only (filter by namespace).
      String skinModelName = skinModel.getName();
      Map<ResourceLocation, Resource> resourceLocations =
          minecraftServer
              .getResourceManager()
              .listResources(
                  DataFileHandler.RESOURCE_POSES_PATH + "/" + skinModelName,
                  fileName ->
                      fileName.getNamespace().equals(Constants.MOD_ID)
                          && fileName.toString().endsWith(TEMPLATE_PREFIX));

      // Copy all default pose files to the pose data folder.
      for (ResourceLocation resourceLocation : resourceLocations.keySet()) {
        File skinModelPoseFile =
            poseModelFolder
                .resolve(DataFileHandler.getFileNameFromResourceLocation(resourceLocation))
                .toFile();
        if (DataFileHandler.copyResourceFile(minecraftServer, resourceLocation, skinModelPoseFile)
            && skinModelPoseFile.exists()
            && skinModelPoseFile.length() > 0) {
          log.info("Copied skin model pose file {} to {}", resourceLocation, skinModelPoseFile);
        }
      }
    }

    registerCustomPoseFiles();
  }

  public static void registerCustomPoseFiles() {
    Path poseDataFolder = getCustomPoseDataFolder();
    if (poseDataFolder == null) {
      return;
    }

    log.info("{} custom poses from {} ...", Constants.LOG_REGISTER_PREFIX, poseDataFolder);
    for (SkinModel skinModel : SkinModel.values()) {
      Path poseModelFolder = getCustomPoseDataFolder(skinModel);
      if (poseModelFolder != null
          && poseModelFolder.toFile().exists()
          && poseModelFolder.toFile().isDirectory()) {
        try (Stream<Path> posePaths = Files.walk(poseModelFolder)) {
          posePaths
              .filter(
                  path -> Files.isRegularFile(path) && path.toString().endsWith(TEMPLATE_PREFIX))
              .forEach(
                  path -> {
                    log.info("Found custom pose file {} ...", path);
                    try {
                      AnimationData animationData = AnimationDataReader.parseAnimationFile(path);
                      PoseManager.registerPoseData(skinModel, animationData);
                    } catch (IOException e) {
                      throw new RuntimeException(e);
                    }
                  });
        } catch (IOException e) {
          log.error("Error reading custom pose files from {}:", poseModelFolder, e);
        }
      }
    }
  }

  public static Path getCustomPoseDataFolder() {
    return DataFileHandler.getOrCreateCustomDataFolder(DATA_FOLDER_NAME);
  }

  public static Path getCustomPoseDataFolder(SkinModel skinModel) {
    Path poseDataFolder = getCustomPoseDataFolder();
    if (poseDataFolder == null) {
      return null;
    }
    String skinModelName = skinModel.getName();
    Path poseDataFolderPath = poseDataFolder.resolve(skinModelName);
    try {
      if (Files.exists(poseDataFolderPath) && Files.isDirectory(poseDataFolderPath)) {
        return poseDataFolderPath;
      }
      log.info("Created new pose data folder {} at {}!", skinModelName, poseDataFolderPath);
      return Files.createDirectories(poseDataFolderPath);
    } catch (IOException e) {
      log.error("Error creating pose data folder {} at {}:", skinModelName, poseDataFolderPath, e);
    }
    return null;
  }
}
