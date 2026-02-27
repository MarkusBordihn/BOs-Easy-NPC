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
import java.io.InputStream;
import java.util.Map;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.packs.resources.Resource;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CustomPoseDataFiles {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String TEMPLATE_SUFFIX = ".json";

  private CustomPoseDataFiles() {}

  public static void registerCustomPoseData(MinecraftServer minecraftServer) {
    log.info("{} custom pose data from data packs ...", Constants.LOG_REGISTER_PREFIX);

    PoseManager.clearPoseData();

    for (SkinModel skinModel : SkinModel.values()) {
      String skinModelName = skinModel.getName();
      String resourcePath = DataFileHandler.RESOURCE_POSES_PATH + "/" + skinModelName;

      Map<ResourceLocation, Resource> resources =
          minecraftServer
              .getResourceManager()
              .listResources(
                  resourcePath, fileName -> fileName.toString().endsWith(TEMPLATE_SUFFIX));

      for (Map.Entry<ResourceLocation, Resource> entry : resources.entrySet()) {
        ResourceLocation resourceLocation = entry.getKey();
        Resource resource = entry.getValue();
        try (InputStream inputStream = resource.open()) {
          AnimationData animationData =
              AnimationDataReader.parseAnimationStream(inputStream, resourceLocation.toString());
          if (animationData != null) {
            PoseManager.registerPoseData(skinModel, animationData);
            log.debug("Loaded pose {} for {}", resourceLocation, skinModelName);
          }
        } catch (Exception e) {
          log.error("Error loading pose {} for {}:", resourceLocation, skinModelName, e);
        }
      }
    }
  }
}
