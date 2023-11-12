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

package de.markusbordihn.easynpc.client.renderer;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Vector3f;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import net.minecraft.core.Rotations;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface EasyNPCRenderer {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  ResourceLocation getTextureByVariant(Enum<?> variant);

  ResourceLocation getDefaultTexture();

  default ResourceLocation getCustomTexture(EasyNPCEntity entity) {
    return CustomTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  default ResourceLocation getPlayerTexture(EasyNPCEntity entity) {
    return PlayerTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  default ResourceLocation getEntityTexture(EasyNPCEntity entity) {
    return switch (entity.getSkinType()) {
      case CUSTOM -> getCustomTexture(entity);
      case SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> getPlayerTexture(entity);
      default -> getTextureByVariant(entity.getVariant());
    };
  }

  default ResourceLocation getEntityPlayerTexture(EasyNPCEntity entity) {
    return switch (entity.getSkinType()) {
      case CUSTOM -> getCustomTexture(entity);
      case PLAYER_SKIN, SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> getPlayerTexture(entity);
      default -> getTextureByVariant(entity.getVariant());
    };
  }

  default void scaleEntity(EasyNPCEntity entity, PoseStack poseStack) {
    if (entity.isBaby()) {
      poseStack.scale(
          entity.getScaleX() * 0.5f, entity.getScaleY() * 0.5f, entity.getScaleZ() * 0.5f);
    } else {
      poseStack.scale(entity.getScaleX(), entity.getScaleY(), entity.getScaleZ());
    }
  }

  default void rotateEntity(EasyNPCEntity entity, PoseStack poseStack) {
    Rotations rootRotation = entity.getModelRootRotation();
    if (rootRotation != null) {
      poseStack.translate(0, 1, 0);
      poseStack.mulPose(Vector3f.XP.rotation(rootRotation.getX()));
      poseStack.mulPose(Vector3f.YP.rotation(rootRotation.getY()));
      poseStack.mulPose(Vector3f.ZP.rotation(rootRotation.getZ()));
      poseStack.translate(0, -1, 0);
    }
  }

  default void rotateEntityAlternative(EasyNPCEntity entity, PoseStack poseStack) {
    Rotations rootRotation = entity.getModelRootRotation();
    if (rootRotation != null) {
      poseStack.translate(0, 0.5, 0);
      poseStack.mulPose(Vector3f.XP.rotation(rootRotation.getX()));
      poseStack.mulPose(Vector3f.YP.rotation(rootRotation.getY()));
      poseStack.mulPose(Vector3f.ZP.rotation(rootRotation.getZ()));
      poseStack.translate(0, -0.5, 0);
    }
  }

  default void renderEntityNameTag(EasyNPCEntity entity, PoseStack poseStack) {
    Rotations rootRotation = entity.getModelRootRotation();
    if (rootRotation != null) {
      poseStack.translate(0, -1, 0);
      poseStack.mulPose(Vector3f.XP.rotation(-rootRotation.getX()));
      poseStack.mulPose(Vector3f.YP.rotation(-rootRotation.getY()));
      poseStack.mulPose(Vector3f.ZP.rotation(-rootRotation.getZ()));
      poseStack.translate(0, 1, 0);
    }
  }

  default int getEntityLightLevel(EasyNPCEntity entity) {
    return 7;
  }
}
