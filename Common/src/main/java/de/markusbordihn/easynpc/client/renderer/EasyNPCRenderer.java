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
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.EasyNPCBaseEntity;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Rotations;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.level.LightLayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface EasyNPCRenderer<E extends EasyNPCBaseEntity<E>, M extends EntityModel<E>> {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static <
      T extends LivingEntity,
      M extends HumanoidModel<T>,
      L extends RenderLayer<T, M>,
      R extends LivingEntityRenderer<T, M>>
  L getHumanoidArmorLayer(
      R mobRenderer,
      EntityRendererProvider.Context context,
      ModelLayerLocation innerArmor,
      ModelLayerLocation outerArmor,
      Class<L> armorLayerClass) {
    try {
      return armorLayerClass
          .getConstructor(RenderLayerParent.class, HumanoidModel.class, HumanoidModel.class)
          .newInstance(
              mobRenderer,
              new HumanoidModel<>(context.bakeLayer(innerArmor)),
              new HumanoidModel<>(context.bakeLayer(outerArmor)));
    } catch (Exception e) {
      log.error(
          "Failed to create custom armor layer for {} will use default armor layer instead.",
          mobRenderer,
          e);
    }
    return (L)
        new HumanoidArmorLayer<>(
            mobRenderer,
            new HumanoidModel<>(context.bakeLayer(innerArmor)),
            new HumanoidModel<>(context.bakeLayer(outerArmor)));
  }

  static void scaleEntity(EasyNPC<?> easyNPC, PoseStack poseStack) {
    LivingEntity livingEntity = easyNPC.getLivingEntity();
    ScaleData<?> scaleData = easyNPC.getEasyNPCScaleData();
    if (livingEntity.isBaby()) {
      poseStack.scale(
          scaleData.getScaleX() * 0.5f, scaleData.getScaleY() * 0.5f, scaleData.getScaleZ() * 0.5f);
    } else {
      poseStack.scale(scaleData.getScaleX(), scaleData.getScaleY(), scaleData.getScaleZ());
    }
  }

  static void renderEntityNameTag(EasyNPC<?> easyNPC, PoseStack poseStack) {
    Rotations rootRotation = easyNPC.getEasyNPCModelData().getModelRootRotation();
    if (rootRotation != null) {
      poseStack.translate(0, -1, 0);
      poseStack.mulPose(Vector3f.XP.rotation(-rootRotation.getX()));
      poseStack.mulPose(Vector3f.YP.rotation(-rootRotation.getY()));
      poseStack.mulPose(Vector3f.ZP.rotation(-rootRotation.getZ()));
      poseStack.translate(0, 1, 0);
    }
  }

  static int getEntityLightLevel(EasyNPC<?> easyNPC, BlockPos blockPos) {
    int entityLightLevel = easyNPC.getEasyNPCAttributeData().getAttributeLightLevel();
    if (entityLightLevel > 0) {
      return entityLightLevel;
    }
    LivingEntity livingEntity = easyNPC.getLivingEntity();
    return livingEntity.level.getBrightness(LightLayer.BLOCK, blockPos);
  }

  ResourceLocation getTextureByVariant(Enum<?> variant);

  ResourceLocation getDefaultTexture();

  default ResourceLocation getTextureOverlayByVariant(Enum<?> variant) {
    return Constants.BLANK_ENTITY_TEXTURE;
  }

  default ResourceLocation getCustomTexture(SkinData<?> entity) {
    return CustomTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  default ResourceLocation getPlayerTexture(SkinData<?> entity) {
    return PlayerTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  default ResourceLocation getRemoteTexture(SkinData<?> entity) {
    return RemoteTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  default void renderCustomPose(
      E entity,
      M model,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
  }

  default void renderDefaultPose(
      E entity,
      M model,
      Pose pose,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
  }

  default <N extends EasyNPC<E>> ResourceLocation getEntityTexture(N easyNPC) {
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    return switch (skinData.getSkinType()) {
      case NONE -> Constants.BLANK_ENTITY_TEXTURE;
      case CUSTOM -> getCustomTexture(skinData);
      case SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> getRemoteTexture(skinData);
      default -> getTextureByVariant(easyNPC.getEasyNPCVariantData().getVariant());
    };
  }

  default <N extends EasyNPC<E>> ResourceLocation getEntityOverlayTexture(N easyNPC) {
    if (easyNPC.getEasyNPCSkinData().getSkinType() == SkinType.DEFAULT) {
      return getTextureOverlayByVariant(easyNPC.getEasyNPCVariantData().getVariant());
    } else {
      return Constants.BLANK_ENTITY_TEXTURE;
    }
  }

  default <N extends EasyNPC<E>> ResourceLocation getEntityPlayerTexture(N easyNPC) {
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    return switch (easyNPC.getEasyNPCSkinData().getSkinType()) {
      case NONE -> Constants.BLANK_ENTITY_TEXTURE;
      case CUSTOM -> getCustomTexture(skinData);
      case PLAYER_SKIN -> getPlayerTexture(skinData);
      case SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> getRemoteTexture(skinData);
      default -> getTextureByVariant(easyNPC.getEasyNPCVariantData().getVariant());
    };
  }

  default <N extends EasyNPC<E>> void rotateEntity(N easyNPC, PoseStack poseStack) {
    Rotations rootRotation = easyNPC.getEasyNPCModelData().getModelRootRotation();
    if (rootRotation != null) {
      poseStack.translate(0, 1, 0);
      poseStack.mulPose(Vector3f.XP.rotation(rootRotation.getX()));
      poseStack.mulPose(Vector3f.YP.rotation(rootRotation.getY()));
      poseStack.mulPose(Vector3f.ZP.rotation(rootRotation.getZ()));
      poseStack.translate(0, -1, 0);
    }
  }

  default void renderModel(
      E entity,
      M model,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {

    // Rotate entity, if needed
    this.rotateEntity(entity, poseStack);

    // Render custom or default pose.
    ModelPose modelPose = entity.getModelPose();
    if (modelPose == ModelPose.DEFAULT) {
      this.renderDefaultPose(
          entity, model, entity.getPose(), entityYaw, partialTicks, poseStack, buffer, packedLight);
    } else if (modelPose == ModelPose.CUSTOM) {
      this.renderCustomPose(entity, model, entityYaw, partialTicks, poseStack, buffer, packedLight);
    }
  }
}
