package de.markusbordihn.easynpc.client.renderer.entity;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.client.renderer.MultiBufferSource;

public class EasyNPCLivingEntityRenderer {

  public static void handleRenderStart(
      EasyNPC<?> easyNPC, PoseStack poseStack, MultiBufferSource bufferSource, int packedLight) {}

  public static void handleRenderEnd(
      EasyNPC<?> easyNPC, PoseStack poseStack, MultiBufferSource bufferSource, int packedLight) {}

  public static void handleScale(EasyNPC<?> easyNPC, PoseStack poseStack) {
    if (easyNPC == null || poseStack == null) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return;
    }

    // Apply default scale to the model.
    CustomScale defaultScale = modelData.getDefaultModelScale();
    if (defaultScale != null && defaultScale.hasChanged()) {
      poseStack.scale(defaultScale.x(), defaultScale.y(), defaultScale.z());
    }

    // Apply custom scale to the model.
    CustomScale customScale = modelData.getModelPartScale(ModelPartType.ROOT);
    if (customScale != null && customScale.hasChanged()) {
      poseStack.scale(customScale.x(), customScale.y(), customScale.z());
    }
  }
}
