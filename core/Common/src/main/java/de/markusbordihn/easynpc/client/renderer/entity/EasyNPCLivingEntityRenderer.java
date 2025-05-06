package de.markusbordihn.easynpc.client.renderer.entity;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.UUID;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;

public class EasyNPCLivingEntityRenderer {

  public static void handleRenderStart(
      LivingEntityRenderState renderState,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight) {}

  public static void handleRenderEnd(
      LivingEntityRenderState renderState,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight) {}

  public static void handleScale(LivingEntityRenderState renderState, PoseStack poseStack) {
    EasyNPC<?> easyNPC = getEasyNPC(renderState);
    if (easyNPC == null) {
      return;
    }

    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
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

  public static EasyNPC<?> getEasyNPC(LivingEntityRenderState livingEntityRenderState) {
    if (livingEntityRenderState
        instanceof EasyNPCRenderStateExtension easyNPCRenderStateExtension) {
      UUID uuid = easyNPCRenderStateExtension.getEasyNpcUUID();
      if (uuid != null) {
        return LivingEntityManager.getEasyNPCEntityByUUID(uuid);
      }
    }
    return null;
  }
}
