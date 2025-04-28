package de.markusbordihn.easynpc.client.renderer;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import java.util.UUID;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;

public class EasyNPCLivingEntityRenderer {

  public static void handleRenderStart(
      LivingEntityRenderState renderState,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight) {
    scaleEasyNPC(renderState, poseStack);
  }

  public static void handleRenderEnd(
      LivingEntityRenderState renderState,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight) {}

  public static void scaleEasyNPC(LivingEntityRenderState renderState, PoseStack poseStack) {
    EasyNPC<?> easyNPC = getEasyNPC(renderState);
    if (easyNPC == null) {
      return;
    }

    ScaleData<?> scaleData = easyNPC.getEasyNPCScaleData();
    if (scaleData == null) {
      return;
    }

    float defaultScaleX = scaleData.getDefaultScaleX();
    float defaultScaleY = scaleData.getDefaultScaleY();
    float defaultScaleZ = scaleData.getDefaultScaleZ();
    if (defaultScaleX != 1.0f || defaultScaleY != 1.0f || defaultScaleZ != 1.0f) {
      poseStack.scale(defaultScaleX, defaultScaleY, defaultScaleZ);
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
