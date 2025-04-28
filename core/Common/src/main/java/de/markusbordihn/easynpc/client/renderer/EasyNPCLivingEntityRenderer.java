package de.markusbordihn.easynpc.client.renderer;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.world.entity.Entity;

public class EasyNPCLivingEntityRenderer {

  public static void handleRenderStart(
      EasyNPC<?> easyNPC, PoseStack poseStack, MultiBufferSource bufferSource, int packedLight) {
    scaleEasyNPC(easyNPC, poseStack);
  }

  public static void handleRenderEnd(
      EasyNPC<?> easyNPC, PoseStack poseStack, MultiBufferSource bufferSource, int packedLight) {}

  public static void scaleEasyNPC(EasyNPC<?> easyNPC, PoseStack poseStack) {
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

  public static EasyNPC<?> getEasyNPC(Entity entity) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      return easyNPC;
    }
    return null;
  }
}
