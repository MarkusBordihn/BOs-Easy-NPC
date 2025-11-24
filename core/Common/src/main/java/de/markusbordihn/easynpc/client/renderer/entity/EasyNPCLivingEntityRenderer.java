package de.markusbordihn.easynpc.client.renderer.entity;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.client.texture.LivingEntityTextureManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.resources.ResourceLocation;

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

  public static ResourceLocation getTexture(
      LivingEntityRenderState livingEntityRenderState, ResourceLocation defaultTexture) {
    if (livingEntityRenderState
        instanceof EasyNPCRenderStateExtension easyNPCRenderStateExtension) {
      ResourceLocation texture = easyNPCRenderStateExtension.getEasyNpcTexture();
      if (texture != null) {
        return texture;
      }
    }
    return defaultTexture;
  }

  public static ResourceLocation extractEntityTexture(
      EasyNPC<?> easyNPC, ResourceLocation defaultTexture) {
    return LivingEntityTextureManager.getEntityTexture(easyNPC, defaultTexture);
  }

  public static ResourceLocation extractEntityPlayerTexture(
      EasyNPC<?> easyNPC, ResourceLocation defaultTexture) {
    return LivingEntityTextureManager.getEntityPlayerTexture(easyNPC, defaultTexture);
  }

  public static ResourceLocation extractEntityTextureWithDefaultCallback(
      EasyNPC<?> easyNPC,
      ResourceLocation defaultTexture,
      Supplier<ResourceLocation> defaultTextureSupplier) {
    return LivingEntityTextureManager.getEntityTextureWithDefaultCallback(
        easyNPC, defaultTexture, defaultTextureSupplier);
  }

  public static void extractAndCacheTexture(
      EasyNPC<?> easyNPC,
      EasyNPCRenderStateExtension renderStateExtension,
      LivingEntityRenderer<?, ?, ?> renderer) {
    if (renderer instanceof EasyNPCEntityRenderer easyNPCRenderer) {
      ResourceLocation defaultTexture = easyNPCRenderer.getDefaultTexture();
      renderStateExtension.setEasyNpcTexture(
          easyNPCRenderer.supportsPlayerSkins()
              ? extractEntityPlayerTexture(easyNPC, defaultTexture)
              : extractEntityTexture(easyNPC, defaultTexture));
    }
  }
}
