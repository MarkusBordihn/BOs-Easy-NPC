package de.markusbordihn.easynpc.mixin.renderer;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCLivingEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.world.entity.LivingEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(LivingEntityRenderer.class)
public class EasyNPCLivingEntityRendererMixin {

  @Inject(method = "extractRenderState", at = @At("TAIL"))
  private void injectEasyNpcUUID(
      LivingEntity livingEntity,
      LivingEntityRenderState renderState,
      float partialTicks,
      CallbackInfo ci) {
    if (livingEntity instanceof EasyNPC<?> easyNPC
        && renderState instanceof EasyNPCRenderStateExtension renderStateExtension) {
      renderStateExtension.setEasyNpcUUID(easyNPC.getEntityUUID());
    }
  }

  @Inject(method = "render", at = @At("HEAD"))
  private void onRenderStart(
      LivingEntityRenderState renderState,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight,
      CallbackInfo ci) {
    if (renderState instanceof EasyNPCRenderStateExtension renderStateExtension) {
      EasyNPCLivingEntityRenderer.handleRenderStart(
          renderState, poseStack, bufferSource, packedLight);
    }
  }

  @Inject(method = "render", at = @At("TAIL"))
  private void onRenderEnd(
      LivingEntityRenderState renderState,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight,
      CallbackInfo ci) {
    if (renderState instanceof EasyNPCRenderStateExtension renderStateExtension) {
      EasyNPCLivingEntityRenderer.handleRenderEnd(
          renderState, poseStack, bufferSource, packedLight);
    }
  }

  @Inject(method = "scale", at = @At("HEAD"))
  private void onScale(LivingEntityRenderState renderState, PoseStack poseStack, CallbackInfo ci) {
    if (renderState instanceof EasyNPCRenderStateExtension renderStateExtension) {
      EasyNPCLivingEntityRenderer.handleScale(renderState, poseStack);
    }
  }
}
