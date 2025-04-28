package de.markusbordihn.easynpc.mixin;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.renderer.EasyNPCLivingEntityRenderer;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.world.entity.LivingEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(LivingEntityRenderer.class)
public class EasyNPCLivingEntityRendererMixin {

  @Inject(
      method =
          "render(Lnet/minecraft/world/entity/LivingEntity;FFLcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/renderer/MultiBufferSource;I)V",
      at = @At("HEAD"))
  private void onRenderStart(
      LivingEntity entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight,
      CallbackInfo ci) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      EasyNPCLivingEntityRenderer.handleRenderStart(easyNPC, poseStack, bufferSource, packedLight);
    }
  }

  @Inject(
      method =
          "render(Lnet/minecraft/world/entity/LivingEntity;FFLcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/renderer/MultiBufferSource;I)V",
      at = @At("TAIL"))
  private void onRenderEnd(
      LivingEntity entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight,
      CallbackInfo ci) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      EasyNPCLivingEntityRenderer.handleRenderStart(easyNPC, poseStack, bufferSource, packedLight);
    }
  }
}
