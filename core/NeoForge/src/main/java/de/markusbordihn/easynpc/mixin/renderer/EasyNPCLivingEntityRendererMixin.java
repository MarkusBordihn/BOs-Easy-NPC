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

package de.markusbordihn.easynpc.mixin.renderer;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCLivingEntityRenderer;
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

  @Inject(method = "render", at = @At("HEAD"))
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
      EasyNPCLivingEntityRenderer.handleRenderEnd(easyNPC, poseStack, bufferSource, packedLight);
    }
  }

  @Inject(method = "scale", at = @At("HEAD"))
  private void onScale(LivingEntity entity, PoseStack poseStack, float scale, CallbackInfo ci) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      EasyNPCLivingEntityRenderer.handleScale(easyNPC, poseStack);
    }
  }
}
