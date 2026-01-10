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
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.VisibilityHandler;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.client.renderer.state.CameraRenderState;
import net.minecraft.world.entity.LivingEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(LivingEntityRenderer.class)
public class EasyNPCLivingEntityRendererMixin {

  @Inject(
      method =
          "extractRenderState(Lnet/minecraft/world/entity/LivingEntity;Lnet/minecraft/client/renderer/entity/state/LivingEntityRenderState;F)V",
      at = @At("TAIL"))
  private void injectEasyNpcUUID(
      LivingEntity livingEntity,
      LivingEntityRenderState renderState,
      float partialTicks,
      CallbackInfo ci) {
    if (livingEntity instanceof EasyNPC<?> easyNPC
        && renderState instanceof EasyNPCRenderStateExtension renderStateExtension) {
      renderStateExtension.setEasyNpcUUID(easyNPC.getEntityUUID());

      // Extract and cache the texture in the render state
      EasyNPCLivingEntityRenderer.extractAndCacheTexture(
          easyNPC,
          renderStateExtension,
          renderState,
          (LivingEntityRenderer<?, ?, ?>) (Object) this);
    }
  }

  @Inject(
      method =
          "submit(Lnet/minecraft/client/renderer/entity/state/LivingEntityRenderState;Lcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/renderer/SubmitNodeCollector;Lnet/minecraft/client/renderer/state/CameraRenderState;)V",
      at = @At("HEAD"))
  private void onRenderStart(
      LivingEntityRenderState renderState,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState,
      CallbackInfo ci) {
    if (renderState instanceof EasyNPCRenderStateExtension) {
      EasyNPCLivingEntityRenderer.handleRenderStart(renderState, poseStack, null, 0);
    }
  }

  @Inject(
      method =
          "submit(Lnet/minecraft/client/renderer/entity/state/LivingEntityRenderState;Lcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/renderer/SubmitNodeCollector;Lnet/minecraft/client/renderer/state/CameraRenderState;)V",
      at = @At("TAIL"))
  private void onRenderEnd(
      LivingEntityRenderState renderState,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState,
      CallbackInfo ci) {
    if (renderState instanceof EasyNPCRenderStateExtension) {
      EasyNPCLivingEntityRenderer.handleRenderEnd(renderState, poseStack, null, 0);
    }
  }

  @Inject(method = "scale", at = @At("HEAD"))
  private void onScale(LivingEntityRenderState renderState, PoseStack poseStack, CallbackInfo ci) {
    if (renderState instanceof EasyNPCRenderStateExtension) {
      EasyNPCLivingEntityRenderer.handleScale(renderState, poseStack);
    }
  }

  @Inject(
      method = "shouldShowName(Lnet/minecraft/world/entity/LivingEntity;D)Z",
      at = @At("HEAD"),
      cancellable = true)
  private void onShouldShowName(
      LivingEntity entity, double distance, CallbackInfoReturnable<Boolean> cir) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      var player = Minecraft.getInstance().player;
      if (player != null) {
        boolean shouldShowName =
            VisibilityHandler.handleIsCustomNameVisibleToPlayer(
                easyNPC, player, entity.isCustomNameVisible(), distance);
        cir.setReturnValue(shouldShowName);
      }
    }
  }
}
