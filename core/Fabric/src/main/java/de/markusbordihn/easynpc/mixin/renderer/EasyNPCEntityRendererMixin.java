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
import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.utils.ItemUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.culling.Frustum;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(EntityRenderer.class)
public class EasyNPCEntityRendererMixin<T extends Entity> {

  private static final double NPC_WAND_RENDER_RANGE = 32.0d;

  @Inject(method = "shouldRender", at = @At("HEAD"), cancellable = true)
  private void onShouldRender(
      T entity,
      Frustum frustum,
      double x,
      double y,
      double z,
      CallbackInfoReturnable<Boolean> cir) {
    if (entity instanceof EasyNPC<?>) {
      var player = Minecraft.getInstance().player;
      if (player == null) {
        return;
      }

      if (ItemUtils.isPlayerHoldingEasyNPCWand(player)) {
        double distanceSquared = entity.distanceToSqr(player);
        if (distanceSquared <= NPC_WAND_RENDER_RANGE * NPC_WAND_RENDER_RANGE) {
          cir.setReturnValue(true);
          return;
        }
      }

      boolean isInvisible = entity.isInvisible() || entity.isInvisibleTo(player);
      if (isInvisible) {
        cir.setReturnValue(false);
      }
    }
  }

  @Inject(method = "getBlockLightLevel", at = @At("HEAD"), cancellable = true)
  private void onGetBlockLightLevel(
      T entity, BlockPos blockPos, CallbackInfoReturnable<Integer> cir) {
    if (entity instanceof EasyNPC<?> easyNPC
        && easyNPC.getEasyNPCDisplayAttributeData() instanceof DisplayAttributeDataCapable<?>) {
      cir.setReturnValue(
          EasyNPCModel.getEntityLightLevel(
              easyNPC, easyNPC.getEasyNPCDisplayAttributeData(), blockPos));
    }
  }

  @Inject(method = "renderNameTag", at = @At("HEAD"), cancellable = true)
  private void onRenderNameTag(
      T entity,
      Component component,
      PoseStack poseStack,
      MultiBufferSource multiBufferSource,
      int i,
      float f,
      CallbackInfo ci) {
    if (entity instanceof EasyNPC<?> easyNPC
        && easyNPC.getEasyNPCModelData() instanceof ModelDataCapable) {
      EasyNPCModel.renderEntityNameTag(easyNPC, easyNPC.getEasyNPCModelData(), poseStack);
    }
  }
}
