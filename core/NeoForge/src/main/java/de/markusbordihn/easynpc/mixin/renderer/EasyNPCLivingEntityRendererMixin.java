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
import de.markusbordihn.easynpc.data.model.ModelArmPose;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.CrossbowItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.ItemUseAnimation;
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

      ItemStack mainHandItem = livingEntity.getMainHandItem();
      ItemStack offHandItem = livingEntity.getOffhandItem();
      boolean isRightHanded = livingEntity.getMainArm() == HumanoidArm.RIGHT;
      renderStateExtension.setEasyNpcLeftArmPose(ModelArmPose.DEFAULT);
      renderStateExtension.setEasyNpcRightArmPose(ModelArmPose.DEFAULT);

      // Check if using item
      if (livingEntity.isUsingItem()) {
        ItemStack useItem = livingEntity.getUseItem();
        InteractionHand usedHand = livingEntity.getUsedItemHand();
        boolean isUsingMainHand = usedHand == InteractionHand.MAIN_HAND;
        boolean isUsingRightHand =
            (isRightHanded && isUsingMainHand) || (!isRightHanded && !isUsingMainHand);

        if (!useItem.isEmpty()) {
          ItemUseAnimation itemUseAnimation = useItem.getUseAnimation();

          if (itemUseAnimation == ItemUseAnimation.BLOCK) {
            if (isUsingRightHand) {
              renderStateExtension.setEasyNpcRightArmPose(ModelArmPose.DEFAULT);
            } else {
              renderStateExtension.setEasyNpcLeftArmPose(ModelArmPose.DEFAULT);
            }
          } else if (itemUseAnimation == ItemUseAnimation.BOW) {
            if (isUsingRightHand) {
              renderStateExtension.setEasyNpcRightArmPose(ModelArmPose.BOW_AND_ARROW);
            } else {
              renderStateExtension.setEasyNpcLeftArmPose(ModelArmPose.BOW_AND_ARROW);
            }
          } else if (itemUseAnimation == ItemUseAnimation.CROSSBOW) {
            if (isUsingRightHand) {
              renderStateExtension.setEasyNpcRightArmPose(ModelArmPose.CROSSBOW_CHARGE);
            } else {
              renderStateExtension.setEasyNpcLeftArmPose(ModelArmPose.CROSSBOW_CHARGE);
            }
          }
        }
      } else {
        boolean isAggressive =
            (easyNPC.getPathfinderMob().getTarget() != null)
                || (livingEntity instanceof Mob mob && mob.isAggressive());

        // Only show crossbow hold pose if NPC is aggressive or has a target
        if (!mainHandItem.isEmpty()
            && mainHandItem.getItem() instanceof CrossbowItem
            && isAggressive) {
          if (isRightHanded) {
            renderStateExtension.setEasyNpcRightArmPose(ModelArmPose.CROSSBOW_HOLD);
          } else {
            renderStateExtension.setEasyNpcLeftArmPose(ModelArmPose.CROSSBOW_HOLD);
          }
        }

        // Check for crossbow in off-hand - only show CROSSBOW_HOLD if NPC is aggressive
        if (!offHandItem.isEmpty()
            && offHandItem.getItem() instanceof CrossbowItem
            && isAggressive) {
          if (!isRightHanded) {
            renderStateExtension.setEasyNpcRightArmPose(ModelArmPose.CROSSBOW_HOLD);
          } else {
            renderStateExtension.setEasyNpcLeftArmPose(ModelArmPose.CROSSBOW_HOLD);
          }
        }
      }
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
