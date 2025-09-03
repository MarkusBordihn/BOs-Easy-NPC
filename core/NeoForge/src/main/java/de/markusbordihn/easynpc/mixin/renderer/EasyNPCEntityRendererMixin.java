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
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import java.util.Optional;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.culling.Frustum;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Holder.Reference;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(EntityRenderer.class)
public class EasyNPCEntityRendererMixin<T extends Entity, S extends EntityRenderState> {

  @Unique
  private static final ResourceLocation NPC_WAND_ID =
      ResourceLocation.fromNamespaceAndPath("easy_npc", "easy_npc_wand");

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
      if (player == null) return;

      // Check if player is holding NPC Wand - if so, always render EasyNPCs (clientside override)
      boolean holdingNPCWand = isPlayerHoldingNPCWand(player);
      if (holdingNPCWand) {
        // Check if entity is within wand range
        double distanceSquared = entity.distanceToSqr(player);
        double wandRange = 32.0d; // HIGHLIGHT_RADIUS from EasyNPCWandItem
        if (distanceSquared <= wandRange * wandRange) {
          // Force rendering for NPCs within wand range - this overrides all invisibility settings
          cir.setReturnValue(true);
          return;
        }
      }

      // Check basic invisibility (normal behavior when not holding wand)
      boolean isInvisible = entity.isInvisible() || entity.isInvisibleTo(player);
      if (isInvisible) {
        // Hide invisible entity
        cir.setReturnValue(false);
      }
    }
  }

  private boolean isPlayerHoldingNPCWand(Player player) {
    // Use registry-based lookup that works across all mod loaders
    Optional<Reference<Item>> npcWandItemHolder = BuiltInRegistries.ITEM.get(NPC_WAND_ID);

    // If the item is not found, we cannot be holding it.
    if (npcWandItemHolder.isEmpty()) {
      return false;
    }

    // Check main hand
    Item npcWandItem = npcWandItemHolder.get().value();
    ItemStack mainHandItem = player.getMainHandItem();
    if (mainHandItem.getItem() == npcWandItem) {
      return true;
    }

    // Check offhand
    ItemStack offHandItem = player.getOffhandItem();
    return offHandItem.getItem() == npcWandItem;
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
      S renderState,
      Component component,
      PoseStack poseStack,
      MultiBufferSource multiBufferSource,
      int i,
      CallbackInfo ci) {
    if (renderState instanceof EasyNPCRenderStateExtension renderStateExtension
        && !EasyNPCModel.renderEntityNameTag(renderStateExtension, poseStack)) {
      ci.cancel();
    }
  }
}
