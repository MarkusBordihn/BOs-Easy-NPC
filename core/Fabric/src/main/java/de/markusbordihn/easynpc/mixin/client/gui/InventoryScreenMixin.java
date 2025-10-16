/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.mixin.client.gui;

import de.markusbordihn.easynpc.client.gui.InventoryScreenHandler;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.InventoryScreen;
import net.minecraft.world.entity.LivingEntity;
import org.joml.Quaternionf;
import org.joml.Vector3f;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(InventoryScreen.class)
public class InventoryScreenMixin {

  @Inject(
      method =
          "renderEntityInInventory(Lnet/minecraft/client/gui/GuiGraphics;IIIIFLorg/joml/Vector3f;Lorg/joml/Quaternionf;Lorg/joml/Quaternionf;Lnet/minecraft/world/entity/LivingEntity;)V",
      at = @At("HEAD"),
      cancellable = true)
  private static void onRenderEntityInInventory(
      GuiGraphics guiGraphics,
      int left,
      int top,
      int right,
      int bottom,
      float scale,
      Vector3f translation,
      Quaternionf rotation,
      Quaternionf entityRotation,
      LivingEntity entity,
      CallbackInfo ci) {
    if (entity instanceof EasyNPC<?> easyNPC
        && InventoryScreenHandler.onRenderEntityInInventory(
            guiGraphics,
            left,
            top,
            right,
            bottom,
            scale,
            translation,
            rotation,
            entityRotation,
            entity,
            easyNPC)) {
      ci.cancel();
    }
  }
}
