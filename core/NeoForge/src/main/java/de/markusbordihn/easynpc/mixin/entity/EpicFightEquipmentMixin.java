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

package de.markusbordihn.easynpc.mixin.entity;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.ItemStack;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Mob.class)
public abstract class EpicFightEquipmentMixin {

  @Inject(method = "getItemBySlot", at = @At("HEAD"), cancellable = true)
  private void onGetItemBySlot(EquipmentSlot slot, CallbackInfoReturnable<ItemStack> cir) {
    Mob self = (Mob) (Object) this;
    if (!self.level().isClientSide()) {
      return;
    }

    if (!(self instanceof EasyNPC<?> easyNPC)) {
      return;
    }

    if (slot != EquipmentSlot.HEAD
        && slot != EquipmentSlot.CHEST
        && slot != EquipmentSlot.LEGS
        && slot != EquipmentSlot.FEET) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return;
    }

    if (!modelData.getModelPartVisibility(slot)) {
      cir.setReturnValue(ItemStack.EMPTY);
    }
  }
}
