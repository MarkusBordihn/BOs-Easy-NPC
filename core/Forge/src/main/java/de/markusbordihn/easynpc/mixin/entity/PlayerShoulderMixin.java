/*
 * Copyright 2026 Markus Bordihn
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

import de.markusbordihn.easynpc.access.ShoulderAccessHelper;
import de.markusbordihn.easynpc.api.handler.EasyNPCShoulderHandler;
import de.markusbordihn.easynpc.data.npc.ShoulderSide;
import java.util.Optional;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(Player.class)
public abstract class PlayerShoulderMixin implements ShoulderAccessHelper {

  // Mirrors the delay check in vanilla Player.removeEntitiesOnShoulder.
  @Unique private static final long SHOULDER_RELEASE_DELAY_TICKS = 20L;

  @Unique private Optional<CompoundTag> keptShoulderNPCDataLeft = Optional.empty();
  @Unique private Optional<CompoundTag> keptShoulderNPCDataRight = Optional.empty();

  @Shadow private long timeEntitySatOnShoulder;

  @Shadow
  protected abstract void setShoulderEntityLeft(CompoundTag entityTag);

  @Shadow
  protected abstract void setShoulderEntityRight(CompoundTag entityTag);

  @Override
  public void clearShoulderEntity(ShoulderSide shoulderSide) {
    if (shoulderSide == ShoulderSide.LEFT) {
      this.setShoulderEntityLeft(new CompoundTag());
    } else {
      this.setShoulderEntityRight(new CompoundTag());
    }
  }

  @Inject(method = "removeEntitiesOnShoulder", at = @At("HEAD"))
  private void onRemoveEntitiesOnShoulder(CallbackInfo callbackInfo) {
    Player player = (Player) (Object) this;
    if (player.level().isClientSide()
        || this.timeEntitySatOnShoulder + SHOULDER_RELEASE_DELAY_TICKS
            >= player.level().getGameTime()) {
      return;
    }

    this.keptShoulderNPCDataLeft =
        EasyNPCShoulderHandler.dismountOrTakeShoulderNPCData(player, ShoulderSide.LEFT);
    this.keptShoulderNPCDataRight =
        EasyNPCShoulderHandler.dismountOrTakeShoulderNPCData(player, ShoulderSide.RIGHT);
  }

  @Inject(method = "removeEntitiesOnShoulder", at = @At("RETURN"))
  private void afterRemoveEntitiesOnShoulder(CallbackInfo callbackInfo) {
    this.keptShoulderNPCDataLeft.ifPresent(this::setShoulderEntityLeft);
    this.keptShoulderNPCDataRight.ifPresent(this::setShoulderEntityRight);
    this.keptShoulderNPCDataLeft = Optional.empty();
    this.keptShoulderNPCDataRight = Optional.empty();
  }
}
