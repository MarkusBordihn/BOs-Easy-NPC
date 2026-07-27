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

package de.markusbordihn.easynpc.entity.easynpc.ai.goal;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.goal.TemptGoal;
import net.minecraft.world.item.crafting.Ingredient;

public class CustomTemptGoal extends TemptGoal {

  private final OwnerDataCapable<?> ownerData;
  private final boolean onlyWithoutOwner;

  public CustomTemptGoal(
      EasyNPC<?> easyNPC,
      PathfinderMob pathfinderMob,
      double speedModifier,
      Ingredient items,
      boolean canScare,
      boolean onlyWithoutOwner) {
    super(pathfinderMob, speedModifier, items, canScare);
    this.ownerData = easyNPC.getEasyNPCOwnerData();
    this.onlyWithoutOwner = onlyWithoutOwner;
  }

  private boolean isBlockedByOwner() {
    return this.onlyWithoutOwner && this.ownerData != null && this.ownerData.hasNPCOwner();
  }

  @Override
  public boolean canUse() {
    return !this.isBlockedByOwner() && super.canUse();
  }

  @Override
  public boolean canContinueToUse() {
    return !this.isBlockedByOwner() && super.canContinueToUse();
  }
}
