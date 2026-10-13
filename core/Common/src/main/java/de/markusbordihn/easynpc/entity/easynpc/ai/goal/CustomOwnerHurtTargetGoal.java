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
import java.util.EnumSet;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.target.TargetGoal;
import net.minecraft.world.entity.ai.targeting.TargetingConditions;

public class CustomOwnerHurtTargetGoal<T extends EasyNPC<?>> extends TargetGoal {

  private final OwnerDataCapable<?> ownerData;
  private LivingEntity ownerLastHurt;
  private int timestamp;

  public CustomOwnerHurtTargetGoal(T easyNPC) {
    super(easyNPC.getMob(), false);
    this.ownerData = easyNPC.getEasyNPCOwnerData();
    this.setFlags(EnumSet.of(Goal.Flag.TARGET));
  }

  @Override
  public boolean canUse() {
    if (this.ownerData == null || !this.ownerData.hasNPCOwner()) {
      return false;
    }

    LivingEntity owner = this.ownerData.getOwner();
    if (owner == null) {
      return false;
    }

    this.ownerLastHurt = owner.getLastHurtMob();
    return owner.getLastHurtMobTimestamp() != this.timestamp
        && this.canAttack(this.ownerLastHurt, TargetingConditions.DEFAULT)
        && this.ownerLastHurt != owner
        && !OwnerAllies.isAlly(this.mob, owner, this.ownerLastHurt);
  }

  @Override
  public void start() {
    this.mob.setTarget(this.ownerLastHurt);
    LivingEntity owner = this.ownerData.getOwner();
    if (owner != null) {
      this.timestamp = owner.getLastHurtMobTimestamp();
    }

    super.start();
  }
}
