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

package de.markusbordihn.easynpc.entity.easynpc.ai.goal;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import java.util.EnumSet;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.target.TargetGoal;
import net.minecraft.world.entity.ai.targeting.TargetingConditions;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CustomOwnerHurtByTargetGoal<T extends EasyNPC<?>> extends TargetGoal {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private final OwnerDataCapable<?> ownerData;
  private LivingEntity ownerLastHurtBy;
  private int timestamp;

  public CustomOwnerHurtByTargetGoal(T easyNPC) {
    super(easyNPC.getMob(), false);
    this.ownerData = easyNPC.getEasyNPCOwnerData();
    this.setFlags(EnumSet.of(Goal.Flag.TARGET));
  }

  @Override
  public boolean canUse() {
    // Verify owner exists
    if (this.ownerData == null || !this.ownerData.hasNPCOwner()) {
      return false;
    }

    LivingEntity owner = this.ownerData.getOwner();
    if (owner == null) {
      return false;
    }

    // Get entity that hurt the owner
    this.ownerLastHurtBy = owner.getLastHurtByMob();
    int lastHurtByTimestamp = owner.getLastHurtByMobTimestamp();

    return lastHurtByTimestamp != this.timestamp
        && this.canAttack(this.ownerLastHurtBy, TargetingConditions.DEFAULT)
        && this.isRevengeTargetValid(this.ownerLastHurtBy);
  }

  @Override
  public void start() {
    this.mob.setTarget(this.ownerLastHurtBy);
    LivingEntity owner = this.ownerData.getOwner();
    if (owner != null) {
      this.timestamp = owner.getLastHurtByMobTimestamp();
    }

    super.start();
  }

  private boolean isRevengeTargetValid(LivingEntity target) {
    if (target == null) {
      return false;
    }

    // Don't attack the owner
    if (target == this.ownerData.getOwner()) {
      return false;
    }

    // Standard validity checks
    return target.isAlive() && !target.isInvulnerable() && target != this.mob;
  }
}
