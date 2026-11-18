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

package de.markusbordihn.easynpc.entity.ai.goal;

import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.target.NearestAttackableTargetGoal;

public class NearestAttackableEasyNPCTargetGoal extends NearestAttackableTargetGoal<LivingEntity> {

  private static final int RANDOM_INTERVAL = 10;

  public NearestAttackableEasyNPCTargetGoal(Mob mob) {
    super(
        mob,
        LivingEntity.class,
        RANDOM_INTERVAL,
        true,
        false,
        (livingEntity, level) -> livingEntity instanceof EasyNPC<?>);
  }

  @Override
  protected void findTarget() {
    if (!LivingEntityManager.hasServerEasyNPCEntities()) {
      this.target = null;
      return;
    }

    this.target =
        getServerLevel(this.mob)
            .getNearestEntity(
                this.mob
                    .level()
                    .getEntitiesOfClass(
                        Mob.class,
                        this.getTargetSearchArea(this.getFollowDistance()),
                        this::isAttackableEasyNPC),
                this.targetConditions.range(this.getFollowDistance()),
                this.mob,
                this.mob.getX(),
                this.mob.getEyeY(),
                this.mob.getZ());
  }

  private boolean isAttackableEasyNPC(Mob candidate) {
    return candidate instanceof EasyNPC<?> && this.mob.canAttack(candidate);
  }
}
