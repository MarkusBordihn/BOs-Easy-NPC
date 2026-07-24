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
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.handler.FactionDisputeNotifier;
import de.markusbordihn.easynpc.handler.FactionHandler;
import java.util.EnumSet;
import java.util.Objects;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.target.TargetGoal;
import net.minecraft.world.entity.ai.targeting.TargetingConditions;
import net.minecraft.world.phys.AABB;

public class CustomFactionHurtByTargetGoal<T extends EasyNPC<?>> extends TargetGoal {

  private static final int DEFAULT_SCAN_INTERVAL = 20;
  private static final double VERTICAL_SEARCH_RANGE = 10.0d;

  private final FactionDataCapable<?> factionData;
  private final int scanInterval;
  private LivingEntity factionMemberAttacker;

  public CustomFactionHurtByTargetGoal(T easyNPC, int scanInterval) {
    super(easyNPC.getMob(), false);
    this.factionData = easyNPC.getEasyNPCFactionData();
    this.scanInterval = scanInterval > 0 ? scanInterval : DEFAULT_SCAN_INTERVAL;
    this.setFlags(EnumSet.of(Goal.Flag.TARGET));
  }

  private static boolean belongsToFaction(LivingEntity entity, String factionName) {
    return entity.isAlive()
        && Objects.equals(FactionHandler.getTargetGroupName(entity), factionName);
  }

  @Override
  public boolean canUse() {
    if (this.factionData == null || !this.factionData.hasFactionName()) {
      return false;
    }

    if (this.mob.getRandom().nextInt(this.scanInterval) != 0) {
      return false;
    }

    this.factionMemberAttacker =
        this.findAttackerOfFactionMember(this.factionData.getFactionName());
    return this.factionMemberAttacker != null;
  }

  @Override
  public void start() {
    this.mob.setTarget(this.factionMemberAttacker);
    super.start();
  }

  private LivingEntity findAttackerOfFactionMember(String factionName) {
    double searchRange = this.getFollowDistance();
    AABB searchArea =
        this.mob.getBoundingBox().inflate(searchRange, VERTICAL_SEARCH_RANGE, searchRange);

    for (LivingEntity factionMember :
        this.mob
            .level()
            .getEntitiesOfClass(
                LivingEntity.class,
                searchArea,
                entity -> entity != this.mob && belongsToFaction(entity, factionName))) {

      // The attacker is cleared by the game after 100 ticks, so this is always a recent attack.
      LivingEntity attacker = factionMember.getLastHurtByMob();
      if (attacker == null || attacker == this.mob) {
        continue;
      }

      if (belongsToFaction(attacker, factionName)) {
        FactionDisputeNotifier.notifyInternalDispute(factionMember, attacker, factionName);
        continue;
      }

      if (this.canAttack(attacker, TargetingConditions.DEFAULT)) {
        return attacker;
      }
    }

    return null;
  }
}
