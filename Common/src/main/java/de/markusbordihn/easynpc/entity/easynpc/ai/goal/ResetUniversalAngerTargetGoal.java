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

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.List;
import net.minecraft.world.entity.EntitySelector;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.NeutralMob;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.level.GameRules;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.AABB;

public class ResetUniversalAngerTargetGoal<T extends EasyNPC<?>> extends Goal {

  private final Mob mob;
  private final Level level;
  private final boolean alertOthersOfSameType;
  private int lastHurtByPlayerTimestamp;

  public ResetUniversalAngerTargetGoal(T easyNPC, boolean alertOthersOfSameType) {
    this.mob = easyNPC.getMob();
    this.level = easyNPC.getLevel();
    this.alertOthersOfSameType = alertOthersOfSameType;
  }

  @Override
  public boolean canUse() {
    return this.level.getGameRules().getBoolean(GameRules.RULE_UNIVERSAL_ANGER)
        && this.wasHurtByPlayer();
  }

  private boolean wasHurtByPlayer() {
    return this.mob.getLastHurtByMob() != null
        && this.mob.getLastHurtByMob().getType() == EntityType.PLAYER
        && this.mob.getLastHurtByMobTimestamp() > this.lastHurtByPlayerTimestamp;
  }

  @Override
  public void start() {
    this.lastHurtByPlayerTimestamp = this.mob.getLastHurtByMobTimestamp();
    ((NeutralMob) this.mob).forgetCurrentTargetAndRefreshUniversalAnger();
    if (this.alertOthersOfSameType) {
      this.getNearbyMobsOfSameType().stream()
          .filter(nearMob -> nearMob != this.mob)
          .map(NeutralMob.class::cast)
          .forEach(NeutralMob::forgetCurrentTargetAndRefreshUniversalAnger);
    }

    super.start();
  }

  private List<? extends Mob> getNearbyMobsOfSameType() {
    double followRange = this.mob.getAttributeValue(Attributes.FOLLOW_RANGE);
    AABB nearbyRange =
        AABB.unitCubeFromLowerCorner(this.mob.position()).inflate(followRange, 10.0, followRange);
    return this.mob.level().getEntitiesOfClass(
        this.mob.getClass(), nearbyRange, EntitySelector.NO_SPECTATORS);
  }
}
