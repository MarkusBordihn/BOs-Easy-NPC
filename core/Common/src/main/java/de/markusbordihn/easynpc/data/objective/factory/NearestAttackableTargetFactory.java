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

package de.markusbordihn.easynpc.data.objective.factory;

import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveGoalFactory;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.function.Predicate;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.target.NearestAttackableTargetGoal;

public record NearestAttackableTargetFactory(
    Class<? extends LivingEntity> targetEntityClass, TargetPredicateFactory predicateFactory)
    implements ObjectiveGoalFactory {

  public NearestAttackableTargetFactory(Class<? extends LivingEntity> targetEntityClass) {
    this(targetEntityClass, null);
  }

  private static <T extends LivingEntity> Goal createTargetGoal(
      Mob mob, Class<T> targetEntityClass, boolean mustSeeTarget) {
    return new NearestAttackableTargetGoal<>(mob, targetEntityClass, mustSeeTarget);
  }

  private static <T extends LivingEntity> Goal createTargetGoal(
      Mob mob,
      Class<T> targetEntityClass,
      ObjectiveDataEntry objectiveDataEntry,
      Predicate<LivingEntity> targetPredicate) {
    return new NearestAttackableTargetGoal<>(
        mob,
        targetEntityClass,
        objectiveDataEntry.getInterval(),
        objectiveDataEntry.isMustSeeTarget(),
        objectiveDataEntry.isMustReachTarget(),
        targetPredicate);
  }

  @Override
  public Goal createTarget(ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC) {
    Mob mob = easyNPC.getMob();
    if (this.predicateFactory == null) {
      return createTargetGoal(mob, this.targetEntityClass, objectiveDataEntry.isMustSeeTarget());
    }

    Predicate<LivingEntity> targetPredicate =
        this.predicateFactory.create(objectiveDataEntry, easyNPC);
    if (targetPredicate == null) {
      return null;
    }

    return createTargetGoal(mob, this.targetEntityClass, objectiveDataEntry, targetPredicate);
  }

  @FunctionalInterface
  public interface TargetPredicateFactory {
    Predicate<LivingEntity> create(ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC);
  }
}
