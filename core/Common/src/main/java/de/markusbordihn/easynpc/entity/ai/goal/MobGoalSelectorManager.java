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

package de.markusbordihn.easynpc.entity.ai.goal;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.ai.goal.target.NearestAttackableTargetGoal;

public class MobGoalSelectorManager {

  private MobGoalSelectorManager() {}

  public static void addMobGoals(
      Mob mob,
      GoalSelector goalSelector,
      GoalSelector targetSelector,
      EntityType<?> entityType,
      ServerLevel serverLevel) {
    if (mob == null || entityType == null || serverLevel == null || mob instanceof EasyNPC<?>) {
      return;
    }

    // Add default goals for all mobs.
    if (goalSelector != null) {
      // Empty for now, but can be used to add custom goals in the future.
    }

    // Add default target goals for all mobs.
    if (targetSelector != null) {
      addTargetingGoals(mob, targetSelector);
    }
  }

  private static void addTargetingGoals(Mob mob, GoalSelector targetSelector) {
    // Define NPCs as potential targets for hostile mobs.
    if (!mob.getType().getCategory().isFriendly()) {
      targetSelector.addGoal(
          4,
          new NearestAttackableTargetGoal<>(
              mob, LivingEntity.class, 10, true, false, entity -> entity instanceof EasyNPC<?>));
    }
  }
}
