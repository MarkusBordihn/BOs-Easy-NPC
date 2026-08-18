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

package de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor;

import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.MoveActionData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.MoveToPositionGoal;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.ai.goal.WrappedGoal;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class MoveActionExecutor {

  public static final int GOAL_PRIORITY = 1;

  private static final Logger log = LogManager.getLogger(MoveActionExecutor.class);

  private MoveActionExecutor() {}

  public static boolean move(
      ActionDataEntry actionDataEntry,
      EasyNPC<?> easyNPC,
      ActionContext actionContext,
      Runnable onArrival) {
    if (actionDataEntry == null || easyNPC == null) {
      return false;
    }

    Level level = easyNPC.getEntityLevel();
    Mob mob = easyNPC.getMob();
    GoalSelector goalSelector = easyNPC.getEntityGoalSelector();
    if (mob == null || goalSelector == null || level == null || level.isClientSide()) {
      return false;
    }

    NavigationDataCapable<?> navigationData = easyNPC.getEasyNPCNavigationData();
    if (navigationData == null || navigationData.isImmovable()) {
      log.debug("Skipping move action for immovable {}", easyNPC);
      return false;
    }

    BlockPos targetPosition = resolveTargetPosition(actionDataEntry, easyNPC, actionContext);
    if (targetPosition == null) {
      log.warn(
          "Skipping move action of {} with unresolvable target {}",
          easyNPC,
          actionDataEntry.moveActionData().targetType());
      return false;
    }

    removeMoveGoals(goalSelector);

    MoveActionData moveActionData = actionDataEntry.moveActionData();
    goalSelector.addGoal(
        GOAL_PRIORITY,
        new MoveToPositionGoal<EasyNPC<?>>(
            easyNPC,
            targetPosition,
            moveActionData.speedModifier(),
            moveActionData.arrivalRadius(),
            moveActionData.timeoutTicks(),
            moveActionData.teleportOnTimeout(),
            onArrival));
    return true;
  }

  private static BlockPos resolveTargetPosition(
      ActionDataEntry actionDataEntry, EasyNPC<?> easyNPC, ActionContext actionContext) {
    BlockPos blockPos = actionDataEntry.blockPos();

    switch (actionDataEntry.moveActionData().targetType()) {
      case POSITION:
        return actionDataEntry.hasBlockPos() ? blockPos : null;
      case RELATIVE:
        return actionDataEntry.hasBlockPos()
            ? easyNPC.getMob().blockPosition().offset(blockPos)
            : null;
      case INITIATOR:
        ServerPlayer initiator = actionContext != null ? actionContext.initiator() : null;
        return initiator != null ? initiator.blockPosition() : null;
      case OWNER:
        OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
        LivingEntity owner = ownerData != null ? ownerData.getOwner() : null;
        return owner != null ? owner.blockPosition() : null;
      case HOME:
        NavigationDataCapable<?> navigationData = easyNPC.getEasyNPCNavigationData();
        return navigationData.hasNPCHomePosition() ? navigationData.getNPCHomePosition() : null;
      default:
        return null;
    }
  }

  private static void removeMoveGoals(GoalSelector goalSelector) {
    List<MoveToPositionGoal<?>> moveGoals = new ArrayList<>();
    for (WrappedGoal wrappedGoal : goalSelector.getAvailableGoals()) {
      if (wrappedGoal.getGoal() instanceof MoveToPositionGoal<?> moveToPositionGoal) {
        moveGoals.add(moveToPositionGoal);
      }
    }
    moveGoals.forEach(goalSelector::removeGoal);
  }
}
