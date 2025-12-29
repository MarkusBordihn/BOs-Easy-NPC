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

package de.markusbordihn.easynpc.entity.easynpc.handlers;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.ActionGroup;
import de.markusbordihn.easynpc.data.action.ActionManager;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionUtils;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.ActionValidator;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.CommandActionExecutor;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.DialogActionExecutor;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.ScoreboardActionExecutor;
import java.util.List;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.Mth;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySelector;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;

public interface ActionHandler<E extends PathfinderMob> extends EasyNPC<E> {

  default List<? extends Player> getPlayersInRange(Double range) {
    Entity entity = this.getEntity();
    return this.getEntityLevel().players().stream()
        .filter(EntitySelector.NO_SPECTATORS)
        .filter(targetPlayers -> entity.closerThan(targetPlayers, range))
        .toList();
  }

  default void checkTradingActions() {
    this.getProfiler().push("npcCheckTradingActions");

    TradingDataCapable<E> tradingData = this.getEasyNPCTradingData();
    TickerDataCapable<E> tickerData = this.getEasyNPCTickerData();
    if (tradingData == null || tickerData == null) {
      return;
    }

    this.getProfiler().pop();
  }

  default void checkDistanceActions() {
    this.getProfiler().push("npcCheckDistanceActions");

    // Validate action data and mob entity.
    Mob mob = this.getMob();
    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData == null || mob == null || mob.isDeadOrDying()) {
      return;
    }

    // Check to avoid additional checks, when no player is in range.
    boolean skipPlayerDistanceCheck = false;

    // Near distance action, if set.
    if (actionEventData.hasActionEvent(ActionEventType.ON_DISTANCE_NEAR)) {
      List<? extends Player> listOfPlayers = this.getPlayersInRange(16.0D);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(mob, ActionGroup.DISTANCE_NEAR);
        skipPlayerDistanceCheck = true;
      } else {
        ActionDataEntry actionDataEntry =
            actionEventData.getActionEvent(ActionEventType.ON_DISTANCE_NEAR);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer
              && !ActionManager.containsPlayer(mob, ActionGroup.DISTANCE_NEAR, serverPlayer)) {
            this.executeAction(actionDataEntry, serverPlayer);
            ActionManager.addPlayer(mob, ActionGroup.DISTANCE_NEAR, serverPlayer);
          }
        }
      }
    }

    // Close distance action, if set.
    if (actionEventData.hasActionEvent(ActionEventType.ON_DISTANCE_CLOSE)) {
      List<? extends Player> listOfPlayers =
          skipPlayerDistanceCheck ? null : this.getPlayersInRange(8.0D);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(mob, ActionGroup.DISTANCE_CLOSE);
        skipPlayerDistanceCheck = true;
      } else {
        ActionDataEntry actionDataEntry =
            actionEventData.getActionEvent(ActionEventType.ON_DISTANCE_CLOSE);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer
              && !ActionManager.containsPlayer(mob, ActionGroup.DISTANCE_CLOSE, serverPlayer)) {
            this.executeAction(actionDataEntry, serverPlayer);
            ActionManager.addPlayer(mob, ActionGroup.DISTANCE_CLOSE, serverPlayer);
          }
        }
      }
    }

    // Very close distance action, if set.
    if (actionEventData.hasActionEvent(ActionEventType.ON_DISTANCE_VERY_CLOSE)) {
      List<? extends Player> listOfPlayers =
          skipPlayerDistanceCheck ? null : this.getPlayersInRange(4.0D);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(mob, ActionGroup.DISTANCE_VERY_CLOSE);
        skipPlayerDistanceCheck = true;
      } else {
        ActionDataEntry actionDataEntry =
            actionEventData.getActionEvent(ActionEventType.ON_DISTANCE_VERY_CLOSE);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer
              && !ActionManager.containsPlayer(
                  mob, ActionGroup.DISTANCE_VERY_CLOSE, serverPlayer)) {
            this.executeAction(actionDataEntry, serverPlayer);
            ActionManager.addPlayer(mob, ActionGroup.DISTANCE_VERY_CLOSE, serverPlayer);
          }
        }
      }
    }

    // Touch distance action, if set.
    if (actionEventData.hasActionEvent(ActionEventType.ON_DISTANCE_TOUCH)) {
      List<? extends Player> listOfPlayers =
          skipPlayerDistanceCheck ? null : this.getPlayersInRange(1.25D);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(mob, ActionGroup.DISTANCE_TOUCH);
      } else {
        ActionDataEntry actionDataEntry =
            actionEventData.getActionEvent(ActionEventType.ON_DISTANCE_TOUCH);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer
              && !ActionManager.containsPlayer(mob, ActionGroup.DISTANCE_TOUCH, serverPlayer)) {
            this.executeAction(actionDataEntry, serverPlayer);
            ActionManager.addPlayer(mob, ActionGroup.DISTANCE_TOUCH, serverPlayer);
          }
        }
      }
    }

    this.getProfiler().pop();
  }

  default void interactWithBlock(BlockPos blockPos) {
    LivingEntity livingEntity = this.getLivingEntity();
    if (livingEntity != null && !this.isClientSideInstance()) {
      this.lookAtBlock(blockPos);
      livingEntity.swing(InteractionHand.MAIN_HAND);
      if (!livingEntity.getMainHandItem().isEmpty()) {
        this.getEntityServerLevel()
            .getBlockState(blockPos)
            .useItemOn(
                livingEntity.getMainHandItem(),
                this.getEntityServerLevel(),
                this.getFakePlayer(this.getEntityServerLevel(), blockPos),
                InteractionHand.MAIN_HAND,
                new BlockHitResult(Vec3.atCenterOf(blockPos), Direction.DOWN, blockPos, false));
      } else {
        this.getEntityServerLevel()
            .getBlockState(blockPos)
            .useWithoutItem(
                this.getEntityServerLevel(),
                this.getFakePlayer(this.getEntityServerLevel(), blockPos),
                new BlockHitResult(Vec3.atCenterOf(blockPos), Direction.DOWN, blockPos, false));
      }
      livingEntity
          .getMainHandItem()
          .use(
              this.getEntityServerLevel(),
              this.getFakePlayer(this.getEntityServerLevel(), blockPos),
              InteractionHand.MAIN_HAND);
    }
  }

  default void lookAtBlock(BlockPos target) {
    Entity entity = this.getEntity();
    Vec3 vec3d = entity.position();
    Vec3 targetVec = Vec3.atCenterOf(target);
    Vec3 delta = targetVec.subtract(vec3d);
    double horizontalDistance = delta.horizontalDistance();
    entity.setXRot(
        Mth.wrapDegrees((float) (-(Mth.atan2(delta.y, horizontalDistance) * (180D / Math.PI)))));
    entity.setYBodyRot(
        Mth.wrapDegrees((float) (Mth.atan2(delta.z, delta.x) * (180D / Math.PI)) - 90.0F));
    entity.setYHeadRot(entity.getYHeadRot());
  }

  default void executeActions(ActionDataSet actionDataSet, ServerPlayer serverPlayer) {
    if (actionDataSet == null || actionDataSet.isEmpty()) {
      return;
    }

    ActionDataEntry closeDialogAction = null;
    boolean hasScreenAction = false;
    for (ActionDataEntry actionDataEntry : actionDataSet.getEntries()) {
      ActionDataType actionType = actionDataEntry.actionDataType();

      // Check for close dialog action and execute it at the end.
      if (actionType == ActionDataType.CLOSE_DIALOG) {
        if (closeDialogAction == null) {
          closeDialogAction = actionDataEntry;
        } else {
          log.warn("Multiple close dialog actions found in action data set {}!", actionDataSet);
        }
        continue;
      }

      // Check for screen actions and execute only the first valid one.
      if (actionType == ActionDataType.OPEN_DEFAULT_DIALOG
          || actionType == ActionDataType.OPEN_NAMED_DIALOG
          || actionType == ActionDataType.OPEN_TRADING_SCREEN) {
        if (hasScreenAction) {
          log.debug(
              "Ignoring {}. Multiple screen actions found in action data set {}! Only the first valid will be executed.",
              actionType,
              actionDataSet);
          continue;
        }

        if ((actionType == ActionDataType.OPEN_DEFAULT_DIALOG
                && !this.getEasyNPCDialogData().hasDialog())
            || (actionType == ActionDataType.OPEN_NAMED_DIALOG
                && actionDataEntry.targetUUID() == null
                && !this.getEasyNPCDialogData().hasDialog(actionDataEntry.command()))
            || (actionType == ActionDataType.OPEN_TRADING_SCREEN
                && !this.getEasyNPCTradingData().hasTradingData())) {
          log.debug(
              "Ignoring {} action because no valid data are available: {}",
              actionType,
              actionDataEntry);
          continue;
        }

        hasScreenAction = true;
      }

      this.executeAction(actionDataEntry, serverPlayer);
    }

    // Execute close dialog action at the end.
    if (closeDialogAction != null) {
      this.executeAction(closeDialogAction, serverPlayer);
    }
  }

  default void executeAction(ActionDataEntry actionDataEntry, ServerPlayer serverPlayer) {
    boolean isValid =
        serverPlayer != null
            ? ActionValidator.validateActionData(actionDataEntry, serverPlayer)
            : ActionValidator.validateActionDataWithoutPlayer(actionDataEntry);
    if (!isValid) {
      return;
    }

    switch (actionDataEntry.actionDataType()) {
      case NONE:
        break;
      case COMMAND:
        if (actionDataEntry.executeAsUser()) {
          if (serverPlayer != null) {
            CommandActionExecutor.executeAsPlayer(
                actionDataEntry,
                serverPlayer,
                this.getLivingEntity(),
                this.getEasyNPCActionEventData());
          } else {
            log.warn(
                "Skipping COMMAND action with executeAsUser=true because no ServerPlayer is available: {}",
                actionDataEntry);
          }
        } else {
          CommandActionExecutor.executeAsEntity(
              actionDataEntry,
              serverPlayer,
              this.getEntity(),
              this.getLivingEntity(),
              this.getEasyNPCActionEventData());
        }
        break;
      case CLOSE_DIALOG:
        if (serverPlayer != null) {
          serverPlayer.closeContainer();
        } else {
          log.warn("Skipping CLOSE_DIALOG action because no ServerPlayer is available");
        }
        break;
      case INTERACT_BLOCK:
        BlockPos blockPos = actionDataEntry.blockPos();
        if (ActionValidator.validateBlockPos(blockPos)) {
          this.interactWithBlock(blockPos);
        } else {
          log.error("No block position found for action {}", actionDataEntry);
        }
        break;
      case OPEN_DEFAULT_DIALOG:
        if (serverPlayer != null) {
          DialogActionExecutor.openDefaultDialog(
              actionDataEntry, serverPlayer, this.getEasyNPCDialogData());
        } else {
          log.warn("Skipping OPEN_DEFAULT_DIALOG action because no ServerPlayer is available");
        }
        break;
      case OPEN_NAMED_DIALOG:
        if (serverPlayer != null) {
          DialogActionExecutor.openNamedDialog(
              actionDataEntry, serverPlayer, this.getEasyNPCDialogData());
        } else {
          log.warn("Skipping OPEN_NAMED_DIALOG action because no ServerPlayer is available");
        }
        break;
      case OPEN_TRADING_SCREEN:
        if (serverPlayer != null) {
          TradingDataCapable<E> tradingData = this.getEasyNPCTradingData();
          if (tradingData != null) {
            tradingData.openTradingScreen(serverPlayer);
          } else {
            log.warn(
                "Cannot execute OPEN_TRADING_SCREEN action for player {}: No trading data found in action {}",
                serverPlayer.getName().getString(),
                actionDataEntry);
          }
        } else {
          log.debug("Skipping OPEN_TRADING_SCREEN action because no ServerPlayer is available");
        }
        break;
      case SCOREBOARD:
        if (serverPlayer != null) {
          ScoreboardActionExecutor.execute(actionDataEntry, serverPlayer);
        } else {
          log.warn("Skipping SCOREBOARD action because no ServerPlayer is available");
        }
        break;
      default:
        log.warn(
            "Unknown action type {} for action {}",
            actionDataEntry.actionDataType(),
            actionDataEntry);
        break;
    }

    if (serverPlayer != null) {
      for (ConditionDataEntry condition : actionDataEntry.conditionDataSet().getConditions()) {
        ConditionUtils.recordActionExecution(condition, serverPlayer, actionDataEntry.getId());
      }
    }
  }
}
