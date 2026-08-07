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

import de.markusbordihn.easynpc.api.event.EasyNPCEventRegistry;
import de.markusbordihn.easynpc.condition.ConditionManager;
import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.ActionGroup;
import de.markusbordihn.easynpc.data.action.ActionManager;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.execution.ExecutionId;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.ActionValidator;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.CommandActionExecutor;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.CustomActionDispatcher;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.DialogActionExecutor;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.MessageActionExecutor;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.ModelAnimationActionExecutor;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.PoseActionExecutor;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.ScoreboardActionExecutor;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.StateActionExecutor;
import de.markusbordihn.easynpc.handler.EnvironmentChangeTracker;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.Mth;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySelector;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;

public interface ActionHandler<E extends Mob> extends EasyNPC<E> {
  double INTERVAL_ACTION_RANGE = 16.0D;

  // Widest range first, so an empty range can skip the narrower ones inside it.
  List<ActionEventType> DISTANCE_ACTION_EVENT_TYPES =
      Arrays.stream(ActionEventType.values())
          .filter(ActionEventType::isDistanceEvent)
          .sorted(Comparator.comparingDouble(ActionEventType::getTriggerDistance).reversed())
          .toList();

  List<ActionEventType> INTERVAL_ACTION_EVENT_TYPES =
      Arrays.stream(ActionEventType.values())
          .filter(ActionEventType::isIntervalEvent)
          .sorted(Comparator.comparingInt(ActionEventType::getIntervalSeconds))
          .toList();

  Map<ActionEventType, TickerType> INTERVAL_ACTION_TICKERS =
      Map.of(
          ActionEventType.ON_INTERVAL_INSTANT, TickerType.INTERVAL_ACTION_INSTANT,
          ActionEventType.ON_INTERVAL_SHORT, TickerType.INTERVAL_ACTION_SHORT,
          ActionEventType.ON_INTERVAL_NORMAL, TickerType.INTERVAL_ACTION_NORMAL,
          ActionEventType.ON_INTERVAL_LONG, TickerType.INTERVAL_ACTION_LONG,
          ActionEventType.ON_INTERVAL_VERY_LONG, TickerType.INTERVAL_ACTION_VERY_LONG);

  private static boolean hasFallbackCondition(ActionDataEntry actionDataEntry) {
    return actionDataEntry.conditionDataSet() != null
        && actionDataEntry.conditionDataSet().getConditions().stream()
            .anyMatch(condition -> condition.conditionType() == ConditionType.FALLBACK);
  }

  private static int getIntervalThreshold(ActionEventType actionEventType) {
    // The base tick body runs every BASE_TICK + 1 ticks and checkAndIncreaseTicker needs one
    // additional call after the threshold is reached.
    float baseTickPeriod = BaseTickHandler.BASE_TICK + 1.0F;
    return Math.max(
        0, Math.round(actionEventType.getIntervalSeconds() * 20.0F / baseTickPeriod) - 1);
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

    Mob mob = this.getMob();
    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData == null || mob == null || mob.isDeadOrDying()) {
      return;
    }

    double emptyBelowDistance = 0.0D;

    for (ActionEventType actionEventType : DISTANCE_ACTION_EVENT_TYPES) {
      if (!actionEventData.hasActionEvent(actionEventType)) {
        continue;
      }

      ActionGroup actionGroup = actionEventType.getActionGroup();
      double triggerDistance = actionEventType.getTriggerDistance();
      List<ServerPlayer> listOfPlayers =
          triggerDistance <= emptyBelowDistance
              ? null
              : this.getServerPlayersInRange(triggerDistance);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(mob, actionGroup);
        emptyBelowDistance = triggerDistance;
        continue;
      }

      ActionDataSet actionDataSet = actionEventData.getActionDataSet(actionEventType);
      ActionContext actionContext = ActionContext.of(actionEventType, null, listOfPlayers);
      for (ServerPlayer serverPlayer : listOfPlayers) {
        if (!ActionManager.containsPlayer(mob, actionGroup, serverPlayer)) {
          this.executeActions(actionDataSet, actionContext.withInitiator(serverPlayer));
          ActionManager.addPlayer(mob, actionGroup, serverPlayer);
        }
      }
    }

    this.getProfiler().pop();
  }

  default ServerPlayer getNearestServerPlayerInRange(double range) {
    Entity entity = this.getEntity();
    return this.getEntityLevel().players().stream()
        .filter(EntitySelector.NO_SPECTATORS)
        .filter(ServerPlayer.class::isInstance)
        .map(ServerPlayer.class::cast)
        .filter(player -> entity.closerThan(player, range))
        .min(Comparator.comparingDouble(entity::distanceToSqr))
        .orElse(null);
  }

  default List<ServerPlayer> getServerPlayersInRange(double range) {
    Entity entity = this.getEntity();
    return this.getEntityLevel().players().stream()
        .filter(EntitySelector.NO_SPECTATORS)
        .filter(ServerPlayer.class::isInstance)
        .map(ServerPlayer.class::cast)
        .filter(player -> entity.closerThan(player, range))
        .sorted(Comparator.comparingDouble(entity::distanceToSqr))
        .toList();
  }

  default ServerPlayer getPreferredServerPlayer(List<ServerPlayer> candidates) {
    if (candidates == null || candidates.isEmpty()) {
      return null;
    }

    OwnerDataCapable<E> ownerData = this.getEasyNPCOwnerData();
    if (ownerData != null && ownerData.hasNPCOwner()) {
      for (ServerPlayer candidate : candidates) {
        if (ownerData.isNPCOwner(candidate)) {
          return candidate;
        }
      }
    }

    return candidates.get(0);
  }

  default ActionContext buildActionContext(ActionEventType actionEventType, double range) {
    List<ServerPlayer> audience = this.getServerPlayersInRange(range);
    return ActionContext.of(actionEventType, this.getPreferredServerPlayer(audience), audience);
  }

  default void checkIntervalActions() {
    this.getProfiler().push("npcCheckIntervalActions");

    Mob mob = this.getMob();
    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    TickerDataCapable<E> tickerData = this.getEasyNPCTickerData();
    if (actionEventData != null && tickerData != null && mob != null && !mob.isDeadOrDying()) {
      List<ServerPlayer> audience = null;

      for (ActionEventType actionEventType : INTERVAL_ACTION_EVENT_TYPES) {
        if (!actionEventData.hasActionEvent(actionEventType)) {
          continue;
        }

        // Pause the interval while no player is in range.
        if (audience == null) {
          audience = this.getServerPlayersInRange(INTERVAL_ACTION_RANGE);
          if (audience.isEmpty()) {
            break;
          }
        }

        TickerType tickerType = INTERVAL_ACTION_TICKERS.get(actionEventType);
        int threshold = getIntervalThreshold(actionEventType);
        if (!tickerData.checkAndIncreaseTicker(tickerType, threshold)) {
          continue;
        }

        this.executeRandomAction(
            actionEventData.getActionDataSet(actionEventType),
            ActionContext.of(actionEventType, this.getPreferredServerPlayer(audience), audience));

        int jitter = threshold / 10;
        tickerData.setTicker(
            tickerType, jitter > 0 ? mob.getRandom().nextInt(2 * jitter + 1) - jitter : 0);
      }
    }

    this.getProfiler().pop();
  }

  default void checkEnvironmentActions() {
    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData == null) {
      return;
    }

    if (actionEventData.hasActionEvent(ActionEventType.ON_TIME_CHANGE)
        && EnvironmentChangeTracker.hasDayTimeChanged(this.getEntityLevel())) {
      actionEventData.handleActionEvent(
          ActionEventType.ON_TIME_CHANGE,
          this.buildActionContext(ActionEventType.ON_TIME_CHANGE, INTERVAL_ACTION_RANGE));
    }

    if (actionEventData.hasActionEvent(ActionEventType.ON_WEATHER_CHANGE)
        && EnvironmentChangeTracker.hasWeatherChanged(this.getEntityLevel())) {
      actionEventData.handleActionEvent(
          ActionEventType.ON_WEATHER_CHANGE,
          this.buildActionContext(ActionEventType.ON_WEATHER_CHANGE, INTERVAL_ACTION_RANGE));
    }
  }

  default void checkSpawnAction() {
    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    StatusDataCapable<E> statusData = this.getEasyNPCStatusData();
    if (actionEventData == null
        || statusData == null
        || !actionEventData.hasActionEvent(ActionEventType.ON_SPAWN)
        || statusData.getStatusDataFlag(StatusDataType.SPAWN_ACTION_FIRED)) {
      return;
    }

    // Wait for a player, so a spawn message of an NPC created far away is not lost.
    List<ServerPlayer> audience = this.getServerPlayersInRange(INTERVAL_ACTION_RANGE);
    if (audience.isEmpty()) {
      return;
    }

    statusData.setStatusDataFlag(StatusDataType.SPAWN_ACTION_FIRED, true);
    actionEventData.handleActionEvent(
        ActionEventType.ON_SPAWN,
        ActionContext.of(
            ActionEventType.ON_SPAWN, this.getPreferredServerPlayer(audience), audience));
  }

  default void executeRandomAction(ActionDataSet actionDataSet, ServerPlayer serverPlayer) {
    this.executeRandomAction(actionDataSet, ActionContext.of(serverPlayer));
  }

  /** Uses the first audience member with at least one matching action as the initiator. */
  default void executeRandomAction(ActionDataSet actionDataSet, ActionContext actionContext) {
    if (actionDataSet == null || actionDataSet.isEmpty()) {
      return;
    }

    ActionEventType actionEventType = actionContext.eventType();
    for (ServerPlayer serverPlayer : this.getInitiatorCandidates(actionContext)) {
      List<ActionDataEntry> candidates = new ArrayList<>();
      for (ActionDataEntry actionDataEntry : actionDataSet.getEntries()) {
        if (!actionEventType.allowsActionDataType(actionDataEntry.actionDataType())) {
          log.warn(
              "Ignoring {} action of {}, which is not allowed for {}.",
              actionDataEntry.actionDataType(),
              this.getEntity(),
              actionEventType);
          continue;
        }

        if (this.validateActionData(actionDataEntry, serverPlayer)) {
          candidates.add(actionDataEntry);
        }
      }

      if (candidates.isEmpty()) {
        continue;
      }

      this.executeAction(
          candidates.get(this.getMob().getRandom().nextInt(candidates.size())),
          actionContext.withInitiator(serverPlayer));
      return;
    }
  }

  private List<ServerPlayer> getInitiatorCandidates(ActionContext actionContext) {
    if (!actionContext.hasAudience()) {
      return Collections.singletonList(actionContext.initiator());
    }

    if (!actionContext.hasInitiator()) {
      return actionContext.audience();
    }

    List<ServerPlayer> candidates = new ArrayList<>();
    candidates.add(actionContext.initiator());
    for (ServerPlayer serverPlayer : actionContext.audience()) {
      if (serverPlayer != actionContext.initiator()) {
        candidates.add(serverPlayer);
      }
    }
    return candidates;
  }

  private boolean validateActionData(ActionDataEntry actionDataEntry, ServerPlayer serverPlayer) {
    if (serverPlayer != null) {
      return ActionValidator.validateActionData(
          actionDataEntry, serverPlayer, this.getLivingEntity());
    }

    return ActionValidator.validateActionDataWithoutPlayer(actionDataEntry, this.getLivingEntity());
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
    Vec3 entityPosition = entity.position();
    Vec3 targetCenter = Vec3.atCenterOf(target);
    Vec3 delta = targetCenter.subtract(entityPosition);
    double horizontalDistance = delta.horizontalDistance();
    entity.setXRot(
        Mth.wrapDegrees((float) (-(Mth.atan2(delta.y, horizontalDistance) * (180D / Math.PI)))));
    entity.setYBodyRot(
        Mth.wrapDegrees((float) (Mth.atan2(delta.z, delta.x) * (180D / Math.PI)) - 90.0F));
    entity.setYHeadRot(entity.getYHeadRot());
  }

  default void executeActions(ActionDataSet actionDataSet, ServerPlayer serverPlayer) {
    this.executeActions(actionDataSet, ActionContext.of(serverPlayer));
  }

  default void executeActions(ActionDataSet actionDataSet, ActionContext actionContext) {
    if (actionDataSet == null || actionDataSet.isEmpty()) {
      return;
    }

    ServerPlayer serverPlayer = actionContext.initiator();
    boolean anyRegularFired = false;
    ActionDataEntry closeDialogAction = null;
    boolean hasScreenAction = false;
    for (ActionDataEntry actionDataEntry : actionDataSet.getEntries()) {
      if (hasFallbackCondition(actionDataEntry)) {
        continue;
      }

      if (!this.validateActionData(actionDataEntry, serverPlayer)) {
        continue;
      }
      anyRegularFired = true;

      ActionDataType actionType = actionDataEntry.actionDataType();

      if (actionType == ActionDataType.CLOSE_DIALOG) {
        if (closeDialogAction == null) {
          closeDialogAction = actionDataEntry;
        } else {
          log.warn("Multiple close dialog actions found in action data set {}!", actionDataSet);
        }
        continue;
      }

      if (actionType == ActionDataType.OPEN_DEFAULT_DIALOG
          || actionType == ActionDataType.OPEN_NAMED_DIALOG
          || actionType == ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL
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
            || ((actionType == ActionDataType.OPEN_NAMED_DIALOG
                    || actionType == ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL)
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

      this.executeAction(actionDataEntry, actionContext);
    }

    if (closeDialogAction != null && !hasScreenAction) {
      this.executeAction(closeDialogAction, actionContext);
    }

    if (!anyRegularFired) {
      ActionDataEntry fallbackCloseDialogAction = null;
      boolean hasFallbackScreenAction = false;
      for (ActionDataEntry actionDataEntry : actionDataSet.getEntries()) {
        if (!hasFallbackCondition(actionDataEntry)) {
          continue;
        }

        if (!this.validateActionData(actionDataEntry, serverPlayer)) {
          continue;
        }

        ActionDataType actionType = actionDataEntry.actionDataType();

        if (actionType == ActionDataType.CLOSE_DIALOG) {
          if (fallbackCloseDialogAction == null) {
            fallbackCloseDialogAction = actionDataEntry;
          }
          continue;
        }

        if (actionType == ActionDataType.OPEN_DEFAULT_DIALOG
            || actionType == ActionDataType.OPEN_NAMED_DIALOG
            || actionType == ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL
            || actionType == ActionDataType.OPEN_TRADING_SCREEN) {
          if (hasFallbackScreenAction) {
            continue;
          }
          hasFallbackScreenAction = true;
        }

        this.executeAction(actionDataEntry, actionContext);
      }

      if (fallbackCloseDialogAction != null && !hasFallbackScreenAction) {
        this.executeAction(fallbackCloseDialogAction, actionContext);
      }
    }
  }

  default void executeAction(ActionDataEntry actionDataEntry, ServerPlayer serverPlayer) {
    this.executeAction(actionDataEntry, ActionContext.of(serverPlayer));
  }

  default void executeAction(ActionDataEntry actionDataEntry, ActionContext actionContext) {
    if (actionDataEntry == null || !actionDataEntry.isValidAndNotEmpty()) {
      return;
    }

    ServerPlayer serverPlayer = actionContext.initiator();
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
      case OPEN_NAMED_DIALOG_CONDITIONAL:
        if (serverPlayer != null) {
          DialogActionExecutor.openNamedDialogConditional(
              actionDataEntry, serverPlayer, this.getEasyNPCDialogData());
        } else {
          log.warn(
              "Skipping OPEN_NAMED_DIALOG_CONDITIONAL action because no ServerPlayer is available");
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
          ScoreboardActionExecutor.execute(actionDataEntry, serverPlayer, this.getLivingEntity());
        } else {
          log.warn("Skipping SCOREBOARD action because no ServerPlayer is available");
        }
        break;
      case NPC_STATE:
        StateActionExecutor.execute(actionDataEntry, this, actionContext);
        break;
      case SET_POSE:
        if (!PoseActionExecutor.setPose(actionDataEntry, this)) {
          log.warn("Unable to set pose {} for {}", actionDataEntry.poseId(), this.getEntity());
        }
        break;
      case RESET_POSE:
        PoseActionExecutor.resetPose(this);
        break;
      case PLAY_ANIMATION:
        if (!ModelAnimationActionExecutor.play(actionDataEntry, this)) {
          log.warn("Unable to play animation for {}", this.getEntity());
        }
        break;
      case STOP_ANIMATION:
        if (!ModelAnimationActionExecutor.stop(actionDataEntry, this)) {
          log.warn("Unable to stop animation for {}", this.getEntity());
        }
        break;
      case RESTART_ANIMATION:
        if (!ModelAnimationActionExecutor.restart(this)) {
          log.warn("Unable to restart animation for {}", this.getEntity());
        }
        break;
      case MESSAGE:
        MessageActionExecutor.execute(actionDataEntry, this, actionContext);
        break;
      case CUSTOM:
        CustomActionDispatcher.execute(actionDataEntry, this, actionContext);
        break;
      default:
        log.warn(
            "Unknown action type {} for action {}",
            actionDataEntry.actionDataType(),
            actionDataEntry);
        break;
    }

    if (serverPlayer != null) {
      ConditionManager.recordExecutions(
          actionDataEntry.conditionDataSet().getConditions(),
          serverPlayer,
          ExecutionId.action(this.getEntity(), actionDataEntry.id()));
    }

    EasyNPCEventRegistry.fireActionExecuted(this, actionDataEntry, actionContext);
  }
}
