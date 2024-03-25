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

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.ParseResults;
import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.ActionGroup;
import de.markusbordihn.easynpc.data.action.ActionManager;
import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySelector;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.player.Player;

public interface ActionHandler<T extends PathfinderMob> extends EasyNPC<T> {

  private static boolean validateActionData(ActionData actionData, ServerPlayer serverPlayer) {
    return actionData != null
        && serverPlayer != null
        && actionData.isValidAndNotEmpty()
        && !serverPlayer.getLevel().isClientSide();
  }

  static void executeEntityCommand(
      String command, Entity entity, int permissionLevel, boolean debug) {
    MinecraftServer minecraftServer = entity.getServer();
    if (minecraftServer == null) {
      log.error("No Minecraft server found for entity {}", entity);
      return;
    }
    if (command.startsWith("/")) {
      command = command.substring(1);
    }
    log.debug("Execute Entity {} Command: \"{}\" with permission level {}", entity, command,
        permissionLevel);
    Commands commands = minecraftServer.getCommands();
    CommandSourceStack commandSourceStack = minecraftServer.createCommandSourceStack()
        .withEntity(entity).withPosition(entity.position()).withRotation(entity.getRotationVector())
        .withPermission(permissionLevel);
    CommandDispatcher<CommandSourceStack> commandDispatcher = commands.getDispatcher();
    ParseResults<CommandSourceStack> parseResults = commandDispatcher.parse(command,
        debug ? commandSourceStack : commandSourceStack.withSuppressedOutput());
    commands.performCommand(parseResults, command);
  }

  static void executePlayerCommand(
      String command, ServerPlayer serverPlayer, int permissionLevel, boolean debug) {
    MinecraftServer minecraftServer = serverPlayer.getServer();
    if (minecraftServer == null) {
      log.error("No Minecraft server found for player {}", serverPlayer);
      return;
    }
    if (command.startsWith("/")) {
      command = command.substring(1);
    }
    log.debug("Execute Player {} Command: \"{}\" with permission level {}", serverPlayer, command,
        permissionLevel);
    Commands commands = minecraftServer.getCommands();
    CommandSourceStack commandSourceStack =
        minecraftServer.createCommandSourceStack().withEntity(serverPlayer)
            .withPosition(serverPlayer.position()).withRotation(serverPlayer.getRotationVector())
            .withPermission(permissionLevel).withLevel(serverPlayer.getLevel());
    CommandDispatcher<CommandSourceStack> commandDispatcher = commands.getDispatcher();
    ParseResults<CommandSourceStack> parseResults = commandDispatcher.parse(command,
        debug ? commandSourceStack : commandSourceStack.withSuppressedOutput());
    commands.performCommand(parseResults, command);
  }

  default List<? extends Player> getPlayersInRange(Double range) {
    Entity entity = this.getEntity();
    return this.getLevel().players().stream()
        .filter(EntitySelector.NO_SPECTATORS)
        .filter(targetPlayers -> entity.closerThan(targetPlayers, range))
        .toList();
  }

  default void checkTradingActions() {
    this.getProfiler().push("npcCheckTradingActions");

    TradingData<?> tradingData = this.getEasyNPCTradingData();
    TickerData<?> tickerData = this.getEasyNPCTickerData();
    if (tradingData == null || tickerData == null) {
      return;
    }

    if ((tradingData.getTradingType() == TradingType.BASIC
        || tradingData.getTradingType() == TradingType.ADVANCED)
        && tradingData.getTradingResetsEveryMin() > 0
        && tickerData.checkAndIncreaseTicker(
        TickerType.TRADING_RESET, tradingData.getTradingResetsEveryMin())) {
      tradingData.resetTradingOffers();
      tickerData.resetTicker(TickerType.TRADING_RESET);
    }

    this.getProfiler().pop();
  }

  default void checkDistanceActions() {
    this.getProfiler().push("npcCheckDistanceActions");

    Mob mob = this.getMob();
    ActionEventData<?> actionEventData = this.getEasyNPCActionEventData();
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
        ActionData actionData = actionEventData.getActionEvent(ActionEventType.ON_DISTANCE_NEAR);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer
              && !ActionManager.containsPlayer(mob, ActionGroup.DISTANCE_NEAR, serverPlayer)) {
            this.executeAction(actionData, serverPlayer);
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
        ActionData actionData = actionEventData.getActionEvent(ActionEventType.ON_DISTANCE_CLOSE);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer
              && !ActionManager.containsPlayer(mob, ActionGroup.DISTANCE_CLOSE, serverPlayer)) {
            this.executeAction(actionData, serverPlayer);
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
        ActionData actionData =
            actionEventData.getActionEvent(ActionEventType.ON_DISTANCE_VERY_CLOSE);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer
              && !ActionManager.containsPlayer(
              mob, ActionGroup.DISTANCE_VERY_CLOSE, serverPlayer)) {
            this.executeAction(actionData, serverPlayer);
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
        ActionData actionData = actionEventData.getActionEvent(ActionEventType.ON_DISTANCE_TOUCH);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer
              && !ActionManager.containsPlayer(mob, ActionGroup.DISTANCE_TOUCH, serverPlayer)) {
            this.executeAction(actionData, serverPlayer);
            ActionManager.addPlayer(mob, ActionGroup.DISTANCE_TOUCH, serverPlayer);
          }
        }
      }
    }

    this.getProfiler().pop();
  }

  default void executeActions(Set<ActionData> actionDataSet, ServerPlayer serverPlayer) {
    if (actionDataSet == null || actionDataSet.isEmpty()) {
      return;
    }
    for (ActionData actionData : actionDataSet) {
      this.executeAction(actionData, serverPlayer);
    }
  }

  default void executeAction(ActionData actionData, DamageSource damageSource) {
    Entity entity = damageSource.getEntity();
    if (entity instanceof ServerPlayer serverPlayer) {
      this.executeAction(actionData, serverPlayer);
    }
  }

  default void executeAction(ActionData actionData, ServerPlayer serverPlayer) {
    if (!validateActionData(actionData, serverPlayer)) {
      return;
    }
    switch (actionData.getType()) {
      case NONE:
        break;
      case COMMAND:
        if (actionData.shouldExecuteAsUser()) {
          this.executePlayerCommand(actionData, serverPlayer);
        } else {
          this.executeEntityCommand(actionData, serverPlayer);
        }
        break;
      case OPEN_NAMED_DIALOG:
        this.openNamedDialog(actionData, serverPlayer);
        break;
      case OPEN_TRADING_SCREEN:
        TradingData<?> tradingData = this.getEasyNPCTradingData();
        if (tradingData != null) {
          tradingData.openTradingScreen(serverPlayer);
        } else {
          log.error("No trading data found for action {}", actionData);
        }
        break;
      default:
        log.warn("Unknown action type {} for action {}", actionData.getType(), actionData);
        break;
    }
  }

  default void openNamedDialog(ActionData actionData, ServerPlayer serverPlayer) {
    if (!validateActionData(actionData, serverPlayer)) {
      return;
    }
    String dialogLabel = actionData.getCommand();
    DialogData<?> dialogData = this.getEasyNPCDialogData();
    if (dialogLabel != null
        && !dialogLabel.isEmpty()
        && dialogData != null
        && dialogData.hasDialog(dialogLabel)) {
      UUID dialogId = dialogData.getDialogId(dialogLabel);
      dialogData.openDialogMenu(serverPlayer, this, dialogId, 0);
    } else {
      log.error("Unknown dialog label {} for action {}", dialogLabel, actionData);
    }
  }

  default void executePlayerCommand(ActionData actionData, ServerPlayer serverPlayer) {
    if (!validateActionData(actionData, serverPlayer)) {
      return;
    }
    ActionEventData<?> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData == null) {
      log.error("No action event data found for action {}", actionData);
      return;
    }
    int userPermissionLevel = actionData.getPermissionLevel();
    if (userPermissionLevel > actionEventData.getActionPermissionLevel()) {
      log.warn(
          "User permission level {} is lower than action permission level {} for action {}",
          actionEventData.getActionPermissionLevel(),
          userPermissionLevel,
          actionData);
      userPermissionLevel = actionEventData.getActionPermissionLevel();
    }

    // Execute action as user with define permission level (default: 1).
    log.debug(
        "Try to execute action {} as user {} with user permission level {} of requested action permission level {} ...",
        actionData,
        serverPlayer,
        userPermissionLevel,
        actionData.getPermissionLevel());
    executePlayerCommand(
        actionData.getAction(this.getLivingEntity(), serverPlayer),
        serverPlayer,
        userPermissionLevel,
        actionData.isDebugEnabled());
  }

  default void executeEntityCommand(ActionData actionData, ServerPlayer serverPlayer) {
    if (!validateActionData(actionData, serverPlayer)) {
      return;
    }
    ActionEventData<?> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData == null) {
      log.error("No action event data found for action {}", actionData);
      return;
    }
    int ownerPermissionLevel = actionEventData.getActionPermissionLevel();
    if (ownerPermissionLevel > 3) {
      ownerPermissionLevel = 3;
    } else if (ownerPermissionLevel <= 0) {
      ownerPermissionLevel = 1;
    }

    // Execute action as NPC entity with owner permission level.
    log.debug(
        "Try to execute action {} as entity {} with owner permission level {} of max. {} ...",
        actionData,
        this.getEntity(),
        ownerPermissionLevel,
        actionEventData.getActionPermissionLevel());
    executeEntityCommand(
        actionData.getAction(this.getLivingEntity(), serverPlayer),
        this.getEntity(),
        ownerPermissionLevel,
        actionData.isDebugEnabled());
  }
}
