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
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
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

public interface ActionHandler<E extends PathfinderMob> extends EasyNPC<E> {

  private static boolean validateActionDataEntry(ActionDataEntry actionDataEntry,
      ServerPlayer serverPlayer) {
    return actionDataEntry != null
        && serverPlayer != null
        && actionDataEntry.isValidAndNotEmpty()
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

    TradingData<E> tradingData = this.getEasyNPCTradingData();
    TickerData<E> tickerData = this.getEasyNPCTickerData();
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
    ActionEventData<E> actionEventData = this.getEasyNPCActionEventData();
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

  default void executeActions(Set<ActionDataEntry> actionDataEntrySet,
      ServerPlayer serverPlayer) {
    if (actionDataEntrySet == null || actionDataEntrySet.isEmpty()) {
      return;
    }
    for (ActionDataEntry actionDataEntry : actionDataEntrySet) {
      this.executeAction(actionDataEntry, serverPlayer);
    }
  }

  default void executeAction(ActionDataEntry actionDataEntry, DamageSource damageSource) {
    Entity entity = damageSource.getEntity();
    if (entity instanceof ServerPlayer serverPlayer) {
      this.executeAction(actionDataEntry, serverPlayer);
    }
  }

  default void executeAction(ActionDataEntry actionDataEntry, ServerPlayer serverPlayer) {
    if (!validateActionDataEntry(actionDataEntry, serverPlayer)) {
      return;
    }
    switch (actionDataEntry.getType()) {
      case NONE:
        break;
      case COMMAND:
        if (actionDataEntry.shouldExecuteAsUser()) {
          this.executePlayerCommand(actionDataEntry, serverPlayer);
        } else {
          this.executeEntityCommand(actionDataEntry, serverPlayer);
        }
        break;
      case OPEN_NAMED_DIALOG:
        this.openNamedDialog(actionDataEntry, serverPlayer);
        break;
      case OPEN_TRADING_SCREEN:
        TradingData<E> tradingData = this.getEasyNPCTradingData();
        if (tradingData != null) {
          tradingData.openTradingScreen(serverPlayer);
        } else {
          log.error("No trading data found for action {}", actionDataEntry);
        }
        break;
      default:
        log.warn(
            "Unknown action type {} for action {}", actionDataEntry.getType(),
            actionDataEntry);
        break;
    }
  }

  default void openNamedDialog(ActionDataEntry actionDataEntry,
      ServerPlayer serverPlayer) {
    if (!validateActionDataEntry(actionDataEntry, serverPlayer)) {
      return;
    }
    String dialogLabel = actionDataEntry.getCommand();
    DialogData<?> dialogData = this.getEasyNPCDialogData();
    if (dialogLabel != null
        && !dialogLabel.isEmpty()
        && dialogData != null
        && dialogData.hasDialog(dialogLabel)) {
      UUID dialogId = dialogData.getDialogId(dialogLabel);
      dialogData.openDialogMenu(serverPlayer, this, dialogId, 0);
    } else {
      log.error("Unknown dialog label {} for action {}", dialogLabel, actionDataEntry);
    }
  }

  default void executePlayerCommand(ActionDataEntry actionDataEntry,
      ServerPlayer serverPlayer) {
    if (!validateActionDataEntry(actionDataEntry, serverPlayer)) {
      return;
    }
    ActionEventData<E> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData == null) {
      log.error("No action event data found for action {}", actionDataEntry);
      return;
    }
    int userPermissionLevel = actionDataEntry.getPermissionLevel();
    if (userPermissionLevel > actionEventData.getActionPermissionLevel()) {
      log.warn(
          "User permission level {} is lower than action permission level {} for action {}",
          actionEventData.getActionPermissionLevel(),
          userPermissionLevel,
          actionDataEntry);
      userPermissionLevel = actionEventData.getActionPermissionLevel();
    }

    // Execute action as user with define permission level (default: 1).
    log.debug(
        "Try to execute action {} as user {} with user permission level {} of requested action permission level {} ...",
        actionDataEntry,
        serverPlayer,
        userPermissionLevel,
        actionDataEntry.getPermissionLevel());
    executePlayerCommand(
        actionDataEntry.getAction(this.getLivingEntity(), serverPlayer),
        serverPlayer,
        userPermissionLevel,
        actionDataEntry.isDebugEnabled());
  }

  default void executeEntityCommand(ActionDataEntry actionDataEntry,
      ServerPlayer serverPlayer) {
    if (!validateActionDataEntry(actionDataEntry, serverPlayer)) {
      return;
    }
    ActionEventData<E> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData == null) {
      log.error("No action event data found for action {}", actionDataEntry);
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
        actionDataEntry,
        this.getEntity(),
        ownerPermissionLevel,
        actionEventData.getActionPermissionLevel());
    executeEntityCommand(
        actionDataEntry.getAction(this.getLivingEntity(), serverPlayer),
        this.getEntity(),
        ownerPermissionLevel,
        actionDataEntry.isDebugEnabled());
  }
}
