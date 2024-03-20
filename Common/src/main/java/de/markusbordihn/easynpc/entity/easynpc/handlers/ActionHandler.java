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

import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import java.util.Set;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;

public interface ActionHandler<T extends LivingEntity> extends EasyNPC<T> {
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
    log.debug(
        "Execute Entity {} Command: \"{}\" with permission level {}",
        entity,
        command,
        permissionLevel);
    Commands commands = minecraftServer.getCommands();
    CommandSourceStack commandSourceStack =
        minecraftServer
            .createCommandSourceStack()
            .withEntity(entity)
            .withPosition(entity.position())
            .withRotation(entity.getRotationVector())
            .withPermission(permissionLevel);
    commands.performCommand(
        debug ? commandSourceStack : commandSourceStack.withSuppressedOutput(), command);
  }

  static void executePlayerCommand(
      String command, ServerPlayer serverPlayer, int permissionLevel, boolean debug) {
    MinecraftServer minecraftServer = serverPlayer.getServer();
    if (minecraftServer == null) {
      log.error("No Minecraft server found for player {}", serverPlayer);
      return;
    }
    log.debug(
        "Execute Player {} Command: \"{}\" with permission level {}",
        serverPlayer,
        command,
        permissionLevel);
    Commands commands = minecraftServer.getCommands();
    CommandSourceStack commandSourceStack =
        minecraftServer
            .createCommandSourceStack()
            .withEntity(serverPlayer)
            .withPosition(serverPlayer.position())
            .withRotation(serverPlayer.getRotationVector())
            .withPermission(permissionLevel)
            .withLevel(serverPlayer.getLevel());
    commands.performCommand(
        debug ? commandSourceStack : commandSourceStack.withSuppressedOutput(), command);
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
