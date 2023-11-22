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

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.commands.CommandManager;
import de.markusbordihn.easynpc.data.action.ActionData;
import java.util.UUID;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.Entity;

public interface EasyNPCEntityAction extends EasyNPCEntityInterface {

  private static boolean validateActionData(ActionData actionData, ServerPlayer serverPlayer) {
    return actionData != null
        && serverPlayer != null
        && actionData.isValidAndNotEmpty()
        && !serverPlayer.getLevel().isClientSide();
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
        this.openTradingScreen(serverPlayer);
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
    if (dialogLabel != null && !dialogLabel.isEmpty() && this.getEntity().hasDialog(dialogLabel)) {
      UUID dialogId = this.getEntity().getDialogId(dialogLabel);
      EasyNPCEntityMenu.openDialogMenu(serverPlayer, this.getEntity(), dialogId, 0);
    } else {
      log.error("Unknown dialog label {} for action {}", dialogLabel, actionData);
    }
  }

  default void executePlayerCommand(ActionData actionData, ServerPlayer serverPlayer) {
    if (!validateActionData(actionData, serverPlayer)) {
      return;
    }
    int userPermissionLevel = actionData.getPermissionLevel();
    if (userPermissionLevel > this.getEntity().getActionPermissionLevel()) {
      log.warn(
          "User permission level {} is lower than action permission level {} for action {}",
          this.getEntity().getActionPermissionLevel(),
          userPermissionLevel,
          actionData);
      userPermissionLevel = this.getEntity().getActionPermissionLevel();
    }

    // Execute action as user with define permission level (default: 1).
    log.debug(
        "Try to execute action {} as user {} with user permission level {} of requested action permission level {} ...",
        actionData,
        serverPlayer,
        userPermissionLevel,
        actionData.getPermissionLevel());
    CommandManager.executePlayerCommand(
        actionData.getAction(this.getEntity(), serverPlayer),
        serverPlayer,
        userPermissionLevel,
        actionData.isDebugEnabled());
  }

  default void executeEntityCommand(ActionData actionData, ServerPlayer serverPlayer) {
    if (!validateActionData(actionData, serverPlayer)) {
      return;
    }
    int ownerPermissionLevel = this.getEntity().getActionPermissionLevel();
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
        this.getEntity().getActionPermissionLevel());
    CommandManager.executeEntityCommand(
        actionData.getAction(this.getEntity(), serverPlayer),
        this.getEntity(),
        ownerPermissionLevel,
        actionData.isDebugEnabled());
  }

  default InteractionResult openTradingScreen(ServerPlayer serverPlayer) {
    if (!this.getEntity().isClientSide()) {
      log.debug(
          "Open trading screen for {} with {} from {}",
          this.getEntity(),
          this.getEntity().getOffers(),
          serverPlayer);
      this.getEntity().setTradingPlayer(serverPlayer);
      this.getEntity()
          .openTradingScreen(
              serverPlayer,
              this.getEntity().getCustomName() != null
                  ? this.getEntity().getCustomName()
                  : Component.translatable(Constants.TEXT_PREFIX + "trading"),
              Entity.BASE_TICKS_REQUIRED_TO_FREEZE);
    }
    return InteractionResult.sidedSuccess(this.getEntity().isClientSide());
  }
}
