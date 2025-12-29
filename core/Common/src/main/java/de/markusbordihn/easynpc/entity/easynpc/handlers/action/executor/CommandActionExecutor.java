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

package de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.ActionValidator;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CommandActionExecutor {

  protected static final Logger log = LogManager.getLogger(CommandActionExecutor.class);

  private CommandActionExecutor() {}

  public static void executeAsPlayer(
      ActionDataEntry actionDataEntry,
      ServerPlayer serverPlayer,
      LivingEntity livingEntity,
      ActionEventDataCapable<?> actionEventData) {
    if (!ActionValidator.validateActionData(actionDataEntry, serverPlayer)) {
      return;
    }
    if (actionEventData == null) {
      log.error("No action event data found for action {}", actionDataEntry);
      return;
    }
    int userPermissionLevel = actionDataEntry.permissionLevel();
    if (userPermissionLevel > actionEventData.getActionPermissionLevel()) {
      log.warn(
          "User permission level {} is lower than action permission level {} for action {}",
          actionEventData.getActionPermissionLevel(),
          userPermissionLevel,
          actionDataEntry);
      userPermissionLevel = actionEventData.getActionPermissionLevel();
    }

    log.debug(
        "Try to execute action {} as user {} with user permission level {} of requested action permission level {} ...",
        actionDataEntry,
        serverPlayer,
        userPermissionLevel,
        actionDataEntry.permissionLevel());
    CommandExecutor.executePlayerCommand(
        actionDataEntry.getAction(livingEntity, serverPlayer),
        serverPlayer,
        userPermissionLevel,
        actionDataEntry.enableDebug());
  }

  public static void executeAsEntity(
      ActionDataEntry actionDataEntry,
      ServerPlayer serverPlayer,
      Entity entity,
      LivingEntity livingEntity,
      ActionEventDataCapable<?> actionEventData) {
    if (serverPlayer == null
        ? !ActionValidator.validateActionDataWithoutPlayer(actionDataEntry)
        : !ActionValidator.validateActionData(actionDataEntry, serverPlayer)) {
      return;
    }
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

    log.debug(
        "Try to execute action {} as entity {} with owner permission level {} of max. {} ...",
        actionDataEntry,
        entity,
        ownerPermissionLevel,
        actionEventData.getActionPermissionLevel());
    CommandExecutor.executeEntityCommand(
        actionDataEntry.getAction(livingEntity, serverPlayer),
        entity,
        ownerPermissionLevel,
        actionDataEntry.enableDebug());
  }
}
