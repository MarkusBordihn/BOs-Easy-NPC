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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.ActionValidator;
import java.util.UUID;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DialogActionExecutor {

  protected static final Logger log = LogManager.getLogger(DialogActionExecutor.class);

  private DialogActionExecutor() {}

  public static void openDefaultDialog(
      ActionDataEntry actionDataEntry, ServerPlayer serverPlayer, DialogDataCapable<?> dialogData) {
    if (!ActionValidator.validateActionData(actionDataEntry, serverPlayer)) {
      return;
    }
    if (dialogData != null) {
      dialogData.openDefaultDialog(serverPlayer);
    } else {
      log.error("No dialog data found for action {}", actionDataEntry);
      serverPlayer.closeContainer();
    }
  }

  public static void openNamedDialog(
      ActionDataEntry actionDataEntry, ServerPlayer serverPlayer, DialogDataCapable<?> dialogData) {
    if (!ActionValidator.validateActionData(actionDataEntry, serverPlayer)) {
      return;
    }

    DialogDataCapable<?> targetDialogData;
    if (actionDataEntry.targetUUID() == null) {
      targetDialogData = dialogData;
    } else {
      EasyNPC<?> targetNpc =
          findEasyNPCByUuid(serverPlayer.serverLevel(), actionDataEntry.targetUUID());
      if (targetNpc == null) {
        log.error("Target NPC with UUID {} not found", actionDataEntry.targetUUID());
        serverPlayer.closeContainer();
        return;
      }
      targetDialogData = targetNpc.getEasyNPCDialogData();
      if (targetDialogData == null) {
        log.error("No dialog data found for NPC {}", actionDataEntry.targetUUID());
        serverPlayer.closeContainer();
        return;
      }
    }

    String dialogLabel = actionDataEntry.command();
    if (ActionValidator.validateNamedDialog(targetDialogData, dialogLabel)) {
      targetDialogData.openDialog(serverPlayer, targetDialogData.getDialogId(dialogLabel));
    } else {
      log.error("Unknown dialog label {} for action {}", dialogLabel, actionDataEntry);
      serverPlayer.closeContainer();
    }
  }

  private static EasyNPC<?> findEasyNPCByUuid(ServerLevel level, UUID uuid) {
    Entity entity = level.getEntity(uuid);
    return entity instanceof EasyNPC<?> easyNPC ? easyNPC : null;
  }
}
